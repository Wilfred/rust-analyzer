# `request handler panicked: field FieldIndex(N) out of range (&'{region error} ...`

## Symptom

Requests that compute diagnostics (or const-eval-backed hover/inlay hints)
fail with an error like:

```
request handler panicked: field FieldIndex(99) out of range: (&'{region error} str, u32)
```

Decoding the message:

- `FieldIndex(N)` is the `Debug` of `crates/hir-ty/src/mir.rs`'s
  `pub struct FieldIndex(pub u32)` (~line 151) — the MIR field-projection
  index (used for tuple fields, named fields via `LocalFieldId`, and closure
  captures alike).
- The parenthesized tail is the `Debug` of the base **tuple type** being
  projected; `'{region error}` is `ra-ap-rustc_type_ir`'s rendering of
  `RegionKind::ReError` (`src/region_kind.rs`), i.e. a reference whose
  lifetime failed to resolve — cosmetic here, not causal.

## Where the panic fires

`crates/hir-ty/src/mir.rs`, `PlaceTy::field_ty`, tuple arm at **line ~1276**:

```rust
TyKind::Tuple(tys) => tys
    .get(f.0 as usize)
    .cloned()
    .unwrap_or_else(|| panic!("field {f:?} out of range: {self_ty:?}")),
```

Introduced by the `PlaceTy` rewrite, commit `bde303ad75` (authored
2025-12-03, merged 2026-06-01). The pre-rewrite equivalent was
`never!("Out of bound tuple field")` + return `Ty::new_error(..)` — logged
and recovered in release builds. This is a hardness regression, not a new
logic bug: the same inconsistent MIR existed before; it just didn't crash.

(If the message in a report uses `out of range (` instead of
`out of range: `, it is from a build carrying an intermediate revision of
the same rewrite; same code path.)

## How it is reached

Identical caller graph to
[03-cant-project-out-of-type-error.md](03-cant-project-out-of-type-error.md)
(same function, adjacent arm): `db.borrowck(..)` — which
`hir::DefWithBody::diagnostics` (`crates/hir/src/lib.rs:2103`) runs for
**every body** during file diagnostics — plus MIR eval
(`crates/hir-ty/src/mir/eval.rs:747`) for const-eval-backed features. No
user action beyond having the file open with diagnostics enabled.

## Root cause

An **out-of-bounds tuple field access is lowered to MIR anyway**:

1. For a tuple field expression `t.N`, MIR lowering
   (`crates/hir-ty/src/mir/lower.rs`, `push_field_projection` ~1401) emits
   `ProjectionElem::Field(FieldIndex(N))` — tuple indices are taken from the
   literal (`name.as_tuple_index()`) / inference's tuple-field records and
   are **not bounds-checked against the base tuple's arity**.
2. Inference does not mark `t.N`-out-of-bounds as a *type mismatch* — it
   produces an unresolved-field diagnostic, but
   `InferenceResult::has_type_mismatches`
   (`crates/hir-ty/src/infer.rs:1102`) stays false, and `is_erroneous`
   (`:1140`, `has_errors && type_of_expr.iter().count() == 0`) is
   practically never true for a real body. So the MIR-lowering bail at
   `crates/hir-ty/src/mir/lower.rs:2390` does not trigger.
3. Borrowck later recomputes the place's type via
   `PlaceTy::projection_ty_core` → `field_ty`, indexes the tuple with the
   stored `FieldIndex`, and panics.

So the trigger is as mundane as it gets: a typo or mid-edit state like
`t.1` on a 1-tuple, `t.99`, or a macro-generated tuple index that exceeds
the tuple's arity. A large `N` observed in the wild (e.g. 12345) is just a
large literal/generated index.

A second, subtler route to the same arm (not needed for the panic, but part
of the same inconsistency class): a `FieldIndex` recorded against one view
of the base type while borrowck's `structurally_normalize` resolves the
base to a *different* tuple — possible when the base is an associated-type
alias whose normalization result diverges between inference time and
borrowck's fresh `InferCtxt`/`TypingMode::borrowck` context
(`crates/hir-ty/src/mir/borrowck.rs:144-146`). The out-of-bounds-literal
route is the one that matters in practice and is what the repros pin.

## Verified reproductions

`cargo test -p hir-ty --lib tests::panic_repros`
(`crates/hir-ty/src/tests/panic_repros.rs`), exact messages asserted:

```rust
fn f() { let t = (1,); let x = t.1; }
// panicked at crates/hir-ty/src/mir.rs:1276:
// field FieldIndex(1) out of range: (i32,)

fn f(s: &'missing str) { let t = (s, 1u32); let x = t.99; }
// field FieldIndex(99) out of range: (&'{region error} str, u32)
```

The second fixture adds an undeclared lifetime so the message shape matches
the in-the-wild report (`(&'{region error} ...`). The tests run
`db.borrowck` over every body of the fixture, exactly like diagnostics.

## Fix directions

1. **Make the tuple arm total** (minimal, safe): the arm already uses
   `.get(..)`; replace the `unwrap_or_else(panic!)` with
   `stdx::never!(...)` + `Ty::new_error(infcx.interner, ErrorGuaranteed)`,
   restoring the pre-rewrite behavior. Do the same for the closure arm one
   line up (`tuple_fields()[f.0 as usize]` is an unchecked index that
   panics with a bare slice-index message) and the enum-variant path
   (`field_types(variant_id)[f.to_local_field_id()]` is an unchecked arena
   index).
2. **Don't lower out-of-bounds tuple fields**: in
   `crates/hir-ty/src/mir/lower.rs` (`push_field_projection` ~1401), when
   the field expression has no `field_resolution` **and** the tuple index
   exceeds the base tuple's arity, return a `MirLowerError` (there is
   already `MirLowerError::UnresolvedField`) instead of emitting the
   projection. This keeps borrowck's invariant ("indices are in bounds")
   true instead of weakening it.
3. Optionally, treat unresolved/out-of-bounds field accesses as recorded
   type mismatches in inference so the existing bail guard catches them —
   broadest fix, but check it doesn't regress diagnostics on
   partially-broken bodies (bailing MIR construction kills need-mut /
   moved-out-of-ref analysis for the whole body).

Recommended: 1 + 2 together. Fix jointly with
[03-cant-project-out-of-type-error.md](03-cant-project-out-of-type-error.md)
— same function, same hardness regression, one PR.

## Acceptance criteria

- `out_of_bounds_tuple_field_literal` and
  `out_of_bounds_tuple_field_region_error` in
  `crates/hir-ty/src/tests/panic_repros.rs` no longer panic: flip their
  `#[should_panic]` into plain tests asserting `db.borrowck` returns
  without unwinding.
- `fn f() { let t = (1,); let x = t.1; }` still produces the
  unresolved-field diagnostic, and mutability diagnostics for the rest of
  the body still work.
- No remaining unchecked indexing in `field_ty` (tuple, closure, and
  enum-variant arms).

## Key files

- `crates/hir-ty/src/mir.rs`: `FieldIndex` (~151), `field_ty` tuple arm
  (~1273-1276), closure arm (~1269-1271), variant arm (~1249-1259)
- `crates/hir-ty/src/mir/lower.rs`: `push_field_projection` (~1401), bail
  guard (~2390)
- `crates/hir-ty/src/infer.rs:1102` (`has_type_mismatches`), `:1140`
  (`is_erroneous` — note the suspicious `count() == 0`)
- `crates/hir-ty/src/mir/borrowck.rs` (caller), `crates/hir/src/lib.rs:2103`
  (diagnostics → borrowck)
- `crates/hir-ty/src/tests/panic_repros.rs` (reproductions)
- History: `git show bde303ad75` (the rewrite), and the pre-rewrite
  recoverable version of this arm (`never!("Out of bound tuple field")`) in
  `ProjectionElem::projected_ty`
