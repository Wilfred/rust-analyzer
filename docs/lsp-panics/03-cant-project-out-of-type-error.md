# `request handler panicked: can't project out of {type error}`

## Symptom

Requests that compute diagnostics (or anything else that runs MIR
borrowck/const-eval) fail with:

```
request handler panicked: can't project out of {type error}
```

`{type error}` is the `Debug` rendering of `TyKind::Error` in
`ra-ap-rustc_type_ir` (`src/ty_kind.rs`: `write!(f, "{{type error}}")`).
Variants observed/reproducible with other base types: `can't project out of
Alias(...)`, `can't project out of ?0t`.

## Where the panic fires

`crates/hir-ty/src/mir.rs`, `PlaceTy::field_ty` (~1243-1280), fallthrough
arm at **line ~1277**:

```rust
fn field_ty(infcx, self_ty, variant: Option<VariantId>, f: FieldIndex) -> Ty<'db> {
    ...
    match self_ty.kind() {
        TyKind::Adt(..) if !adt_def.is_enum() => ...,
        TyKind::Closure(..) => ...,
        TyKind::Tuple(tys) => tys.get(f.0 as usize).cloned()
            .unwrap_or_else(|| panic!("field {f:?} out of range: {self_ty:?}")),  // see doc 04
        _ => panic!("can't project out of {self_ty:?}"),                          // THIS panic
    }
}
```

Introduced by the `PlaceTy` rewrite, commit `bde303ad75` ("Unify Field and
ClosureField ...", authored 2025-12-03, merged 2026-06-01). Before that
commit the equivalent code used `stdx::never!` + returned an error type, so
release builds logged and recovered.

## How it is reached (all passive surfaces)

`PlaceTy::projection_ty` → `projection_ty_core` → `handle_field` →
`field_ty`. Callers:

| Call site | Feature |
|---|---|
| `crates/hir-ty/src/mir/borrowck.rs:206` (`moved_out_of_ref`), `:441` (`place_case` ← `mutability_of_locals`) | `db.borrowck(..)` — run by `hir::DefWithBody::diagnostics` (`crates/hir/src/lib.rs:2103`) for **every body** when file diagnostics are computed |
| `crates/hir-ty/src/mir.rs:322` (`PlaceRef::ty`) | borrowck partial-move reporting |
| `crates/hir-ty/src/mir/eval.rs:747` | const-eval → hover, inlay hints |
| `crates/hir-ty/src/mir/pretty.rs:355` | MIR pretty printing (internal) |

Since `mutability_of_locals` visits every `Assign`/`&mut` in every body,
a single bad body in an open file panics the diagnostics request.

## Root cause

`projection_ty_core` (~line 1311) guards only against a base type that is
*already* an error — and only at the **top level** (`is_ty_error()` is
`matches!(kind, ty::Error(_))`, not deep):

```rust
if self.ty.is_ty_error() {
    return PlaceTy::from_ty(Ty::new_error(tcx, ErrorGuaranteed));
}
...
ProjectionElem::Field(f) => {
    PlaceTy::from_ty(handle_field(structurally_normalize(self.ty), self.variant_id, f))
}
```

When the base is a `TyKind::Alias`, the `structurally_normalize` callback
(built in `projection_ty`, ~1289-1302) runs **after** the guard and may
*produce* the bad type:

```rust
|ty| {
    if matches!(ty.kind(), TyKind::Alias(..)) {
        let mut ocx = ObligationCtxt::new(infcx);
        match ocx.structurally_normalize_ty(&ObligationCause::dummy(), env, ty) {
            Ok(it) => it,
            Err(_) => Ty::new_error(infcx.interner, ErrorGuaranteed),  // ← fed to field_ty
        }
    } else { ty }
}
```

Three outcomes reach the fallthrough panic (all verified):

1. **Normalization fails** (e.g. the alias' self type is an error:
   `<{error} as Tr>::A` passes the shallow `is_ty_error` guard, then no impl
   candidate matches the error self type) → `Ty::new_error` →
   `can't project out of {type error}`.
2. **Rigid alias** (no equality bound in the env): `structurally_normalize`
   legitimately returns the alias unchanged (see
   `crates/hir-ty/src/next_solver/structural_normalize.rs` — a rigid
   projection is a valid result) → `can't project out of Alias(..)`.
3. **Ambiguous normalization** (conflicting or cyclic equality bounds): the
   fresh inference variable created by `structurally_normalize_term` stays
   unconstrained and is returned → `can't project out of ?0t`.

Why MIR containing such projections gets built at all: the lowering bail
guard (`crates/hir-ty/src/mir/lower.rs:2390`) is

```rust
if infer.has_type_mismatches() || infer.is_erroneous() { /* bail */ }
```

but `is_erroneous` (`crates/hir-ty/src/infer.rs:1140`) is
`has_errors && type_of_expr.iter().count() == 0` — i.e. only bodies with
*zero* typed expressions count as erroneous — and inference does **not**
record a type mismatch when it fails to relate a tuple pattern against an
alias it cannot normalize. So tuple-pattern destructuring
(`crates/hir-ty/src/mir/lower/pattern_matching.rs` ~139-156) happily emits
`Field(FieldIndex(0..n))` projections against the pattern's own inferred
type, while the base local keeps the (alias) type from the signature.
Relevant detail: MIR **parameter locals keep the unnormalized signature
type** (e.g. `x_1: <T as Tr>::A`), while let-bound locals get normalized
types — so projections directly on parameters are the exposed case.

Also note the borrowck context differs from inference:
`crates/hir-ty/src/mir/borrowck.rs:144-146` builds a fresh
`InferCtxt` with `TypingMode::borrowck` and
`db.trait_environment(def.generic_def(db))`, so anything inference resolved
with body-local information is re-derived from scratch here.

## Verified reproductions

`cargo test -p hir-ty --lib tests::panic_repros`
(`crates/hir-ty/src/tests/panic_repros.rs`), exact messages asserted. All
fixtures are ordinary mid-edit states:

```rust
// can't project out of {type error} — unresolved name inside the alias
trait Tr { type A; }
fn f(x: <Missing as Tr>::A) {
    let (mut a, b) = x;
    a = 1;
}

// can't project out of Alias(..) — rigid alias, wrong (tuple) pattern
trait Tr { type A; }
fn f<T: Tr>(x: T::A) { let (mut a, b) = x; a = 1; }

// can't project out of ?0t — conflicting equality bounds (duplicate bound typo)
trait Tr { type A; }
fn f<T: Tr<A = (u32, u32)> + Tr<A = (u32, u32, u32)>>(x: T::A) {
    let (mut a, b, c) = x; a = 1;
}

// can't project out of ?0t — cyclic equality bound
trait Tr { type A; }
fn f<T: Tr<A = (T::A, u32)>>(x: T::A) { let (mut a, b) = x; a = 1; }
```

The panic fires inside `db.borrowck(def)`; the test harness
(`borrowck_all`) simply runs borrowck for every body of the fixture,
mirroring what diagnostics does.

## Fix directions

1. **Guard after normalization** in `projection_ty_core`
   (`crates/hir-ty/src/mir.rs` ~1352): compute
   `let base = structurally_normalize(self.ty);` once, and if
   `base.is_ty_error()` return `PlaceTy::from_ty(Ty::new_error(..))` before
   calling `handle_field` — mirroring the existing pre-normalization guard.
   This fixes outcome 1.
2. **Make `field_ty` total**: replace the fallthrough
   `panic!("can't project out of ...")` with
   `stdx::never!(...)` + `Ty::new_error(..)` (the pre-`bde303ad75`
   behavior) so rigid aliases (outcome 2) and infer vars (outcome 3)
   degrade instead of crashing. Borrowck results for such bodies are
   best-effort anyway.
3. Optionally strengthen the lowering bail: record a type mismatch when a
   pattern cannot be related to the scrutinee type (would prevent building
   the inconsistent MIR in the first place), and/or fix `is_erroneous`'s
   `count() == 0` condition, which looks like a bug on its own. Be careful:
   over-bailing regresses diagnostics coverage on partially-broken bodies.

1 + 2 are safe and local; the rewrite's upstream author (commit
`bde303ad75`) may want the panics kept in debug builds — `never!` gives
exactly that (panics under `debug_assertions`/`force` , logs otherwise).

## Acceptance criteria

- The four repro tests in `crates/hir-ty/src/tests/panic_repros.rs`
  (`tuple_pattern_on_error_self_alias`, `tuple_pattern_on_rigid_alias`,
  `tuple_pattern_conflicting_alias_bounds`,
  `tuple_pattern_cyclic_alias_bound`) no longer panic: flip their
  `#[should_panic]` into plain tests asserting `db.borrowck` returns
  (Ok or Err both acceptable — just no unwind).
- Diagnostics for these fixtures still produce sensible output (no new
  false positives from error-typed places — check `need-mut`/`unused-mut`
  behavior on the fixtures).

## Key files

- `crates/hir-ty/src/mir.rs`: `FieldIndex` (~151), `PlaceTy` (~1219),
  `field_ty` (~1243, panics at ~1276/~1277), `projection_ty` (~1283,
  normalize callback ~1289-1302), `projection_ty_core` (~1311, shallow
  guard ~1321)
- `crates/hir-ty/src/next_solver/structural_normalize.rs` (why rigid
  aliases and ambiguity behave as they do)
- `crates/hir-ty/src/mir/borrowck.rs:137-152` (borrowck env/typing mode),
  `:206`, `:441` (projection callers)
- `crates/hir-ty/src/mir/lower.rs:2390` + `crates/hir-ty/src/infer.rs:1102,
  1140` (weak bail guards)
- `crates/hir-ty/src/mir/lower/pattern_matching.rs:139-156` (tuple-pattern
  Field emission)
- `crates/hir/src/lib.rs:2103` (diagnostics → borrowck)
- `crates/hir-ty/src/tests/panic_repros.rs` (reproductions)
- Companion issue: [04-field-index-out-of-range.md](04-field-index-out-of-range.md)
  (the tuple arm of the same function; fix together)
