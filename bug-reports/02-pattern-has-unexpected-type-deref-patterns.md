# Bug: "pattern has unexpected type" logged for `deref_patterns` matches (match-check lacks a `DerefPattern` constructor)

## Summary

When the analyzed crate enables `#![feature(deref_patterns)]`, matching on a smart-pointer
scrutinee (anything implementing `Deref`, e.g. `Box<Option<T>>`, `String`, `Cow<'_, str>`)
with a pattern for the *pointee* makes inference record `PatAdjust::OverloadedDeref` pattern
adjustments. The exhaustiveness checker lowers **every** pattern adjustment to
`PatKind::Deref`, but its pattern-analysis layer only knows how to handle `Deref` patterns
whose type is a *built-in reference*. For a smart-pointer type it hits `stdx::never!`, logs
`ERROR pattern has unexpected type: pat: ..., ty: ...` to stderr (panics in debug builds), and
falls back to a wildcard constructor — degrading exhaustiveness checking.

rust-analyzer honors `#![feature(...)]` attributes of the crate it analyzes regardless of the
user's toolchain, so this fires for anyone analyzing a nightly project using `deref_patterns`
(e.g. parts of rust-lang/rust). It fires **once per match arm per analysis**, so a single such
match spams the log on every keystroke-triggered recomputation.

A companion **hard panic** (not a `never!`) exists in MIR lowering for the same feature — see
"Companion bug" below. Both should be fixed together.

## Exact symptom

```
ERROR pattern has unexpected type: pat: Pat { ty: MyBox<Option<i32>>, kind: Deref { subpattern:
Pat { ty: Option<{type error}>, kind: Variant { substs: [{type error}], enum_variant:
EnumVariantId(...), subpatterns: [FieldPat { field: Idx::<FieldData>(0), pattern: Pat { ty:
{type error}, kind: Wild } }] } } } }, ty: MyBox<Option<i32>>
```

(In debug builds: panic at `crates/hir-ty/src/diagnostics/match_check/pat_analysis.rs:178`.)

## Where the message is emitted

`crates/hir-ty/src/diagnostics/match_check/pat_analysis.rs:174-184` (in `lower_pat`):

```rust
PatKind::Deref { subpattern } => {
    ctor = match pat.ty.kind() {
        TyKind::Ref(..) => Ref,
        _ => {
            never!("pattern has unexpected type: pat: {:?}, ty: {:?}", pat, &pat.ty);
            Wildcard
        }
    };
    fields = singleton(self.lower_pat(subpattern));
    arity = 1;
}
```

A second instance of the same message exists at line 220 for `Leaf`/`Variant` patterns whose
type is not a tuple/ADT (not implicated by this reproduction, but the same fix PR could audit it).

## Root cause / mechanism

1. **Inference** (`crates/hir-ty/src/infer/pat.rs:425-449`): if the crate has
   `features.deref_patterns` and the scrutinee is an ADT implementing `Deref`
   (`should_peel_smart_pointer`, line 723), the expected type is peeled through
   `<T as Deref>::Target` and a `PatAdjustment { kind: PatAdjust::OverloadedDeref, source: T }`
   is pushed (line 593 in `check_deref_pattern`). The feature flag comes from the analyzed
   crate's own `#![feature]` attributes (`UnstableFeatures`,
   `crates/hir-def/src/unstable_features.rs`, reached via
   `resolver.top_level_def_map().features()` at `crates/hir-ty/src/infer.rs:1358`).

2. **Match-check lowering** (`crates/hir-ty/src/diagnostics/match_check.rs:114-128`,
   `lower_pattern`): all pattern adjustments — built-in refs *and* overloaded derefs alike —
   are folded into `PatKind::Deref` wrappers:

   ```rust
   let unadjusted_pat = self.lower_pattern_unadjusted(pat);
   self.infer.pat_adjustments.get(&pat).map(|it| &**it).unwrap_or_default().iter().rev().fold(
       unadjusted_pat,
       |subpattern, ref_ty| Pat {
           ty: ref_ty.source.as_ref(),
           kind: Box::new(PatKind::Deref { subpattern }),
       },
   )
   ```

   Note it discards the adjustment `kind` (`PatAdjust::BuiltinDeref` vs `OverloadedDeref`) and
   only keeps the source type.

3. **Pattern analysis** (`pat_analysis.rs:174`): `PatKind::Deref` is mapped to the `Ref`
   constructor only when `pat.ty` is `TyKind::Ref(..)`. For `MyBox<...>` / `String` / `Cow` the
   type is `TyKind::Adt`, so the `never!` fires and the constructor degrades to `Wildcard`.

4. The guards in `validate_match` (`crates/hir-ty/src/diagnostics/expr.rs:210-264`) do NOT
   prevent this: the adjusted pattern type equals the scrutinee type, and no type mismatch is
   recorded, so the match proceeds into lowering.

Upstream rustc models this with a dedicated `Constructor::DerefPattern` in
`rustc_pattern_analysis` (see `compiler/rustc_pattern_analysis/src/constructor.rs`, variant
`DerefPattern(Ty)`, and its handling in `usefulness.rs`/`lower` — added for the
`deref_patterns` feature). rust-analyzer vendors/mirrors this crate's design in
`crates/hir-ty/src/diagnostics/match_check/pat_analysis.rs` but has not implemented that
constructor. There is an acknowledging FIXME at `pat_analysis.rs:510`:
`// FIXME(deref_patterns): This could report an error comparable to the one in rustc.`

## Reproduction

### A. In-tree test (verified to panic in debug builds)

Append to `crates/ide-diagnostics/src/tests.rs`:

```rust
#[test]
fn repro_deref_patterns_smart_pointer() {
    check_diagnostics(
        r#"
//- minicore: deref, sized, option
#![feature(deref_patterns)]
use core::ops::Deref;
struct MyBox<T>(T);
impl<T> Deref for MyBox<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.0 }
}
fn f(x: MyBox<Option<i32>>) {
    match x {
        Some(_) => {}
        None => {}
    }
}
"#,
    );
}
```

`cargo test -p ide-diagnostics --lib repro_deref_patterns_smart_pointer` panics at
`pat_analysis.rs:178` with the exact message quoted above. minicore already provides the
required lang items (`deref`, `deref_target` at `crates/test-utils/src/minicore.rs:596-598`,
`deref_pure` at `:623`).

### B. Real-world shape (against a real sysroot)

```rust
#![feature(deref_patterns)]
#![allow(incomplete_features)]
pub fn f(x: Box<Option<i32>>) {
    match x {
        Some(_) => {}
        None => {}
    }
}
```

Run `RA_LOG=error rust-analyzer diagnostics <project>` (release-flavored build) → two `ERROR
pattern has unexpected type ...` lines (one per arm's lowering).

## Confirmed shape inventory

Scanning a corpus with a CLI build whose `never!`s log instead of panic produced this family
of shapes (all from the same `never!` at `pat_analysis.rs:178`); any `Deref`-implementing ADT
scrutinee works, and chains nest one `Deref` level per pattern adjustment:

```
ty: String,                 Deref { str Wild }                       // match String  { "" => }
ty: Box<str>,               Deref { str Wild }                       // match Box<str> { "" => }
ty: Box<&str>,              Deref { &'static str Wild }              // match Box<&str> { "" => }
ty: Box<Box<&str>>,         Deref { Box<&str> Deref { &str Wild } }  // two-level chain
ty: Box<String>,            Deref { String Deref { str Wild } }      // two-level chain
ty: Rc<str> / Rc<String>,   ...                                      // ditto
ty: Cow<'{region error}, str>, Deref { str Wild }                    // match Cow<str> { "" => }
ty: Vec<u8>,                Deref { [u8] Wild }                      // match Vec<u8> { b"" => }
ty: Box<Option<String>>,    Deref { Option<String> Variant {...} }   // match box { Some("")=> }
```

The `&'{region error}` regions are normal: writeback resolves unresolved region variables to
error regions, so any inferred reference type in these dumps carries them.

## Variant: outer type printed as bare `str` (primitive-shadowing ADT)

A reported real-world shape looks like:

```
pattern has unexpected type: pat: Pat { ty: str, kind: Deref { subpattern:
Pat { ty: &'{region error} str, kind: Deref { subpattern:
Pat { ty: &'{region error} str, kind: Wild } } } } }, ty: str
```

At first glance the outer `ty: str` looks impossible: both adjustment-push sites require the
peeled type to be `TyKind::Ref` (`infer/pat.rs:407-417`) or `TyKind::Adt`
(`should_peel_smart_pointer`, `infer/pat.rs:730`), and the primitive `str` is `TyKind::Str` —
so no adjustment can ever have the primitive `str` as its `source`, and the `Deref` wrapper
types in these dumps are exactly the adjustment sources.

The resolution: the `str` in the dump is **not the primitive** — it is an ADT *named* `str`
(shadowing a primitive type name is legal Rust), which Debug-prints identically to the
primitive (ADTs print bare names, cf. `String`). Confirmed reproduction:

```rust
#![feature(deref_patterns)]
#![allow(incomplete_features, unused, non_camel_case_types)]
use std::ops::Deref;
pub struct str;
impl Deref for str {
    type Target = &'static str;
    fn deref(&self) -> &&'static str { &"" }
}
pub fn f(x: str) { match x { "" => {}, _ => {} } }        // literal pattern
pub fn g(x: str) { match x { None => {}, _ => {} } }      // any peel-inducing pattern
```

logs exactly:

```
pattern has unexpected type: pat: Pat { ty: str, kind: Deref { subpattern:
Pat { ty: &'static str, kind: Wild } } }, ty: str
```

Notes on the remaining details of the reported shape:

- Inside such an impl, `type Target = &str` resolves `str` to the *shadowing ADT* and has an
  elided lifetime (a hard error in rustc — "missing lifetime specifier" — but rust-analyzer
  analyzes it anyway, lowering the elision to an error region). So the middle
  `&'{region error} str` layer is `&str-ADT` — print-indistinguishable from a reference to the
  primitive — and the Target is *recursive* (`str-ADT` derefs to `&str-ADT`), which lets
  peeling alternate BuiltinDeref/OverloadedDeref and produce multi-level `[str, &str, ...]`
  wrapper chains (bounded by `recursion_limit`, `infer/pat.rs:591`).
- Whether the second (ref) peel actually happens depends on normalization timing:
  `deref_pat_target` (`infer/pat.rs:1295`) returns `try_structurally_resolve_type(<T as
  Deref>::Target)`, which with the lazy next-solver can come back as an inference variable; the
  `TyKind::Ref` guard then fails and peeling stops after one adjustment. With a resolved
  (cache-warm) projection the guard sees the `Ref` and pushes the second adjustment. This
  makes the exact chain length environment/caching-dependent.
- While investigating this, another latent oddity surfaced:
  `InferenceResult::type_of_pat_with_adjust` (`infer.rs:1134-1139`) returns
  `adjustments.last().source` — the *innermost* peeled type — as the pattern's "adjusted
  type", but the scrutinee-side type is the *first* adjustment's source. `validate_match`'s
  `pat_ty == scrut_ty` guard only works today because of the `as_reference` special case and
  because `is_none_or` returns `true` for non-reference scrutinees; worth auditing when fixing
  this bug.

## Companion bug: hard panic in MIR lowering (crashes the process, even in release)

With the same feature, a deref pattern on `String`:

```rust
#![feature(deref_patterns)]
#![allow(incomplete_features)]
pub fn f(x: String) {
    match x {
        "hello" => {}
        _ => {}
    }
}
```

MIR lowering (used by borrowck-based diagnostics such as `need-mut`/`moved-out-of-ref`)
panics — this is a real `panic!`, not a `never!`, so it kills analysis in release builds too:

```
thread panicked at crates/hir-ty/src/mir.rs:1330:
deref projection of non-dereferenceable ty PlaceTy { ty: String, variant_id: None }
```

`crates/hir-ty/src/mir.rs:1326-1332` (`PlaceTy::projection_ty` for `ProjectionElem::Deref`)
only handles `builtin_deref` types; a `ProjectionElem::Deref` generated from an overloaded
deref pattern needs either a distinct projection (rustc lowers deref patterns through a
`deref_mut`/`deref` *call*, not a plain place deref) or a graceful bail-out in the MIR
lowering for bodies containing overloaded deref patterns.

## Fix guidance

1. Preserve the adjustment kind in `match_check::lower_pattern` (don't collapse
   `OverloadedDeref` into the same `PatKind::Deref` as builtin refs, or add a flag/type check),
   and add a `DerefPattern`-style constructor to
   `crates/hir-ty/src/diagnostics/match_check/pat_analysis.rs` mirroring
   `rustc_pattern_analysis`'s `Constructor::DerefPattern`: it behaves like a single-field
   unary constructor whose field type is `<T as Deref>::Target`, and it is neither covered by
   nor covers other constructors except wildcards. Follow rustc's implementation as the
   reference; rust-analyzer's `Constructor` enum here comes from the shared
   `rustc_pattern_analysis` crate (`rustc-ap` / vendored via `ra-ap-rustc_pattern_analysis`),
   so check which variants the vendored version already exposes — if the crate already has
   `DerefPattern`, the work is only in `lower_pat`/`ctor_arity`/`ctor_sub_tys`/display.
2. Minimal fallback if full support is deferred: in `lower_pattern_unadjusted`/`lower_pattern`,
   when an `OverloadedDeref` adjustment is present, push a `PatternError` (e.g.
   `PatternError::Unimplemented`) so `validate_match` bails out cleanly
   (`crates/hir-ty/src/diagnostics/expr.rs:257-263`, `has_lowering_errors` → return) instead of
   reaching the `never!`. This trades missing-match-arm coverage for silence, matching how
   other unimplemented pattern forms are handled (`match_check.rs:209-212`).
3. Fix the MIR panic independently: `ProjectionElem::Deref` on a non-`builtin_deref` type
   should produce `MirLowerError`/implementation-error instead of `panic!`
   (`crates/hir-ty/src/mir.rs:1330` and the corresponding place-lowering that emits the
   projection for deref patterns in `crates/hir-ty/src/mir/lower/pattern_matching.rs`).
4. Tests: the fixture from section A should, after the fix, produce **no** stderr error and
   correct exhaustiveness results (e.g. removing the `None` arm should report
   `missing match arm: None not covered`). Add a `String`-literal deref-pattern test to cover
   the MIR path (it currently aborts the test process in debug).

## How this was verified

- rust-analyzer commit `28ccb8be` (master, 2026-07), rustc 1.97.0-beta.6.
- Test A panics at `pat_analysis.rs:178` with the full pat/ty debug dump quoted above.
- CLI scan of a scratch project with `Box`/`Cow`/`String` deref patterns reproduced both the
  `never!` log lines and the `mir.rs:1330` process-killing panic.
- `core`/`alloc`/`std` do **not** enable `deref_patterns` crate-wide (only `#[unstable]`
  markers on `deref!`), so plain projects are unaffected; the trigger requires the analyzed
  crate (or a workspace member) to enable the feature.
