# Bug: "inference diagnostic in desugared expr" logged for diagnostics inside `format_args!` desugaring

## Summary

Inference diagnostics whose primary expression is a *synthetic* (desugared) expression cannot
be mapped back to source syntax. The conversion layer in `crates/hir/src/diagnostics.rs` hits a
`stdx::never!` soft assertion, logs `ERROR inference diagnostic in desugared expr` to stderr, and
silently drops the diagnostic. All confirmed real-world triggers are erroneous `format_args!`
invocations (i.e. `format!`, `println!`, etc.) — states an editor session passes through
constantly while a user is typing a format string, which is why the message appears repeatedly:
it is logged again on every diagnostics recomputation of the affected file.

Two bugs compound here:

1. The dropped diagnostic is a *real* user-facing error (rustc reports these), so the user gets
   no squiggle at all.
2. The `never!` treats an expected situation as an invariant violation, spamming stderr (and
   panicking in debug builds).

## Exact symptom

With default logging (`RA_LOG` unset; the default filter is `warn` — see
`crates/rust-analyzer/src/bin/main.rs:165`), stderr shows:

```
2026-07-02T09:11:18.607158568Z ERROR inference diagnostic in desugared expr
```

In debug builds (`cfg(debug_assertions)`), `never!` panics instead:
`internal error: entered unreachable code: inference diagnostic in desugared expr`.

## Where the message is emitted

`crates/hir/src/diagnostics.rs:1231-1239`:

```rust
fn expr_syntax(
    expr: ExprId,
    source_map: &ExpressionStoreSourceMap,
) -> Option<InFile<ExprOrPatPtr>> {
    source_map
        .expr_syntax(expr)
        .inspect_err(|_| stdx::never!("inference diagnostic in desugared expr"))
        .ok()
}
```

`source_map.expr_syntax(expr)` returns `Err(SyntheticSyntax)` for expressions allocated during
lowering via `alloc_expr_desugared` (no AST pointer). The helper is called from
`InferenceDiagnostic → AnyDiagnostic` conversion
(`AnyDiagnostic::inference_diagnostic`, same file, starting around line 806), via the closures
at lines 815-820:

```rust
let expr_syntax = |expr| Self::expr_syntax(expr, source_map);
let pat_syntax = |pat| Self::pat_syntax(pat, source_map);
let expr_or_pat_syntax = |id| match id {
    ExprOrPatId::ExprId(expr) => expr_syntax(expr),
    ExprOrPatId::PatId(pat) => pat_syntax(pat),
};
```

Sibling never!s exist for patterns and types (`inference diagnostic in desugared pattern` /
`... type`, lines 1241-1259), and `span_syntax` (line 1261) routes `SolverDiagnostic` and
`TypeMustBeKnown` spans through the same helpers.

## Root cause / mechanism

The `format_args!` desugaring (`crates/hir-def/src/expr_store/lower/format_args.rs`) allocates
many expressions with `alloc_expr_desugared` (no source pointer): the args tuple, per-argument
`Expr::Field` projections, `Argument::new_display(...)` calls, width/precision `Count`
expressions, etc. When the *user's* format string or arguments are wrong, inference produces
diagnostics **on those synthetic nodes**, and the conversion drops them.

Three confirmed triggers (verified by panic backtrace in a debug build, see "How verified"):

| Snippet | Diagnostic dropped | Conversion arm hit |
|---|---|---|
| `format!("{0} {1}", 1);` | `UnresolvedField` (field `1` on the synthetic args tuple, i.e. desugared `args.1`) | `crates/hir/src/diagnostics.rs:886` |
| `let width = S; format!("{:w$}", 3, w = width);` where `S` is not `usize` | `TypeMismatch` on the synthetic width expression | `crates/hir/src/diagnostics.rs:1070` |
| `format!("{:.*}", S, 1.0);` | `TypeMismatch` on the synthetic precision expression | `crates/hir/src/diagnostics.rs:1070` |

Important ABI caveat: rust-analyzer has **two** `format_args!` lowerings, selected by probing the
sysroot for the `format_count` lang item (`crates/hir-def/src/expr_store/lower/format_args.rs:98-102`):

```rust
let idx = if self.lang_items().FormatCount.is_none() {
    self.collect_format_args_after_1_93_0_impl(syntax_ptr, fmt)
} else {
    self.collect_format_args_before_1_93_0_impl(syntax_ptr, fmt)
};
```

- The missing-positional-arg trigger reproduces under **both** ABIs.
- The width/precision triggers only reproduce under the **post-1.93** ABI (real current sysroot).
  The test `minicore` still contains `#[lang = "format_count"]`
  (`crates/test-utils/src/minicore.rs:1432`), so plain `check_diagnostics` tests exercise the
  *pre*-1.93 path and will NOT reproduce the width/precision cases. Keep this in mind when
  writing regression tests: either extend minicore with the new-ABI items or test both paths.

Precedent that this is fixable: `UnresolvedIdent` *already* has a fallback for synthetic exprs —
`crates/hir/src/diagnostics.rs:918-928` maps a synthetic expr back to a range inside the format
string template via `source_map.format_args_implicit_capture(id)`:

```rust
&InferenceDiagnostic::UnresolvedIdent { id } => {
    let node = match id {
        ExprOrPatId::ExprId(id) => match source_map.expr_syntax(id) {
            Ok(syntax) => syntax.map(|it| (it, None)),
            Err(SyntheticSyntax) => source_map
                .format_args_implicit_capture(id)?
                .map(|(node, range)| (node.wrap_left(), Some(range))),
        },
        ...
```

The supporting infrastructure is `template_map` in the expression store
(`crates/hir-def/src/expr_store/lower/format_args.rs`, see `implicit_capture_to_source` and
`format_args_to_captures`), which records mappings from synthetic exprs to
`(template AstPtr, TextRange inside the string literal)`.

## Reproduction

### A. In-tree test (fastest; reproduces the missing-positional case)

Append to `crates/ide-diagnostics/src/tests.rs`:

```rust
#[test]
fn repro_fmt_missing_positional() {
    check_diagnostics(
        r#"
//- minicore: fmt
fn f() {
    format_args!("{0} {1}", 1);
}
"#,
    );
}
```

Run `cargo test -p ide-diagnostics --lib repro_fmt_missing_positional`. In a debug build the
test panics at `crates/hir/src/diagnostics.rs:1237` with
`internal error: entered unreachable code: inference diagnostic in desugared expr`; the
backtrace passes through the `UnresolvedField` arm at line 886.

### B. Against a real sysroot (reproduces all three)

Create a scratch cargo project:

```rust
// src/lib.rs
#![allow(unused)]
struct S;
pub fn a() { let s = format!("{0} {1}", 1); }
pub fn b() { let width = S; let s = format!("{:w$}", 3, w = width); }
pub fn c() { let s = format!("{:.*}", S, 1.0); }
```

Then run a debug-built rust-analyzer CLI over it:

```
RA_LOG=error RUST_BACKTRACE=1 rust-analyzer diagnostics /path/to/scratch
```

A release-flavored build (no `debug_assertions`) logs one/two `ERROR inference diagnostic in
desugared expr` lines per function; a debug build panics with a backtrace identifying the
conversion arm.

Note: the codebase MSRV is rustc 1.95 (`Cargo.toml` `rust-version`); build with a matching
toolchain (e.g. `rustup run beta cargo build -p rust-analyzer`).

## Fix guidance

Goals, in decreasing order of value:

1. **Stop dropping useful diagnostics.** For `format_args!`-originated synthetic exprs, map the
   diagnostic back to source the same way `UnresolvedIdent` does:
   - For placeholder-related nodes (missing positional args, width/precision expressions), the
     natural anchor is the placeholder's range inside the template. The lowering already records
     per-placeholder spans (see the `mappings` vector and `format_args_to_captures` in
     `collect_format_args`, `crates/hir-def/src/expr_store/lower/format_args.rs:51-108`); the
     synthetic `Expr::Field` / count expressions may need to be added to `template_map` (a new
     map alongside `implicit_capture_to_source`) at the points where
     `collect_format_args_after_1_93_0_impl` / `..._before_1_93_0_impl` allocate them.
   - rustc reports these as dedicated errors ("invalid reference to positional argument N",
     E0308 on the width/precision argument). Even mapping to the whole macro call (the
     `syntax_ptr` passed into `collect_format_args`) would be better than dropping.
2. **Make the fallback exhaustive rather than per-arm.** Instead of patching only
   `UnresolvedField` and `TypeMismatch`, consider changing `Self::expr_syntax` to accept the
   source map and return a best-effort pointer for synthetic exprs
   (template range → else macro call ptr → else `None` *without* `never!`). The `never!` should
   remain only for cases with no known desugaring provenance, or be demoted/removed entirely —
   diagnostics inside desugared code are an expected state, not an invariant violation.
3. **Tests.** Add `check_diagnostics` regression tests for all three snippets. For the
   width/precision cases the test must exercise the post-1.93 ABI: either add the new-ABI fmt
   items to minicore (and drop `#[lang = "format_count"]` in a variant) or gate the fixture
   appropriately. Expected behavior after the fix: an error diagnostic anchored inside/at the
   format string instead of a dropped diagnostic + stderr error.

## How this was verified

- rust-analyzer commit `28ccb8be` (master, 2026-07), built with rustc 1.97.0-beta.6.
- Debug-build test panic backtraces resolve to `crates/hir/src/diagnostics.rs:886`
  (UnresolvedField) and `:1070` (TypeMismatch) via `:815`/`:818` and the `never!` at `:1237`.
- A scan of rust-analyzer's own 887-file workspace (clean code) produced **zero** hits,
  confirming the trigger requires (transiently) erroneous code.

## Related

- `crates/hir/src/diagnostics.rs:1241-1259`: the same pattern exists for desugared *patterns*
  and *types*; any fix to the helper should cover those too.
- `SolverDiagnostic` spans (`span_syntax`, line 1261+) route through the same helper; trait
  errors with obligation-cause spans on synthetic exprs would be dropped the same way (not
  observed in the corpus, but structurally possible).
