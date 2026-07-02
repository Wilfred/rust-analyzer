# In-the-wild LSP request panics

Five error messages that rust-analyzer LSP deployments report as
`request handler panicked: ...`. Each issue has a self-contained doc with the
root-cause analysis, verified reproductions, and fix directions:

| Doc | Error message | Status |
|---|---|---|
| [01-bad-range-node-range.md](01-bad-range-node-range.md) | `Bad range: node range ...` | reproduced (exact message) |
| [02-failed-to-unify-type-owners.md](02-failed-to-unify-type-owners.md) | `failed to unify type owners` | reproduced (exact message) |
| [03-cant-project-out-of-type-error.md](03-cant-project-out-of-type-error.md) | `can't project out of {type error}` | reproduced (exact message) |
| [04-field-index-out-of-range.md](04-field-index-out-of-range.md) | `field FieldIndex(N) out of range (&'{region error} ...` | reproduced (exact message) |
| [05-salsa-cant-merge-cycle-heads.md](05-salsa-cant-merge-cycle-heads.md) | `Can't merge cycle heads ... variances_of_query` | analyzed; sibling salsa panic reproduced |

Common context for all five:

- The `request handler panicked: ...` prefix is added by
  `crates/rust-analyzer/src/handlers/dispatch.rs` (~line 349), which wraps
  every LSP request handler in `catch_unwind` and stringifies the panic
  payload into the LSP error response. Any panic below the handler surfaces
  with this prefix.
- `{type error}` and `'{region error}` fragments inside messages are `Debug`
  renderings from the vendored `ra-ap-rustc_type_ir` crate
  (`TyKind::Error` → `{type error}` in `src/ty_kind.rs`,
  `RegionKind::ReError` → `'{region error}` in `src/region_kind.rs`). Seeing
  them identifies the code as the new-solver type machinery.
- Issues 03 and 04 are hardness regressions from the `PlaceTy` rewrite
  (commit `bde303ad75`, authored 2025-12-03, merged 2026-06-01): code that
  previously recovered via `stdx::never!` + error types now hard-panics.

Reproduction tests live on this branch and are asserted with
`#[should_panic(expected = ...)]`, so the suite is green while the bugs
exist; after fixing an issue, flip its repro test into a plain regression
test (see the "Acceptance criteria" section of each doc).

```
cargo test -p hir-ty --lib tests::panic_repros
cargo test -p ide --lib lsp_error_repros
cargo test -p ide-assists --lib extract_variable::tests::repro_bad_range
VARIANCE_ROUNDS=10000 cargo test -p hir-ty --lib parallel_variances_cycle_heads -- --ignored   # soak, issue 05
```
