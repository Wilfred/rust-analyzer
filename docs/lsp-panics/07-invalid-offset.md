# `request handler panicked: invalid offset`

## Symptom

LSP requests or the diagnostics pipeline fail with:

```
request handler panicked: invalid offset
```

This is the **response-path sibling** of
[01-bad-range-node-range.md](01-bad-range-node-range.md): there, an
out-of-file range is applied to a syntax tree; here, an out-of-file offset
is converted into an LSP position on the way *out*.

## Where the panic fires

`lib/line-index/src/lib.rs:110-112`:

```rust
pub fn line_col(&self, offset: TextSize) -> LineCol {
    self.try_line_col(offset).expect("invalid offset")
}
```

`try_line_col` returns `None` when `offset > self.len` **or** when the
offset points into the middle of a multi-byte character.

The server-side funnel is `crates/rust-analyzer/src/lsp/to_proto.rs:42-51`:

```rust
pub(crate) fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.index.line_col(offset);            // panics
    match line_index.encoding {
        PositionEncoding::Utf8 => ...,
        PositionEncoding::Wide(enc) =>
            line_index.index.to_wide(enc, line_col).unwrap(),    // secondary panic point
    }
}
```

`to_proto::range` calls `position` twice; almost every LSP response
conversion goes through here.

## Where bad offsets come from (classified)

- **(b) Macro-span-resolved ranges — the weakest path.** `ide::Diagnostic`
  ranges for nodes in macro-expanded code are up-mapped via
  `sema.diagnostics_display_range*`
  (`crates/ide-diagnostics/src/lib.rs:223/470` →
  `crates/hir/src/semantics.rs:1599-1615` →
  `crates/hir-expand/src/files.rs:456` → `map_node_range_up_rooted` →
  **`crates/hir-expand/src/db.rs:126` `resolve_span`, which computes
  `range + anchor_offset` with no clamp to the file length** — the exact
  root cause of doc 01). The diagnostics publish pipeline
  (`crates/rust-analyzer/src/diagnostics.rs:353-370` `convert_diagnostic` →
  `to_proto::range` at line 358) converts these with the panicking
  `line_col` and runs **automatically on every edit**, unprompted. Bad
  spans (buggy/version-skewed proc-macro server, see doc 01's
  `deserialize_span_data_index_map`) or any up-map inflation land here.
- **(c) Ranges cached across revisions.** `to_proto.rs:628` serializes
  `resolve_range` into `InlayHintResolveData` (with a version stamp);
  `inlayHint/resolve` re-converts it later against the then-current
  line index.
- **CLI sinks with the panicking `line_col`**: SCIP
  (`crates/rust-analyzer/src/cli/scip.rs:362-374`), LSIF
  (`cli/lsif.rs:131/264`), diagnostics CLI (`cli/diagnostics.rs:103`).
  Symbol/occurrence ranges there are macro-up-mapped, so
  `rust-analyzer scip` on proc-macro-heavy code can crash deterministically
  given one bad span. Relevant to indexer deployments.
- **(e) Char-boundary risk** exists in principle (offset arithmetic on
  multibyte text) but no live producer was found; the known `end() -
  TextSize::of('\n')` in semantic tokens is ASCII-safe.
- **Not affected**: cargo/rustc flycheck diagnostics
  (`crates/rust-analyzer/src/diagnostics/flycheck_to_proto.rs:84-116`)
  build LSP positions directly from cargo's line/col JSON without touching
  `LineIndex` — stale build output produces wrong-but-in-bounds positions,
  never this panic.

`to_wide`/`to_utf8` themselves return `Option` and are only unwrapped at
`to_proto.rs:47`; the primary panic is `line_col`.

## Verified reproduction

`cargo test -p rust-analyzer --lib to_proto::tests::repro_invalid_offset_past_eof`
(`crates/rust-analyzer/src/lsp/to_proto.rs` test module), exact message
asserted: `to_proto::position` with an offset 2 bytes past EOF panics with
`invalid offset`. This pins the sink contract; the end-to-end producers are
the unclamped up-map paths above (shared root cause with doc 01, where the
request-path half is reproduced end-to-end via
`extract_variable::tests::repro_bad_range_unresolved_macro_paren`).

## Fix directions

1. **Fix the shared root cause: clamp in `resolve_span`**
   (`crates/hir-expand/src/db.rs:126`) — one change kills both this panic
   and doc 01's "Bad range" at their common source, and protects the
   diagnostics/SCIP/inlay ranges before they are cached. Pair with
   validating proc-macro spans (doc 01, fix 3).
2. **Make `to_proto::position` total** as a backstop: use `try_line_col`
   and clamp to the end-of-file position on `None`; replace the
   `to_wide(..).unwrap()` with the same fallback. A wrong-but-in-bounds
   position beats a dead request, and matches the existing clamping
   philosophy in `from_proto::offset`.
3. Switch the CLI sinks (scip/lsif/diagnostics CLI) to `try_line_col` with
   clamping if (2) is not adopted wholesale.
4. Optional defense-in-depth: validate `d.range` against
   `line_index.index.len()` in `convert_diagnostic`
   (`crates/rust-analyzer/src/diagnostics.rs:353`), since the publish
   pipeline is auto-triggered and less guarded than request handlers.

## Acceptance criteria

- `repro_invalid_offset_past_eof` inverted: `to_proto::position` past EOF
  returns the end-of-file position instead of panicking (if fix 2), or the
  test is retargeted to assert `resolve_span` output is always in-bounds
  (if fix 1 only).
- `rust-analyzer scip` / `lsif` cannot crash on a bad span (feed one via a
  mock proc-macro server or a unit test on `text_range_to_scip_range`).
- Doc 01's repros remain green/fixed in tandem — same root cause.

## Key files

- `lib/line-index/src/lib.rs:110-132` (`line_col`/`try_line_col`),
  `:147-178` (`to_wide`/`to_utf8`)
- `crates/rust-analyzer/src/lsp/to_proto.rs:42-57` (funnel), `:628`
  (inlay resolve data), test module (repro)
- `crates/rust-analyzer/src/diagnostics.rs:353-370` (publish sink)
- `crates/hir-expand/src/db.rs:126` (`resolve_span`, unclamped root cause)
- `crates/rust-analyzer/src/cli/scip.rs:362-374`, `cli/lsif.rs:131,264`,
  `cli/diagnostics.rs:103` (CLI sinks)
- `crates/rust-analyzer/src/diagnostics/flycheck_to_proto.rs:84-116`
  (not affected — for contrast)
