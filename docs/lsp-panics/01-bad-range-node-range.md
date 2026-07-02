# `request handler panicked: Bad range: node range ...`

## Symptom

LSP requests fail with an error response like:

```
request handler panicked: Bad range: node range 0..1234, range 1300..1310
```

The related message `Bad offset: range 0..N offset M` comes from the sibling
assert and has the same family of causes.

## Where the panic fires

rowan `SyntaxNode::covering_element`,
`rowan-0.15.18/src/cursor.rs:787-796` (a dependency; vendored source in the
cargo registry):

```rust
pub fn covering_element(&self, range: TextRange) -> SyntaxElement {
    let mut res: SyntaxElement = self.clone().into();
    loop {
        assert!(
            res.text_range().contains_range(range),
            "Bad range: node range {:?}, range {:?}",
            res.text_range(),
            range,
        );
        ...
```

`contains_range(other)` is `self.start() <= other.start() && other.end() <=
self.end()` (`text-size` crate), so the assert fires exactly when the queried
range's **end exceeds the tree** it is applied to (start underflow is
impossible for unsigned offsets). `token_at_offset` has the analogous
`Bad offset` assert.

An empty range at EOF (`len..len`) is **safe**: `0 <= len && len <= len`
holds, and descent just stops. Only ranges strictly past the tree length
panic.

## Why out-of-tree ranges occur

The LSP-boundary conversion is mostly safe:
`crates/rust-analyzer/src/lsp/from_proto.rs::offset()` (lines 25-52) **clamps
columns** to the line length and **errors** on out-of-range lines, and
`text_range()` rejects `end < start`. So a stale position converted against
the *current* file cannot directly produce an out-of-bounds range. Note the
FIXME in `offset()` about request retrying — clamping there papers over
version skew, it does not eliminate the class.

The panics come from paths that compute a range against one text/tree and
apply it to another:

### Path A (highest confidence): macro span up-mapping, no clamping

- `crates/hir-expand/src/db.rs:126` `resolve_span` computes
  `range + anchor_offset` with **no clamping to the anchor file's length**:

  ```rust
  fn resolve_span(db: &dyn ExpandDatabase, Span { range, anchor, ctx: _ }: Span) -> FileRange {
      // no check that the result fits in the file
  }
  ```

- `crates/hir-expand/src/lib.rs` `map_node_range_up_rooted` (~line 945) /
  `map_node_range_up` (~line 968) union `span.range` over all spans in a
  range (min start / max end), which can further inflate the result.
- The resulting `FileRange` is handed **directly** to `covering_element` on
  the real file's freshly parsed tree at
  `crates/hir-expand/src/files.rs:353` (`original_syntax_node_rooted`) and
  `crates/hir-expand/src/files.rs:538` (`original_ast_node_rooted`), with no
  `range.end() <= tree_len` guard. Both are gated to attribute
  macros/proc-macro attributes and are reached from completions
  (`ide-completion` trait_impl), codeLens resolve
  (`ide/src/annotations.rs` → `original_ast_node_rooted`), rename, and more.
- Span *input* is unvalidated:
  `crates/proc-macro-api/src/legacy_protocol/msg/flat.rs:69`
  `deserialize_span_data_index_map` reads raw
  `(file_id, ast_id, range.start, range.end, ctx)` u32 tuples from the
  proc-macro server into `TextRange::new(start, end)` with **no validation**.
  A buggy or version-skewed proc-macro server can inject spans whose ranges
  exceed the anchor item / file. (Down-mapping via
  `ExpansionSpanMap::ranges_with_span*` is safe by construction —
  `syntax-bridge` builds those offsets against the same text it builds the
  tree from, and `SpanMap::finish` asserts monotonicity.)

### Path B: cross-node range arithmetic

`crates/ide-assists/src/handlers/inline_call.rs:390` and `:416`:

```rust
body.syntax().covering_element(range - body_offset)
```

`range` is a `FileReference` range from `usages_for_locals`; if a usage lies
outside `body` (e.g. macro-expanded), the subtraction underflows or the
result exceeds `body`'s length.

Similarly, `ide-assists/src/handlers/extract_variable.rs` →
`utils::cover_edit_range`: descending tokens into an **unresolved macro
call** yields an `original_range` that is not contained in the node passed
in. This one is reproduced (see below) and needs no proc macros or desync.

### Path C: client/server version skew

rust-analyzer retries in-flight requests after `didChange`. Positions
validated against text version N can be replayed against version N+1's tree
in entrypoints reached without full re-validation. The resolve-request
handlers in `crates/rust-analyzer/src/handlers/request.rs` mostly re-derive
offsets via `from_proto` and check document versions, so this path mostly
produces clamped (wrong but in-bounds) positions — the residual risk is
`Analysis`-layer APIs that receive raw `TextSize`/`TextRange` values kept
from an earlier snapshot.

## Verified reproductions

1. Exact message, in-process, no desync (`cargo test -p ide-assists --lib
   extract_variable::tests::repro_bad_range_unresolved_macro_paren`):

   `crates/ide-assists/src/handlers/extract_variable.rs`, test
   `repro_bad_range_unresolved_macro_paren`: request assists on
   `fn f() { m!$0($0 }` (selection covering the `(` inside an unresolved
   macro call). Panics with `Bad range: node range 9..11, range 11..12`.

2. Layer demonstration (`cargo test -p ide --lib
   lsp_error_repros::out_of_bounds_offset_panics`): completions,
   extend-selection and matching-brace panic when handed an offset 2 bytes
   past EOF at the `Analysis` API layer — the state any of paths A-C ends
   in.

## Fix directions

Fix at the chokepoints, not the dozens of `covering_element` call sites:

1. **Clamp/validate in `resolve_span`** (`crates/hir-expand/src/db.rs:126`):
   clamp the computed `FileRange` to the anchor file's text length (the file
   text is available via the database), or return an `Option`/sentinel the
   two `files.rs` callers can treat as "fall back to the macro call node"
   (both already have a fallback path for the not-found case).
2. **Guard the two sinks** `crates/hir-expand/src/files.rs:353` and `:538`:
   check `range.end() <= parse.syntax_node().text_range().end()` before
   calling `covering_element`, else take the existing fallback.
3. **Validate proc-macro spans** in `deserialize_span_data_index_map`
   (`crates/proc-macro-api/.../flat.rs:69`): reject or clamp ranges with
   `end < start` or implausible lengths.
4. **Fix the assist arithmetic**: in `inline_call.rs:390/:416` check
   `body.syntax().text_range().contains_range(range)` before subtracting;
   in `extract_variable`/`cover_edit_range` verify containment of
   `original_range` results before use (this specific one is what the
   repro test exercises).
5. Optionally, make the `Analysis` layer defensive: entrypoints taking
   `FilePosition`/`FileRange` could validate against the file length and
   return empty results instead of panicking (matches the server's
   stateless/retry design).

## Acceptance criteria

- `repro_bad_range_unresolved_macro_paren` no longer panics — flip its
  `#[should_panic]` into a plain test asserting the assist is simply not
  applicable.
- `lsp_error_repros::out_of_bounds_offset_panics` inverted: assert that
  *no* probed feature panics (if option 5 is taken; otherwise leave it
  documenting the layer contract).
- No `covering_element`/`token_at_offset` call reachable from
  `original_syntax_node_rooted` / `original_ast_node_rooted` can receive a
  range past the tree, even with a hostile proc-macro server.

## Key files

- `crates/rust-analyzer/src/handlers/dispatch.rs:349` (panic → LSP error)
- `crates/rust-analyzer/src/lsp/from_proto.rs:25-64`
- `crates/hir-expand/src/db.rs:126` (`resolve_span`, unclamped)
- `crates/hir-expand/src/files.rs:353`, `:538` (panic sinks)
- `crates/hir-expand/src/lib.rs:945-986` (`map_node_range_up*`)
- `crates/proc-macro-api/src/legacy_protocol/msg/flat.rs:69-83`
- `crates/ide-assists/src/handlers/inline_call.rs:390`, `:416`
- `crates/ide-assists/src/handlers/extract_variable.rs` (repro test)
- rowan assert: `rowan-0.15.18/src/cursor.rs:787-796`
