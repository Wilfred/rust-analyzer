# `request handler panicked: unmatched closing delimiter from syntax fixup`

## Symptom

LSP requests fail with:

```
request handler panicked: unmatched closing delimiter from syntax fixup
```

## Where the panic fires

`crates/syntax-bridge/src/lib.rs:243`, in `convert_tokens`, in the arm
handling `tt::Leaf::Punct` tokens whose char is a delimiter:

```rust
Some(&tt::Leaf::Punct(Punct { char: char @ ('(' | ')' | '{' | '}' | '[' | ']'), .. })) => {
    // if it's a closer we expect: close subtree(s) and continue
    ...
    let delim = match char {
        '(' => tt::DelimiterKind::Parenthesis,
        '{' => tt::DelimiterKind::Brace,
        '[' => tt::DelimiterKind::Bracket,
        _ => panic!("unmatched closing delimiter from syntax fixup"),   // ')' '}' ']'
    };
    builder.open(delim, span);
}
```

Fires iff a **leaf** punct closing delimiter (`)`/`}`/`]`) arrives while no
open subtree on the builder stack has the matching kind. Real (non-leaf)
tokens can never reach it: an unexpected *real* closer falls into the
plain-token path (lib.rs ~298) and silently becomes a stray `Punct` leaf
(there is a FIXME there noting the asymmetry).

## Reachability — who produces leaf delimiter puncts

Leaf tokens only exist in the `Converter` used by
`syntax_node_to_token_tree_modified`, whose sole production caller is
`attr_macro_input_to_token_tree` (`crates/hir-expand/src/cfg_process.rs:295`)
via the `macro_arg` salsa query (`crates/hir-expand/src/db.rs` ~488). Note:
**function-like `m!(...)` args do not take this path** — only
**attribute-macro and derive inputs** (the annotated item) do. Two sources
inject leaves:

1. **Syntax fixup** (`crates/hir-expand/src/fixup.rs`): appends recovery
   tokens for incomplete syntax. Since commit `5335d8cbc5` ("Fix syntax
   fixup producing invalid punctuation", 2025-02-28 — the commit that also
   *introduced* this panic), every delimiter fixup appends is a **balanced,
   contiguous `{`/`}` pair** under a single anchor, which self-closes
   immediately — it cannot trigger the panic. (Before that commit, fixup
   appended a **lone `)`** for unclosed `ArgList`s — the shape behind
   issues #18244/#19206; builds older than 2025-03 panic on e.g.
   `#[attr] fn f() { g(a }`.)
2. **cfg_attr reconstruction** (`macro_input_callback`,
   `crates/hir-expand/src/cfg_process.rs:239-287`): when expanding
   `#[cfg_attr(pred, attrs...)]` inside an attr/derive input, it inserts
   leaf `#`, (`!`,) `[` when a reconstructed attribute starts and a leaf
   `]` when it ends — with the attribute's **real meta tokens streamed in
   between**. This channel was added by the "Rewrite attribute handling"
   commit `f0e372c3b6` (2025-11-29) and is the **live route**.

## Root cause

The multi-close recovery in `convert_tokens` closes **every** open subtree
up to the matched delimiter:

```rust
let found_expected_delimiter = builder.expected_delimiters().enumerate()
    .find(|(_, delim)| ...matches closer...);
if let Some((idx, _)) = found_expected_delimiter {
    for _ in 0..=idx { builder.close(span); }   // sweeps inner subtrees too
    continue;
}
```

If the real meta tokens between an inserted leaf `[` and its leaf `]`
contain a **stray closing delimiter** (a mid-edit typo), the recovery
matches an outer subtree and **sweeps the leaf-opened `[` subtree away**.
When the inserted leaf `]` then arrives, no bracket is open → panic.

## Verified reproduction

`cargo test -p hir-def --lib repro_unmatched_closing_delimiter_from_cfg_attr_reconstruction`
(`crates/hir-def/src/macro_expansion_tests/proc_macros.rs`), exact message
asserted:

```rust
//- proc_macros: identity
#[cfg_attr(true, allow(]))]
#[proc_macros::identity]
fn f() {}
```

The stray `]` inside `allow(...)`'s token tree (which still parses as a
valid attr — a stray `}` breaks the attr node instead and takes a different
path) matches the leaf-inserted `[` bracket in the recovery scan, closes it
early, and the inserted `]` panics. Both attribute orders reproduce, as
does `#[cfg_attr(true, allow(]))]` on items under a derive.

One stray-bracket typo inside a `cfg_attr` on any attr/derive-macro'd item
takes down every macro-expanding feature for the file (diagnostics,
completions, highlighting, hover) until edited.

Related silent-corruption sibling (worth fixing in the same pass): when the
cfg_attr meta does *not* parse (e.g. a stray `}`), reconstruction is
skipped, and the stray real closers flow through the silent stray-punct arm
(lib.rs ~298 FIXME), producing corrupted expansions like
`#[cfg_attr (true , allow())]) ) ]` rather than a panic.

## Fix directions

1. **Make the leaf-closer arm total** (minimal): treat an unmatched leaf
   closer exactly like the real-token path does — emit it as a stray
   `Punct` leaf (or drop it) instead of panicking. The fixup/undo machinery
   already tolerates stray puncts; `reverse_fixups` strips
   `FIXUP_DUMMY_AST_ID`-marked tokens.
2. **Don't let recovery sweep synthetic subtrees**: the multi-close
   recovery could stop at (not close past) subtrees opened by *inserted*
   leaves (identifiable by their spans: cfg_attr-inserted brackets carry
   `brackets_span`), so a stray real closer inside a reconstructed
   attribute can't orphan the attribute's `]`.
3. **Balance-check in `macro_input_callback`**: track delimiter depth of
   the streamed meta tokens between the inserted `[` and `]`; if
   unbalanced, either censor the whole attribute from the input or emit
   the meta as an opaque balanced group.
4. Address the lib.rs ~298 FIXME (silent stray closer) with a recorded
   error while at it, so corrupted expansions become diagnosable.

Option 1 is the safe crash fix; 2 or 3 preserve better-formed macro inputs.

## Acceptance criteria

- `repro_unmatched_closing_delimiter_from_cfg_attr_reconstruction` no
  longer panics — flip `#[should_panic]` into a plain expansion test (the
  expansion may contain error recovery, but must not unwind).
- The green brute-force probe
  (`ide lsp_error_repros::syntax_fixup_unmatched_closing_delimiter_probe`,
  ~28 broken-body × 14 attr-shape combinations plus targeted statement/field
  cases) stays green.
- Fuzz-ish confidence: no panic for stray `)`/`}`/`]` at any position
  inside cfg_attr metas on items, statements, and fields under attr/derive
  macros.

## Key files

- `crates/syntax-bridge/src/lib.rs`: leaf-delimiter arm (~220-248, panic at
  243), multi-close recovery (~232-236 and ~270-276), silent stray-closer
  FIXME (~298), end-of-input auto-close (~354)
- `crates/hir-expand/src/cfg_process.rs`: `macro_input_callback`
  (~239-287, leaf `#`/`[`/`]` insertion), `attr_macro_input_to_token_tree`
  (~295)
- `crates/hir-expand/src/fixup.rs`: append/remove model (balanced `{`/`}`
  pairs only), `reverse_fixups`, test at ~482 asserting fixup emits no bare
  delimiter puncts
- History: `git show 5335d8cbc5` (introduced the panic, removed the lone-`)`
  append; fixes #18244/#19206), `git show f0e372c3b6` (added the cfg_attr
  reconstruction channel that made the panic reachable again)
- `crates/hir-def/src/macro_expansion_tests/proc_macros.rs` (repro),
  `crates/ide/src/lsp_error_repros.rs` (probe corpus)
