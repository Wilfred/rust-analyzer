# `request handler panicked: used an incorrect AttrId`

## Symptom

LSP requests — reproduced via `textDocument/completion` — fail with:

```
request handler panicked: used an incorrect `AttrId`; crate=Crate(Id(...)), attr_id=AttrId { id: N }
```

## Where the panic fires

`crates/hir-expand/src/attrs.rs:400`, in
`AttrId::find_attr_range_with_source`:

```rust
pub fn find_attr_range_with_source(
    self,
    db: &dyn ExpandDatabase,
    krate: Crate,
    owner: &dyn ast::HasAttrs,
) -> (ast::Attr, ast::Meta) {
    let mut index = 0;
    let result = collect_item_tree_attrs(owner, || krate.cfg_options(db), |meta, top_attr| {
        if index == self.id { return ControlFlow::Break((top_attr, meta)); }
        index += 1;
        ControlFlow::Continue(())
    });
    match result {
        Some(Either::Left(it)) => it,
        _ => panic!("used an incorrect `AttrId`; crate={krate:?}, attr_id={self:?}"),
    }
}
```

`find_derive_range` (attrs.rs:405) funnels through the same walk and panics
identically.

This machinery is new: `git log -S "used an incorrect"` shows it was
introduced by the **"Rewrite attribute handling"** commit `f0e372c3b6`
(2025-11-29; first landed as `455ca02f17`, reverted, re-landed). There is no
pre-existing recoverable version — the panic arrived with the rewrite.

## Background: what an AttrId is

- `AttrId { id: u32 }` (attrs.rs:349) — a bare index into an item's
  **cfg_attr-expanded, filtered** attribute list, in the order produced by
  `collect_item_tree_attrs` (attrs.rs:180-215):
  - active `cfg_attr(pred, a, b)` contributes its inner metas individually;
    an **inactive** cfg_attr contributes nothing — so the index depends on
    the crate's `CfgOptions`;
  - builtin "item-tree filtered" attrs (doc/allow/repr/inline/…,
    `is_item_tree_filtered_attr`, attrs.rs:159) are skipped and not counted;
  - a disabled bare `#[cfg(false)]` stops collection early.
- Minted during item-tree lowering (`AttrsOrCfg::lower`,
  `crates/hir-def/src/item_tree/attrs.rs:44`), using the **same walk** with
  the **crate's** cfg options — the item tree query is keyed on
  `(HirFileId, Crate)` (`file_item_tree_query`,
  `crates/hir-def/src/item_tree.rs:141`). AttrIds are therefore
  **crate-specific**.
- Stored in salsa-interned structs: `MacroCallKind::Attr/Derive` inside
  `MacroCallLoc` (hir-expand/src/lib.rs:229-357), `BuiltinDeriveImplLoc`,
  and `DefDiagnosticKind::{InvalidDeriveTarget, MalformedDerive}`.

The panic means: the resolve-time walk (owner node + `krate.cfg_options`)
produced **fewer countable attributes** than the mint-time walk, so the
stored index is out of range.

## Resolve call sites (how LSP requests reach it)

| Site | Feature |
|---|---|
| `crates/hir-expand/src/db.rs:228` (`expand_speculative`) | **completion inside attribute-macro args** — the reproduced path |
| `crates/hir/src/semantics.rs:1352` (`descend_into_macros` attr strip) | hover / goto / find-refs token descent |
| `crates/hir/src/semantics/child_by_source.rs:129` (`// FIXME: Is this the right crate?`) | navigating derive-generated items |
| `crates/hir/src/lib.rs:1300/1308/1344/1349` (def-map diagnostics) | publishDiagnostics (InvalidDeriveTarget, MalformedDerive, unresolved attr/derive) |
| `crates/hir-expand/src/lib.rs:638/643/764/768` (`original_call_range*`) | goto-def, hover, call hierarchy of macro calls |
| `crates/hir-def/src/builtin_derive.rs:161` | builtin-derive impl navigation |

## Verified reproduction

`cargo test -p ide --lib lsp_error_repros::incorrect_attr_id_speculative_cfg_attr`
(`crates/ide/src/lsp_error_repros.rs`), exact message asserted:

```rust
//- proc_macros: identity
//- /lib.rs crate:a cfg:feature=on
#[cfg_attr(feat$0ure = "on", proc_macros::identity)]
fn f() {}
```

Requesting completions at `$0` (inside the cfg_attr **predicate**) panics
with `used an incorrect AttrId; crate=Crate(Id(...)), attr_id=AttrId { id: 0 }`.

Mechanism: completion inside macro inputs runs `expand_speculative`
(hir-expand/src/db.rs:228), which resolves the *stored* AttrId (minted from
the real item, where the cfg_attr predicate is intact and active, so the
`proc_macros::identity` meta is countable index 0) against the
**speculatively edited copy** of the item — with the completion's dummy
token spliced in at the cursor. The mutated predicate changes how
`collect_item_tree_attrs` sees the cfg_attr in the speculative copy, the
walk ends before index 0, and resolution panics instead of degrading.

An offset sweep shows the panic fires for completion at essentially **every
offset inside the cfg_attr predicate region**, also with nested
`cfg_attr(…, cfg_attr(…, attr))` and with `cfg_attr`-wrapped derives. Typing
inside a `cfg_attr` that gates an attribute/derive macro is an ordinary
editing action, consistent with the error being seen frequently.

## Other incoherence candidates (analyzed, not yet reproduced)

- **Same file in two crates with different cfgs**: AttrIds are minted per
  `(file, krate)`, and `find_attr_range(db, krate, owner)` takes `krate`
  independently — nothing ties it to the minting crate. A shared-file
  two-crate fixture swept with hover/goto/diagnostics did *not* fire
  (`lsp_error_repros::incorrect_attr_id_probe` stays green), because the
  obvious callers all pass `loc.krate`. The self-flagged risk is
  `child_by_source.rs:129`'s FIXME.
- **Crate-root injected attributes**: minting prepends
  `parse_extra_crate_attrs` (build-system-injected `#![...]`) to the crate
  root's attr list (item_tree.rs:154-165), but `find_attr_range_with_source`
  walks only the file node without that prefix — an off-by-N for
  crate-root inner attributes. Relevant to environments that inject
  crate-level attrs (e.g. Buck/Bazel `crate_attrs`).
- **Out-of-line modules**: attrs.rs:345's own doc comment warns the AttrId
  scheme "does not support out-of-line modules, which may have attributes
  spread across 2 files".

## Fix directions

1. **Make resolution total** (minimal): return `Option`/a fallback range
   from `find_attr_range_with_source`/`find_derive_range` instead of
   panicking; callers can fall back to the whole attribute list range or
   the item's name range. For `expand_speculative` specifically, a miss on
   the speculative copy should abort the speculative expansion gracefully
   (completions just lose one source of results).
2. **Fix the speculative mismatch at the source**: `expand_speculative`
   should locate the attribute in the speculative copy *structurally*
   (e.g. by re-running the same collection on the speculative item and
   matching by attr syntax position), not by replaying an index minted
   from the real item against a mutated tree.
3. **Bind the crate to the AttrId**: store the minting `Crate` alongside
   (or resolve through the item tree that produced the id), eliminating the
   independent-`krate` footgun; resolve the `child_by_source.rs:129` FIXME.
4. Account for `parse_extra_crate_attrs` in `find_attr_range_with_source`
   (or assert the owner is never a crate root), covering the injected-attrs
   candidate.

## Acceptance criteria

- `lsp_error_repros::incorrect_attr_id_speculative_cfg_attr` no longer
  panics — flip its `#[should_panic]` into a plain test asserting
  completions return.
- Completion still works inside cfg_attr predicates (returns cfg values /
  attr names as appropriate).
- The two green probes (`incorrect_attr_id_probe`, offset sweep) stay green.

## Key files

- `crates/hir-expand/src/attrs.rs`: panic (400), `collect_item_tree_attrs`
  (180-215), `AttrId` (349), filtered attrs (159)
- `crates/hir-expand/src/db.rs:228` (`expand_speculative` — reproduced path)
- `crates/hir-def/src/item_tree/attrs.rs:44` (minting),
  `crates/hir-def/src/item_tree.rs:141-197` (crate-keyed item tree,
  extra_crate_attrs merge at 154-165)
- `crates/hir/src/semantics/child_by_source.rs:127-129` (krate FIXME)
- `crates/ide/src/lsp_error_repros.rs` (repro + probes)
- History: `git show f0e372c3b6` ("Rewrite attribute handling")
