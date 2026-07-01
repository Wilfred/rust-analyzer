# In-the-wild LSP request panics: analysis and reproductions

Investigation of five error messages that rust-analyzer LSP deployments
report as `request handler panicked: ...` (the wrapper lives in
`crates/rust-analyzer/src/handlers/dispatch.rs`, which `catch_unwind`s each
request and stringifies the panic payload). Four of the five are reproduced
with exact panic messages by executable tests on this branch; the fifth is
analyzed and a sibling crash in the same machinery is reproduced.

Reproductions (all asserted with `#[should_panic]`, so the suite stays green):

| Error | Repro test | Status |
|---|---|---|
| `Bad range: node range ...` | `ide-assists handlers::extract_variable::tests::repro_bad_range_unresolved_macro_paren`; `ide lsp_error_repros::out_of_bounds_offset_panics` | reproduced (exact message) |
| `failed to unify type owners` | `ide lsp_error_repros::term_search_unify_type_owners` | reproduced (exact message) |
| `can't project out of {type error}` | `hir-ty tests::panic_repros::tuple_pattern_on_error_self_alias` (+3 variants) | reproduced (exact message) |
| `field FieldIndex(N) out of range (&'{region error} ...` | `hir-ty tests::panic_repros::out_of_bounds_tuple_field_region_error` (+1 variant) | reproduced (exact message) |
| `assertion failed: Can't merge cycle heads ... variances_of_query` | `hir-ty tests::panic_repros::parallel_variances_cycle_heads` (`#[ignore]`d soak) | not reproduced; sibling salsa panic reproduced |

Run them with:

```
cargo test -p hir-ty --lib tests::panic_repros
cargo test -p ide --lib lsp_error_repros
cargo test -p ide-assists --lib extract_variable::tests::repro_bad_range
VARIANCE_ROUNDS=10000 cargo test -p hir-ty --lib parallel_variances_cycle_heads -- --ignored  # soak
```

---

## 1. `request handler panicked: Bad range: node range ...`

**Where.** rowan `SyntaxNode::covering_element`
(`rowan-0.15.18/src/cursor.rs:792`) asserts
`node.text_range().contains_range(range)`. The sibling assert in
`token_at_offset` produces the related `Bad offset` message.

**Why it happens.** A range computed against one text/tree is applied to a
different tree. `lsp/from_proto.rs::offset()` clamps *columns* to the line
length and errors on out-of-range *lines*, so a straightforward stale
position on the current file is usually converted safely. The panics come
from paths that bypass that protection:

- **Macro span up-mapping.** `hir_expand::db::resolve_span` computes
  `span.range + anchor_offset` with **no clamping to the file length**, and
  `map_node_range_up*` unions span ranges (min start / max end). The result
  is fed to `covering_element` on the real file's tree in
  `hir-expand/src/files.rs` (`original_syntax_node_rooted`,
  `original_ast_node_rooted`) with no bounds check. Proc-macro servers
  contribute raw spans through
  `proc-macro-api .../msg/flat.rs::deserialize_span_data_index_map`, which
  performs **no validation** of the deserialized ranges, so a buggy or
  version-skewed proc-macro server can inject arbitrary ranges.
- **Range arithmetic across trees**, e.g. `ide-assists` `inline_call.rs`
  computes `covering_element(range - body_offset)` with a usage range that
  can originate outside the body node.
- **Client/server desync while requests are retried** (the FIXME in
  `from_proto.rs` about request retrying): offsets validated against text
  version N replayed against version N+1 can still land out of bounds at
  the `Analysis` layer for entrypoints reached without re-validation.

**Repros.**
- Exact message, no desync required:
  `extract_variable::tests::repro_bad_range_unresolved_macro_paren` —
  code-action request on `fn f() { m!( }` selecting the `(`. Descending into
  the unresolved macro yields an `original_range` outside the node passed to
  `cover_edit_range` → `Bad range: node range 9..11, range 11..12`.
- Layer demonstration: `ide::lsp_error_repros::out_of_bounds_offset_panics`
  shows completions / extend-selection / matching-brace panic when handed an
  offset 2 bytes past EOF.

**Fix direction.** Clamp in the chokepoints rather than each caller: bail (or
clamp) in `resolve_span` / `map_node_range_up*` when
`range.end() > file_len`, and validate spans in
`deserialize_span_data_index_map`.

## 2. `request handler panicked: failed to unify type owners`

**Where.** `crates/hir/src/lib.rs` `TypeOwnerId::must_unify`
(`expect("failed to unify type owners")`), called from
`Type::could_unify_with{,_deeply}` / `could_coerce_to` /
`generic_args_from_tys`.

**Why it happens.** Every `hir::Type` carries a `TypeOwnerId` (the
`GenericDefId` its parameters are numbered against). `could_unify_with*`
*panics* when the two types' owners are two different real defs instead of
returning `false`. Most call sites are protected by rebase helpers
(`rebase_ty`, `instantiate_with_errors` → `NoParams`), but **term search**
is not: `LookupTable::find`/`find_autoref` (`hir/src/term_search.rs`) and
`tactics.rs` compare `TypeParam::ty` / `Field::ty` / `ret_type().instantiate(..)`
values whose owners are whatever item they came from, against lookup-table
entries owned by other items. Term search runs from **expression
completions** (`enable_term_search`), the typed-hole diagnostic fix, and the
term-search assist — i.e. passive, every-keystroke surfaces. Completion
relevance `match_types` (render.rs) is a second, scope-divergence candidate.

**Repro.** `ide::lsp_error_repros::term_search_unify_type_owners`:

```rust
static X: i32 = 5;
const Y: u32 = 1;
fn g(x: i32) -> u32 { 0 /*complete here*/ }
fn f() -> u32 { g(_) }
```

Requesting completions runs term search; the lookup table mixes types owned
by the `static`, the `const`, and the functions → exact panic.

**Fix direction.** `could_unify_with*` should treat owner mismatch as "does
not unify" (return `false`, or rebase param-free types), reserving the
panic for genuinely inconsistent internal use.

## 3. `request handler panicked: can't project out of {type error}`

**Where.** `crates/hir-ty/src/mir.rs` `PlaceTy::field_ty`, fallthrough arm
(`panic!("can't project out of {self_ty:?}")`), introduced by the
`PlaceTy` rewrite (`bde303ad75`, part of the new-solver work; before that
commit these were recoverable `stdx::never!`s). `{type error}` is the
`Debug` rendering of `TyKind::Error` in `rustc_type_ir`.

**Why it happens.** This code recomputes a MIR place's type at
**borrowck/const-eval time** by re-projecting from the base local's stored
type. It is reached from `db.borrowck(..)`, which
`hir::DefWithBody::diagnostics` runs for **every body** when file
diagnostics are computed — completely passive. The guard in
`projection_ty_core` returns early only if the base type is *already*
`TyKind::Error` **at the top level**; when the base is a `TyKind::Alias`,
the `structurally_normalize` callback runs first and can *produce*:

- `Ty::new_error(..)` when normalization fails (e.g. the alias' args contain
  an error type) → `can't project out of {type error}`;
- the rigid alias unchanged (no equality bound in the env) →
  `can't project out of Alias(..)`;
- an **unconstrained inference variable** when normalization is ambiguous
  (conflicting or cyclic equality bounds) → `can't project out of ?0t`.

MIR with such projections gets built because the "bail on broken bodies"
guards are weak: `has_type_mismatches` only fires on recorded mismatch
nodes, and inference does *not* record a mismatch when it fails to relate a
tuple pattern against an alias it cannot normalize; `is_erroneous` only
catches bodies with zero typed expressions.

**Repros** (all in `hir-ty tests::panic_repros`, exact messages asserted):

```rust
// can't project out of {type error}
trait Tr { type A; }
fn f(x: <Missing as Tr>::A) {   // error self-type inside the alias
    let (mut a, b) = x;         // tuple pattern emits Field projections
    a = 1;
}

// can't project out of Alias(..)   — rigid alias, wrong pattern
fn f<T: Tr>(x: T::A) { let (mut a, b) = x; a = 1; }

// can't project out of ?0t          — ambiguous normalization
fn f<T: Tr<A = (u32, u32)> + Tr<A = (u32, u32, u32)>>(x: T::A) {
    let (mut a, b, c) = x; a = 1;
}
```

All are ordinary mid-edit states (an unresolved name in a signature, a
missing equality bound, duplicated bounds).

**Fix direction.** After `structurally_normalize`, short-circuit to an error
type instead of calling `handle_field` when the normalized base is not an
ADT/closure/tuple (mirroring the pre-rewrite `never!` + error-type
recovery), and make `field_ty`'s fallthrough return `Ty::new_error` rather
than panic.

## 4. `request handler panicked: field FieldIndex(12345) out of range (&'{region error} ...`

**Where.** Same function, tuple arm:
`panic!("field {f:?} out of range: {self_ty:?}")`
(`crates/hir-ty/src/mir.rs`). The `(&'{region error} ...` fragment in the
in-the-wild report is the `Debug` of the base **tuple type** whose first
element is a reference with a lifetime that failed to resolve
(`RegionKind::ReError` prints as `'{region error}`).

**Why it happens.** An out-of-bounds tuple field access (`t.1` on a
1-tuple, `t.99`, a macro-generated index — plain typos or mid-edit states)
is lowered to a MIR `Field` projection anyway: tuple indices need no
`field_resolution` and the bad index does not mark the body as having type
mismatches. Borrowck then recomputes the place type and indexes past the
tuple's arity. Before commit `bde303ad75` this was
`never!("Out of bound tuple field")` — logged and recovered in release
builds; now it is a hard panic that takes down diagnostics for the file.

**Repros** (exact messages asserted):

```rust
fn f() { let t = (1,); let x = t.1; }
// panicked at crates/hir-ty/src/mir.rs: field FieldIndex(1) out of range: (i32,)

fn f(s: &'missing str) { let t = (s, 1u32); let x = t.99; }
// field FieldIndex(99) out of range: (&'{region error} str, u32)
```

**Fix direction.** The arm already uses `.get(..)`; return
`Ty::new_error` in the `None` case instead of panicking.

## 5. `assertion failed: Can't merge cycle heads variances_of_query(...)`

**Where.** salsa (0.27.2 pinned) `CycleHeads::insert`
(`src/cycle.rs:346`): merging two live copies of the same cycle head with
**different fixpoint iteration counts**. The head being
`variances_of_query` implicates `crates/hir-ty/src/variance.rs`, which is
rust-analyzer's only fixpoint-iterating salsa query
(`cycle_fn`/`cycle_initial`); its module docs already flag salsa cycle
handling as a risk.

**Why it happens.** `variances_of(X)` recursively queries `variances_of` of
every ADT mentioned in X's fields, so mutually recursive generic ADTs form
query cycles that salsa resolves by fixpoint iteration. With **parallel**
query evaluation (rust-analyzer runs diagnostics/prime-caches on many
threads, with cancellation on every keystroke), provisional memos stamped at
iteration *i* can survive (via cross-thread head-lock transfer, provisional
reuse, or post-panic lingering) while another thread has advanced the same
head to iteration *i+1*; when the head sets are unioned
(`ActiveQuery::add_read` / `collect_all_cycle_heads`), the assert fires.
salsa ships a dedicated parallel regression test for exactly this shape
(`tests/parallel/cycle_iteration_mismatch.rs`) — driven by the `shuttle`
deterministic-interleaving explorer, which is what it takes to pin the
race.

**Repro status.** `hir-ty tests::panic_repros::parallel_variances_cycle_heads`
(`#[ignore]`d soak) mirrors salsa's regression-test dependency graph as six
mutually recursive generic ADTs and evaluates `variances_of` from 8
staggered threads. Under plain OS scheduling it has not yet produced the
cycle-heads assert, but it **does intermittently crash inside the same
salsa parallel-cycle machinery** (roughly once per few thousand rounds on
8 cores):

```
panicked at salsa-0.27.2/src/runtime/dependency_graph.rs:422:
Circular reference between blocked edges: Edges({ThreadId(..): Edge { blocked_on_id: ThreadId(..), .. }})
```

which independently confirms that parallel `variances_of` cycles violate
salsa's blocking/merging invariants. Reproducing the exact assert
deterministically needs shuttle-style scheduling control inside salsa
itself; recommended next step is to report both crashes upstream to salsa
with this workload (and rust-analyzer's `variances_of` shape) attached, and
to check salsa releases after 0.27.2 for fixes to
`collect_all_cycle_heads` / `CycleHeads::insert`.

---

## Notes

- The `PlaceTy` panics (#3, #4) are regressions of hardness, not of logic:
  the pre-rewrite code hit the same inconsistencies but recovered with
  `stdx::never!` + error types in release builds. Since `db.borrowck` runs
  for every body during diagnostics, a single `t.1`-on-a-1-tuple typo
  anywhere in an open file now kills the diagnostics request until edited.
- `{type error}` / `'{region error}` in the reported messages are Debug
  renderings from `rustc_type_ir` (`TyKind::Error` / `ReError`), which is
  how the reports were matched to the new-solver code paths.
- A related earlier investigation with additional assist-level panics lives
  on the `claude/panics-unfinished-code-9apu61` branch (`PANIC_FINDINGS.md`
  there); its term-search and out-of-bounds-tuple findings were confirmed
  and re-anchored to the current `PlaceTy` code by this branch.
