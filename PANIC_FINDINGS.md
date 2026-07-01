# Panics reachable from legitimate intermediate states of user code

17 verified panics/crashes in rust-analyzer that fire on code states a user
passes through while typing (unclosed delimiters, half-typed literals,
out-of-bounds field access, recursive definitions), plus a few on ordinary
complete code. Every finding is backed by an **executable reproduction** on
this branch (tests prefixed `zztemp_`); two brute-force harnesses re-discover
most of them from scratch:

- `crates/ide-assists/src/tests.rs::zztemp_brute_force_incomplete_code` — runs
  all assists (`AssistResolveStrategy::All`) over every selection range of ~55
  unfinished-code snippets.
- `crates/ide/src/zztemp_incomplete.rs::zztemp_ide_brute_force_incomplete_code`
  — runs hover, goto-def, completions, signature help, diagnostics-with-fixes,
  inlay hints, highlight, rename, find-refs, join-lines, call hierarchy,
  on-char-typed, etc. at every offset of ~110 snippets (including unicode,
  cyclic definitions, typed holes, and MIR-triggering code).

Run: `cargo test -p ide-assists zztemp` and `cargo test -p ide zztemp`.
(All `zztemp_` tests are *intentional-failure* reproductions.)

---

## Severity ranking

Ranked by (blast radius on a passive IDE feature that runs unprompted) ×
(how ordinary the triggering code is) × (crash hardness).

| # | Severity | Location | Trigger surface | Trigger code |
|---|----------|----------|-----------------|--------------|
| 1 | **Critical** | rowan `cursor.rs` "Bad offset"/"Bad range" via ide entrypoints | hover, completions, goto, sig-help, join-lines, on-char-typed, … (request retry) | any edit that shrinks the file while a request is in flight |
| 2 | **Critical** | `hir-ty/src/mir.rs:217` (borrowck) | **diagnostics** (runs on every file, unprompted) | `t.99` — out-of-bounds tuple field |
| 3 | **Critical** | `hir/src/lib.rs:5219` `must_unify` | **completions** (term search) | typed hole / any expr completion near items of different owners |
| 4 | **Critical** | `layout_of_adt` ↔ `layout_of_ty` **stack overflow** (uncatchable, aborts process) | hover (memory layout) | `struct A<T>(B<fn(T)>); struct B<T>(A<fn(T)>);` polymorphic recursion |
| 5 | **High** | `ide-db/src/source_change.rs:80` `never!` | **rename** | rename `self`→name with a chained-call site; complete code |
| 6 | **High** | `unwrap_return_type.rs:120` | assist (shown in lightbulb) | `-> Option<()>` with a `match` tail; **complete code** |
| 7 | **High** | `flip_comma.rs:82` | assist | `#[repr(,​ C)]` leading comma |
| 8 | **High** | `remove_unused_param.rs:197` | assist | `fn foo(x: i32,` / `foo(1,` unclosed |
| 9 | **High** | `generate_getter_or_setter.rs:443` | assist | `impl S` with no body yet |
| 10 | **High** | `generate_enum_is_method.rs:80` | assist | `impl E` with no body yet |
| 11 | **High** | `generate_enum_projection_method.rs:159` | assist | `impl E` with no body yet |
| 12 | **High** | `extract_function.rs` → `edit_algo.rs:297` `unimplemented!` | assist | selection spanning fn header/body in incomplete code |
| 13 | **Medium** | `extract_variable.rs` → `cover_edit_range` "Bad range" | assist | selecting `(` in unresolved `m!(` |
| 14 | **Medium** | `replace_let_with_if_let.rs` → `expr_if` | assist | `let s = "` unterminated string initializer |
| 15 | **Medium** | `raw_string.rs` → `make.rs:601` assert | assist | half-typed raw ident `let r#  = 1;` |
| 16 | **Low** | `generate_enum_is_method.rs:59` `never!` | assist | selection covering only the comma between variants |

Why the top of the list is worse than the assists: items 1–3 fire from
**passive features** — diagnostics and hover/completions run without the user
invoking anything, so the crash reaches users who never asked for a refactor.
The assist panics (6–16) only fire once the user opens the lightbulb menu on
that exact spot, and several are already caught per-request by the server's
`catch_unwind`, degrading to a missing code action rather than a session
crash. Item 4 is singled out as Critical despite a narrow trigger because a
stack overflow **aborts the whole process** — it cannot be caught by the
per-request `catch_unwind` that contains the other panics.

---

## Detailed findings

### 1. Out-of-bounds offsets/ranges — "Bad offset" / "Bad range" (rowan)
`Bad offset: range 0..22 offset 23` and `Bad range: node range …`
Repro: `zztemp_out_of_bounds_ranges_desynced_client`.

Every position/range-taking entrypoint (`hover`, `completions`,
`goto_definition`, `signature_help`, `extend_selection`, `on_char_typed`,
`join_lines`, `matching_brace`) forwards the client offset straight into
`SyntaxNode::token_at_offset` / `covering_element`, which **assert** the
offset is within the tree. `lsp/from_proto.rs::offset` clamps a column to its
line but cannot protect against **version skew**: rust-analyzer retries
in-flight requests after a `didChange`, so an offset computed against text
version *N* is replayed against the syntax tree of version *N+1*. When the
edit shrank the file, the offset is now past EOF and the assert fires. This is
the exact shape of the widespread in-the-wild `request handler panicked: Bad
range: node range …` reports. Highest severity: passive, frequent,
input-independent, affects nearly every feature.

### 2. MIR borrowck — out-of-bounds tuple field (`mir.rs:217`)
`internal error: entered unreachable code: Out of bound tuple field`
Repro: `zztemp_mir_out_of_bounds_tuple_field`.

```rust
fn f() { let t = (1,); let x = t.1 + t.99; }
```

`t.99` on a 1-tuple is a mid-typing state (or a plain typo). Diagnostics run
MIR borrowck (`moved_out_of_ref` → `ProjectionElem::projected_ty`), whose
tuple-field arm hits `never!("Out of bound tuple field")`. Because it's in a
**salsa query executed for diagnostics**, it runs unprompted on the open file.
This is the same family as the wild `can't project out of {type error}` /
`field FieldIndex(...) out of range` reports — projection code assuming the
field index is in bounds for the base type.

### 3. Term search — "failed to unify type owners" (`hir/lib.rs:5219`)
Repro: `zztemp_term_search_unify_type_owners`.

```rust
static X: i32 = 5;
const Y: u32 = 1;
fn g(x: i32) -> u32 { 0 /*<cursor here>*/ }
fn f() -> u32 { g(_) }
```

Completing an expression (or a `_` typed hole) runs term search, whose
`LookupTable::find` calls `Type::could_unify_with_deeply`, which calls
`self.owner.must_unify(other.owner)`. The lookup table mixes `Type`s from
different `TypeOwnerId`s (here a `static` and a function), which do not unify,
tripping `expect("failed to unify type owners")`. Fires from **completions** —
a passive, every-keystroke feature — hence Critical.

### 4. Layout — stack overflow on polymorphic recursion (`layout_of_adt`)
Repro: `zztemp_recursive_generic_stack_overflow` (aborts the process).

```rust
struct A<T>(B<fn(T)>);
struct B<T>(A<fn(T)>);
fn f(a: A<i32>) { a }
```

Hovering computes the memory layout of `A<i32>`, which needs the layout of
field `B<fn(i32)>`, which needs `A<fn(fn(i32))>`, … — each monomorphization is
a **distinct type**, so salsa's cycle detection never triggers (it only
catches identical query keys), and `layout_of_adt_query`/`layout_of_ty_query`
recurse until the stack overflows. Unlike every other finding here, a stack
overflow **cannot be caught** by the server's per-request `catch_unwind` — it
takes down the whole language server. gdb backtrace confirms the
`layout_of_adt_query ↔ layout_of_ty_query` recursion.

### 5. Rename `self`→param — overlapping edits (`source_change.rs:80`)
`overlapping edits for same file`
Repro: `zztemp_rename_self_chained_method_calls`.

```rust
struct S;
impl S { fn s(&self) -> S { S } }   // rename `self` -> `this`
fn f(s: S) { s.s().s(); }
```

`rename_self_to_param` → `transform_method_call_into_assoc_fn` rewrites each
call site; for chained calls the rewrite of the inner `s.s()` and the outer
`s.s().s()` overlap, tripping `never!(union(edit).is_err())`. In release the
second edit is silently dropped → a **wrong rename** on completely ordinary
code. Rename is user-invoked but common, and the corruption is silent.

### 6. `unwrap_option/result_return_type` — `unwrap_return_type.rs:120`
Repro: `crates/ide-assists/.../unwrap_return_type.rs::zztemp_unwrap_option_unit_match_tail`.

```rust
fn foo() -> Option<()> { match 0 { _ => Some(()), } }
```

For unit-wrapped types each tail expr's parent is cast to
`Either<ReturnExpr, StmtList>` and unwrapped, but `for_each_tail_expr` also
yields `match`-arm tails whose parent is a `MatchArm`. **Complete, compiling
code.**

### 7. `flip_comma` — `flip_comma.rs:82`
Repro: `flip_comma.rs::zztemp_flip_comma_attribute_leading_comma`.
`#[repr(,​ C)] struct Foo;` — the trimming logic unwraps `position()` on a
slice holding only the `(` delimiter.

### 8. `remove_unused_param` — `remove_unused_param.rs:197`
Repro: `remove_unused_param.rs::zztemp_remove_unused_param_unclosed_param_list`
(and `…_unclosed_call`). Unclosed `fn foo(x: i32,` or `foo(1,`: the comma is
the last sibling, `nth(1).unwrap()` fails.

### 9–11. `generate_getter/setter`, `generate_enum_is_method`, `generate_enum_projection_method` — `assoc_item_list().unwrap()`
Repros: `zztemp_generate_getter_existing_impl_without_body`,
`zztemp_generate_enum_is_existing_impl_without_body`,
`zztemp_generate_enum_try_into_existing_impl_without_body`. A just-typed
`impl S` / `impl E` with no `{}` yet: `utils::find_struct_impl` returns it,
all three then unwrap its missing assoc-item list.

### 12. `extract_function` — `edit_algo.rs:297` `unimplemented!`
Repro: `extract_function.rs::zztemp_extract_function_selection_spanning_fn_header`.
`fn f() { l<sel>et x =` — the assist emits a `ReplaceWithMany`/`ReplaceAll`
that another change depends on, unsupported by the dependent-change resolver.

### 13. `extract_variable` — `cover_edit_range` "Bad range"
Repro: `extract_variable.rs::zztemp_extract_variable_unresolved_macro_paren`.
`fn f() { m!( }` selecting the `(`: descending tokens into an unresolved macro
yields an `original_range` not contained in the local node passed to
`cover_edit_range`, tripping `covering_element`'s containment assert.

### 14. `replace_let_with_if_let` — `expr_if` unwraps
Repro: `replace_let_with_if_let.rs::zztemp_replace_let_unclosed_string_literal`.
`let s = "` — the unterminated string swallows the generated `if let`
template, so the reparsed shape lacks the expected children.

### 15. `add_hash` (raw strings) — `make.rs:601` assert
Repro: `raw_string.rs::zztemp_add_hash_half_typed_raw_ident`.
`let r#  = 1;` — the half-typed raw ident lexes with trailing whitespace,
tripping `make::expr_literal`'s `text.trim() == text`.

### 16. `generate_enum_is_method` — `never!(variants.is_empty())` (`:59`)
Discovered by the assists harness (selection covering only the comma between
enum variants makes the variant filter match nothing).

---

## Unicode / offset edges

The ide harness includes multibyte, combining-mark, RTL, ZWJ, emoji-in-ident,
BOM, and raw-string-with-emoji snippets, exercised at every **char-boundary**
offset. No unicode-specific panic surfaced beyond the offset-contract issue
(#1), which is encoding-agnostic. This is a mild positive result: the
`is_char_boundary` discipline in the converters holds for in-bounds offsets;
the crashes come from *out-of-bounds* offsets, not multibyte handling.

## Attempted but NOT reproduced

- **salsa `Can't merge cycle heads … variances_of` with different iteration
  counts** (`salsa/cycle.rs:340`). This is a fixpoint-iteration robustness
  assertion in the `variances_of` cyclic query; reproducing it needs a
  specific interleaving of *nested* variance cycles reaching a shared head at
  different iteration counts. Seven mutually-recursive-generic ADT webs
  (invariant via `*mut`/`fn`, with lifetime subtyping to force
  variance-aware relation) were tried; none forced the nested interleaving.
  Left as an open upstream-fragility item.
- `field FieldIndex(...) out of range` / `can't project out of {type error}`
  as exact strings — not present verbatim in this pinned `rustc_*`/next-solver
  version; the **same class** is covered by finding #2 (MIR projection index
  assumed in bounds).

## Candidates investigated and found guarded (not bugs)

`unmerge_match_arm.rs:94` (guarded by earlier `expr()?`), `convert_closure_to_fn`
text slicing (guarded by `ends_with(')')` + empty-captures return),
`typing.rs:359` `on_dot_typed` (whitespace token ends at the dot),
`goto_definition.rs:630` (`find_loops` returns only labeled blocks),
`nameres/collector.rs:1605` (push precedes `last_mut`), `mbe/parser.rs:308`
(punct pre-peeked), `expr_store/lower.rs:393` (`unreachable!` re-checks the
guard). Also robust under the harnesses: glob/`include!`/`#[path]` cycles,
unbounded macro recursion, same-type recursive structs (`struct R { r: R }`),
`Deref` cycles, const-initializer cycles, and generic-arg kind mismatches.

## Methodology note

Fuzzing the IDE entrypoints over an unfinished-code corpus (the two harnesses)
was markedly more productive than static `.unwrap()` auditing: it found
extract_function/extract_variable/add_hash/replace_let/rename/term-search/MIR/
layout crashes that a grep-and-reason pass missed or misjudged, and it filtered
out the many guarded `.unwrap()`s that looked dangerous but aren't reachable.
Recommended as a standing regression harness once these are fixed.
