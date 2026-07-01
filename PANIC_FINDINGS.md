# Panics reachable from legitimate intermediate states of user code

This document reports panics in rust-analyzer that can be triggered by code
states a user passes through while typing (unclosed delimiters, half-typed
literals, missing bodies), plus two triggered by ordinary complete code.
Every finding below is **verified by an executable reproduction** in this
branch: all tests prefixed `zztemp_` are intentional-failure repros
(`cargo test -p ide-assists zztemp`, `cargo test -p ide zztemp`), and two
brute-force harnesses re-discover all of them from scratch:

- `crates/ide-assists/src/tests.rs` — `zztemp_brute_force_incomplete_code`:
  runs *all* assists with `AssistResolveStrategy::All` over every selection
  range (all char-boundary pairs) of ~55 unfinished-code snippets.
- `crates/ide/src/zztemp_incomplete.rs` — `zztemp_ide_brute_force_incomplete_code`:
  runs hover, goto-def, completions, signature help, diagnostics (with fixes),
  inlay hints, highlight, rename, find-refs, join-lines, call hierarchy,
  on-char-typed, etc. at every offset of ~85 snippets, including cyclic
  definitions and multi-file cycles.

## Hard panics (crash in release builds)

### 1. `flip_comma` — `flip_comma.rs:82`
`called Option::unwrap() on a None value`

```rust
#[repr(,$0 C)] struct Foo;      // also: #[repr(, ,$0 C)] struct Foo;
```

`flip_tree` guards only that the comma has non-trivia siblings and that the
*next* one isn't punctuation. With a leading comma in an attribute token
tree, `before` contains only the `(` delimiter, so
`before[prev_start_untrimmed..prev_end].iter().position(not_ws)` is `None`.
Runs in the assist-resolution closure, i.e. when the assist is rendered.

### 2. `unwrap_option_return_type` / `unwrap_result_return_type` — `unwrap_return_type.rs:120`
`called Option::unwrap() on a None value`

```rust
//- minicore: option
fn foo() -> Option<()>$0 {
    match 0 { _ => Some(()), }
}
```

For unit wrapped types the code casts each tail expression's parent to
`Either<ast::ReturnExpr, ast::StmtList>` and unwraps. `for_each_tail_expr`
also yields tail expressions of `match` arms, whose parent is a
`MatchArm`. Note this is *complete* code, not even mid-edit.

### 3–5. `generate_getter`/`generate_setter`, `generate_enum_is_method`, `generate_enum_projection_method` — existing `impl` without a body
`called Option::unwrap() on a None value` at
`generate_getter_or_setter.rs:443`, `generate_enum_is_method.rs:80`,
`generate_enum_projection_method.rs:159`

```rust
struct S { fie$0ld: i32 }
impl S              // just typed, no `{}` yet
```

```rust
enum Variant { Undefined, Minor$0, Major }
impl Variant
```

`utils::find_struct_impl` happily returns an `ast::Impl` that has no
`assoc_item_list()` (its helper `has_any_fn` even handles that case), but
all three generators then do `impl_def.assoc_item_list().unwrap()`.

### 6. `remove_unused_param` — `remove_unused_param.rs:197`
`called Option::unwrap() on a None value`

```rust
fn foo(x$0: i32,
```
and independently at the call-site rewrite:
```rust
fn foo(x$0: i32) {}
fn main() {
    foo(1,
}
```

`elements_to_remove` finds the comma following the parameter/argument and
assumes something follows it: `token.siblings_with_tokens(dir).nth(1).unwrap()`.
In an unclosed param list/arg list the comma is the last element.

### 7. `replace_let_with_if_let` — `syntax_factory/constructors.rs:1303` (`expr_if`)
`called Option::unwrap() on a None value`

```rust
fn f() { $0let s = "
```

The assist builds `if let <pat> = <initializer>` textually. The unterminated
string literal swallows the rest of the template, so the reparsed `IfExpr`
lacks the expected children and `ast.condition().unwrap()` /
`then_branch().unwrap()` in the mapping builder fails.

### 8. `add_hash` (raw strings) — `make.rs:601` (`expr_literal`)
`assertion left == right failed: "r###" vs "r### "`

```rust
fn f() { let $0r#  = 1; }       // user is mid-way through typing a raw ident / raw string
```

The half-typed `r#` lexes as an error token whose text includes trailing
whitespace; `add_hash` feeds `{token_text}#` into `make::expr_literal`,
which asserts `text.trim() == text`.

### 9. `extract_function` — `syntax_editor/edit_algo.rs:297`
`not implemented: cannot resolve changes that depend on replacing many elements`

```rust
fn f()$0 { l$0et x =          // selection spans the fn header/body boundary
```

With this selection over incomplete code the assist produces a
`ReplaceWithMany`/`ReplaceAll` change that another change depends on, which
the syntax editor's dependent-change resolution explicitly does not support.

### 10. `extract_variable` — rowan `cursor.rs:790` via `utils::cover_edit_range`
`Bad range: node range 11..12, range 9..12`

```rust
fn f() { m!$0($0 }             // `m!` is unresolved; selection covers the `(`
```

For a selection inside a macro-call token tree, `extract_variable` descends
tokens into macro expansions and calls
`ctx.sema.original_range(expr)`; the resulting range is not contained in the
local node passed to `cover_edit_range`, and
`SyntaxNode::covering_element` asserts containment.

## Soft panics (`stdx::never!` — panic in dev/debug builds, silent misbehavior in release)

### 11. `generate_enum_is_method` — `generate_enum_is_method.rs:59`
`assertion failed: !variants.is_empty()`

```rust
enum E { A,$0 B }               // selection covering only the comma (offsets 10..11)
```

The variant filter `is_selected` matches no variant when the selection
covers only the separator, violating the `never!(variants.is_empty())`
invariant.

### 12. Rename `self` to a named parameter — `ide-db/source_change.rs:80`
`overlapping edits for same file`

```rust
struct S;
impl S { fn s(&$0self) -> S { S } }   // rename `self` -> `this`
fn f(s: S) { s.s().s(); }
```

`rename_self_to_param` → `transform_method_call_into_assoc_fn` rewrites each
call site of the method; for chained calls the rewrite of `s.s()` (receiver
of the outer call) and of `s.s().s()` overlap, tripping
`never!(value.0.union(edit).is_err())`. In release the second edit is
silently dropped, producing a wrong rename. This is fully complete code.

## Candidates investigated and found NOT reachable (guarded)

- `unmerge_match_arm.rs:94` — guarded by `match_arm.expr()?` earlier.
- `convert_closure_to_fn.rs:624/628/639` — guarded by `ends_with(')')`
  check and the `captures_as_args.is_empty()` early return.
- `typing.rs:359` (`on_dot_typed`) — the whitespace token ends exactly at the
  typed dot, so `offset - current_indent_len` cannot underflow.
- `goto_definition.rs:630` — `find_loops` only returns labeled blocks.
- `nameres/collector.rs:1605` — `derive_call_ids.push(None)` precedes the
  `last_mut().unwrap()` in the same iteration.
- `mbe/parser.rs:308` — a punct was already peeked, `expect_glued_punct`
  cannot fail.
- `expr_store/lower.rs:393` — `unreachable!` re-checks the same tokens as the
  guarding condition.

Also probed with the harnesses and found robust: glob-import cycles,
`include!` self/mutual cycles, `#[path]` self-module, unbounded macro
recursion, type-alias cycles, recursive struct layout (hover memory layout),
`Deref` cycles, const-initializer cycles, generic arg kind mismatches
(`S::<5>` where a type is expected), and all corpus snippets against hover /
completions / diagnostics-with-fixes / inlay hints / goto / signature help.

## Note on methodology

Five parallel searches over `ide-assists`, `ide`/`ide-completion`,
`hir-def`/`hir-expand`/`mbe`, `hir-ty`, and `syntax`/`ide-db` produced ~50
candidates; each was then either traced to a guard (list above) or confirmed
with a failing test. The brute-force harnesses independently re-discovered
every confirmed assist panic and found several the targeted search missed
(extract_function, extract_variable, add_hash, replace_let_with_if_let,
rename-self), suggesting fuzzing IDE entry points over an unfinished-code
corpus is a productive regression strategy for this class of bug.
