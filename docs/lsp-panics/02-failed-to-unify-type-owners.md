# `request handler panicked: failed to unify type owners`

## Symptom

LSP requests — most likely `textDocument/completion` — fail with:

```
request handler panicked: failed to unify type owners
```

## Where the panic fires

`crates/hir/src/lib.rs`:

```rust
// ~line 5192
enum TypeOwnerId {
    GenericDefId(GenericDefId),
    BuiltinDeriveImplId(BuiltinDeriveImplId),
    AnonConstId(AnonConstId),
    NoParams(base_db::Crate),
}

impl TypeOwnerId {
    fn unify(self, other: Self) -> Option<Self> {
        // NoParams unifies with anything; otherwise owners must be EQUAL
    }

    #[track_caller]
    fn must_unify(self, other: Self) -> Self {   // line ~5218
        self.unify(other).expect("failed to unify type owners")
    }
}
```

Every `hir::Type<'db>` carries a `TypeOwnerId`: the definition whose generic
parameters the type's `Param`s are numbered against. `must_unify` panics
whenever two `Type`s with **different real owners** are combined — even if
both types are fully concrete (no params at all): `must_unify` does not
check `has_param()`.

Panic call sites (all in `crates/hir/src/lib.rs`):

- `Type::could_unify_with` (~6574): `self.owner.must_unify(other.owner)`
- `Type::could_unify_with_deeply` (~6589)
- `Type::could_coerce_to` (~6600)
- `Type::new_tuple` (~5502) and `generic_args_from_tys` (~7426): fold the
  owners of all argument types with `must_unify`

## Why owners get mixed

Where owners come from:

- body-derived types (`Semantics::type_of_expr`, `Local::ty`, …) →
  the enclosing body's `GenericDefId` (`Type::new_body`)
- `Field::ty` → the **ADT**'s def; `TypeParam::ty` → the param's **parent**
  def; `Function::ret_type` → the **function**'s def
- `Type::no_params` / `unknown` → `NoParams` (unifies with everything)

The intended guards are the rebase helpers (`try_rebase_into`,
`rebase_into_or_error`, `instantiate_with_errors` — failure degrades to
`NoParams`) and `instantiate`, which folds the result owner from the
argument types. Most `could_unify*` callers in ide crates go through one of
these. **Term search does not**:

- `crates/hir/src/term_search.rs:117` (`LookupTable::find`), `:142` and
  `:148` (`find_autoref`):

  ```rust
  .find(|(t, _)| t.could_unify_with_deeply(db, ty))
  ```

  `t` iterates lookup-table entries whose owners are whatever item they were
  harvested from (statics, consts, functions, ADT fields across the module);
  `ty` is the query type. Any owner mismatch between an entry and the query
  panics.

- `crates/hir/src/term_search/tactics.rs:361`:

  ```rust
  it.ty(db).could_unify_with(db, &generic)
  ```

  compares `TypeParam::ty` (owner = the param's parent impl/function, and it
  genuinely contains a `Param`) against an arbitrary candidate type. Several
  other tactics call sites are protected by `.instantiate_with_errors()` on
  one operand (e.g. tactics.rs:85, :135, :286), which is why the panic is
  intermittent rather than constant.

Term search runs from three LSP surfaces:

- **expression completions** when `enable_term_search` is on
  (`ide-completion/src/completions/expr.rs` ~:492)
- the **typed-hole diagnostic** quickfix
  (`ide-diagnostics/src/handlers/typed_hole.rs` ~:65)
- the **term-search assist** (`ide-assists/src/handlers/term_search.rs`)

Completions make this effectively an every-keystroke panic risk in projects
where the lookup table gathers items from more than one owner — i.e. almost
any real module.

A secondary candidate with the same shape: completion relevance
`match_types` (`ide-completion/src/render.rs` ~:671) compares
`ctx.expected_type` (rebased into the scope of the *speculative* fake-ident
parse) with candidates rebased into `ctx.scope.generic_def()` (from the
*original* parse); if those two scopes resolve to different generic-owning
items, two successful rebases still yield two different real owners.

## Verified reproduction

`cargo test -p ide --lib lsp_error_repros::term_search_unify_type_owners`
(`crates/ide/src/lsp_error_repros.rs`), exact panic message asserted:

```rust
static X: i32 = 5;
const Y: u32 = 1;
fn g(x: i32) -> u32 { 0 /* request completions here */ }
fn f() -> u32 { g(_) }
```

Requesting completions (with `enable_term_search: true`, the default config
shape) runs term search; the lookup table mixes `Type`s owned by the
`static`, the `const`, and the functions → `must_unify` panics.

## Fix directions

1. **Make the comparison APIs total** (preferred): in
   `Type::could_unify_with`, `could_unify_with_deeply`, and
   `could_coerce_to`, replace `must_unify` with `unify`; on owner mismatch,
   either return `false`, or — better — rebase whichever side has no params
   into the other owner (a param-free type is owner-independent) and only
   return `false` when *both* sides carry params of different owners.
   Reserve panicking for genuinely internal invariants
   (`new_tuple`/`generic_args_from_tys` may keep `must_unify`, or degrade
   to `NoParams` + `Ty::new_error` like `instantiate_with_errors` does).
2. **Fix term search's inputs**: normalize every type entering
   `LookupTable` (and every query type) with `instantiate_with_errors()` /
   an explicit rebase to a single owner, mirroring what tactics.rs already
   does at :85/:135/:286. This fixes the reproduced path but leaves the API
   booby-trapped for the next caller.
3. Audit remaining `could_unify*`/`could_coerce_to` callers
   (`ide-completion/src/render.rs`, `ide-assists/src/utils.rs:867`,
   `wrap_return_type.rs:114`, `add_missing_match_arms.rs:517`) once the API
   is total; also note `add_missing_match_arms.rs:97` has a separate
   `try_rebase_into_owner(..).unwrap()` that can panic on its own.

Semantic caution for option 1: `could_unify` drives completion relevance
and diagnostics suggestions; silently returning `false` for mixed owners is
exactly the old (pre-panic) behavior callers already expect from
non-unifying types, so this should be low-risk, but check completion
relevance snapshots.

## Acceptance criteria

- `lsp_error_repros::term_search_unify_type_owners` no longer panics: flip
  its `#[should_panic]` into a plain test asserting completions return
  (contents may vary).
- No remaining `must_unify` reachable from an `Analysis`-level entry point
  with attacker-free ordinary inputs (grep callers of `must_unify` and
  check each is either internal or preceded by a rebase).

## Key files

- `crates/hir/src/lib.rs`: `TypeOwnerId` (~5192), `unify`/`must_unify`
  (~5203/5218), `can_rebase_into` (~5222), `could_unify_with` (~6574),
  `could_unify_with_deeply` (~6589), `could_coerce_to` (~6600),
  `new_tuple` (~5502), `generic_args_from_tys` (~7426)
- `crates/hir/src/term_search.rs:113-150` (`LookupTable::find*`)
- `crates/hir/src/term_search/tactics.rs:361` (+ the protected sites at
  :85/:135/:286 as the pattern to copy)
- `crates/ide-completion/src/completions/expr.rs` (~:492),
  `crates/ide-diagnostics/src/handlers/typed_hole.rs` (~:65) — LSP surfaces
- `crates/ide/src/lsp_error_repros.rs` — reproduction
