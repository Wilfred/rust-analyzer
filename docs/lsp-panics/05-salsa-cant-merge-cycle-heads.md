# `assertion failed: Can't merge cycle heads ... variances_of_query(...)`

## Symptom

Requests fail (or the server logs a worker panic) with:

```
request handler panicked: assertion `left == right` failed: Can't merge cycle heads variances_of_query(Id(...)) with different iterations (IterationCount(i), IterationCount(j))
```

Unlike issues 01-04 this is not rust-analyzer application code: it is an
internal invariant of **salsa's fixpoint-cycle machinery**, hit through
rust-analyzer's `variances_of` query.

## Where the panic fires

salsa 0.27.2 (pinned in `Cargo.lock`), `src/cycle.rs` `CycleHeads::insert`
(~line 346):

```rust
assert_eq!(
    existing_iteration, iteration,
    "Can't merge cycle heads {:?} with different iterations ({existing_iteration:?}, {iteration:?})",
    existing.database_key_index
);
```

The `{:?}` of the `DatabaseKeyIndex` prints the query name —
`variances_of_query` — which points at
`crates/hir-ty/src/variance.rs`:

```rust
#[salsa::tracked(
    returns(ref),
    cycle_fn = crate::variance::variances_of_cycle_fn,        // returns new value each iteration
    cycle_initial = crate::variance::variances_of_cycle_initial, // seeds Bivariant
)]
fn variances_of_query(db: &dyn HirDatabase, def: GenericDefId) -> StoredVariancesOf { ... }
```

`variances_of` is rust-analyzer's **only** fixpoint-iterating salsa query
(`cycle_fn` + `cycle_initial` ⇒ `CycleRecoveryStrategy::Fixpoint`; every
other cycle-recoverable query in the codebase uses `cycle_result` /
immediate fallback — grep `cycle_fn|cycle_initial|cycle_result` under
`crates/hir-ty/`). The module's own doc comment (variance.rs lines 7-14)
flags salsa cycle handling as the risk area.

## Why cycles happen

Variance computation is a fixpoint over the ADT dependency graph:
`variances_of(X)` walks X's field types and re-enters `variances_of` for
every ADT mentioned (`add_constraints_from_ty` → `TyKind::Adt` →
`add_constraints_from_args` → `self.db.variances_of(def_id)` at
variance.rs ~304). Any mutually recursive generic ADTs create a query
cycle, e.g.:

```rust
struct A<T>(B<T>);
struct B<T>(fn(A<T>));   // fn(..) flips to contravariance → multiple iterations
```

Real-world code has such webs everywhere (ASTs, graphs, futures).

## Mechanism of the assertion (from salsa 0.26.2/0.27.2 source)

Salsa resolves fixpoint cycles by iterating the SCC, stamping each
provisional memo with an **iteration count** per cycle head. Two places
merge head-sets:

- `ActiveQuery::add_read` → `CycleHeads::extend` — a query unions the cycle
  heads of every provisional dependency it reads;
- `collect_all_cycle_heads` (`function/execute.rs`) — after executing a
  fixpoint query, salsa walks all reachable provisional memos and collects
  their `(head, iteration_count)` pairs.

The assert fires when the same head is present **twice, live, with
different iteration counts** — i.e. a provisional memo stamped at iteration
*i* survived while another copy advanced to *i+1*. Salsa's bookkeeping
(`max_iteration_count` adoption, `remove_all_except`,
`update_iteration_count`) is supposed to keep all live copies in sync;
the ingredients for breaking it are:

- **cross-thread head-lock transfer** (`outer_cycle` /
  `ReleaseMode::TransferTo` in execute.rs) while another thread reuses a
  provisional memo from an earlier iteration
  (`validate_same_iteration` / `fetch_cold_cycle` reuse paths);
- **cancellation / panic** mid-iteration leaving provisional memos behind
  (`PoisonProvisionalIfPanicking` shows salsa knows they can linger);
- **nested overlapping cycles** converging at different iterations.

rust-analyzer's runtime is exactly this environment: diagnostics and
prime-caches evaluate queries on many worker threads, and every keystroke
cancels in-flight work.

Salsa ships a dedicated parallel regression test for this shape:
`salsa-0.27.2/tests/parallel/cycle_iteration_mismatch.rs` — six queries
A-F forming overlapping cycles, raced from 4 threads — driven by the
**shuttle** deterministic-interleaving explorer (`crate::sync::check`).
That is what it takes to pin the race; plain OS scheduling hits it rarely.

## Reproduction status

`crates/hir-ty/src/tests/panic_repros.rs::parallel_variances_cycle_heads`
(`#[ignore]`d soak test; run with
`VARIANCE_ROUNDS=10000 cargo test -p hir-ty --lib parallel_variances_cycle_heads -- --ignored`).

It mirrors salsa's regression-test dependency graph as six mutually
recursive generic ADTs (with `fn(..)` contravariant and `*mut` invariant
edges so the fixpoint takes several iterations):

```rust
struct A<T>(B<T>, C<T>);
struct B<T>(fn(A<C<T>>));
struct C<T>(A<T>, *mut E<T>);
struct D<T>(E<T>);
struct E<T>(fn(D<T>), F<T>);
struct F<T>(B<T>, *mut E<T>);
```

and evaluates `variances_of` for A, D, F, B, E, C (doubled up) from 8
barrier-synchronized, half-staggered threads, on a fresh database per
round.

Result so far: the exact `Can't merge cycle heads` assert has **not**
fired under plain OS scheduling, but roughly once per few thousand rounds
(8-core container) the soak trips a **sibling invariant in the same salsa
parallel-cycle machinery**:

```
panicked at salsa-0.27.2/src/runtime/dependency_graph.rs:422:
Circular reference between blocked edges: Edges({ThreadId(254): Edge { blocked_on_id: ThreadId(257), .. }, ...})
```

That independently confirms parallel `variances_of` cycles violate salsa's
blocking/merging invariants; the cycle-heads assert is one unlucky
interleaving away in the same subsystem.

## Investigation / fix directions

This must land upstream in salsa; rust-analyzer-side work is mitigation.

1. **Upstream report**: file both crashes against salsa
   (`CycleHeads::insert` iterations mismatch and the
   `dependency_graph.rs:422` circular-blocked-edges panic) with the soak
   test's workload description. Check salsa releases/commits after 0.27.2
   for fixes to `collect_all_cycle_heads`, `CycleHeads::insert`,
   `validate_same_iteration`, and the dependency-graph edge transfer; try
   simply bumping the salsa dependency and re-running the soak.
2. **Deterministic repro** (what an investigating LLM should build): port
   the six-ADT workload into salsa's own test suite as a
   shuttle-scheduled test (copy the harness of
   `tests/parallel/cycle_iteration_mismatch.rs` / `cycle_nested_deep.rs`,
   which use `crate::sync::check` + Knobs signalling). Shuttle explores
   interleavings exhaustively and will either pin the assert or prove the
   invariant holds for this shape.
3. **rust-analyzer mitigation options** while upstream is unfixed:
   - catch the panic at the query boundary the way cancellation is caught
     (rust-analyzer already special-cases salsa cycle unwinds in
     `thread_result_to_response`, see PR #19888) — risky, since a broken
     memo table may poison the revision;
   - reduce exposure by computing `variances_of` non-cyclically (rustc
     solves variance per crate-level SCC in one pass rather than per-item
     fixpoint; `crates/hir-ty/src/variance.rs`'s doc comment already
     discusses this trade-off).
4. Keep the soak in CI-adjacent tooling: it currently catches *any*
   regression in salsa's parallel cycle handling within minutes of soak
   time.

## Acceptance criteria

- The soak (`VARIANCE_ROUNDS=10000`, repeated ~10×) no longer produces
  either the `Circular reference between blocked edges` panic or the
  `Can't merge cycle heads` assert after the salsa fix/bump.
- Ideally: a deterministic shuttle test upstream in salsa covering the
  six-node two-cycle workload.

## Key files

- `crates/hir-ty/src/variance.rs` (fixpoint query: lines ~33-112; recursive
  re-entry ~304; doc-comment risk note lines 7-14)
- salsa 0.27.2 (registry sources): `src/cycle.rs` (`CycleHeads::insert`,
  assert ~346), `src/function/execute.rs` (`collect_all_cycle_heads`,
  iteration bookkeeping), `src/function/maybe_changed_after.rs`
  (`validate_same_iteration`), `src/function/fetch.rs` (`fetch_cold_cycle`),
  `src/runtime/dependency_graph.rs:422` (the reproduced sibling panic)
- salsa regression tests to copy: `tests/parallel/cycle_iteration_mismatch.rs`,
  `tests/parallel/cycle_nested_*.rs`
- `crates/hir-ty/src/tests/panic_repros.rs::parallel_variances_cycle_heads`
  (the soak)
- rust-analyzer PR #19888 (salsa cycle unwinds in `thread_result_to_response`)
