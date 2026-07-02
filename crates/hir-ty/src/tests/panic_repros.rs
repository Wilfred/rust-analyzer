//! Reproductions for panics observed in the wild in LSP request handlers
//! ("request handler panicked: ..."). Each reproduction is an intentional
//! crash, asserted via `#[should_panic]`; see docs/lsp-panics/ for the
//! per-issue analysis.
//!
//! All of the `PlaceTy::field_ty` panics below fire from `db.borrowck(..)`,
//! which `hir::DefWithBody::diagnostics` runs for *every body* when computing
//! file diagnostics — no user action beyond opening the file is required.

use hir_def::{AdtId, DefWithBodyId, GenericDefId, ModuleDefId};
use test_fixture::WithFixture;

use crate::{db::HirDatabase, test_db::TestDB, tests::visit_module};

fn borrowck_all(ra_fixture: &str) {
    let (db, files) = TestDB::with_many_files(ra_fixture);
    crate::attach_db(&db, || borrowck_all_inner(&db, files));
}

fn borrowck_all_inner(db: &TestDB, files: Vec<base_db::EditionedFileId>) {
    for file_id in files {
        let module = match db.module_for_file_opt(file_id.file_id(db)) {
            Some(it) => it,
            None => continue,
        };
        let def_map = module.def_map(db);
        visit_module(db, def_map, module, &mut |it| {
            let def: DefWithBodyId = match it {
                ModuleDefId::FunctionId(it) => it.into(),
                ModuleDefId::ConstId(it) => it.into(),
                ModuleDefId::StaticId(it) => it.into(),
                ModuleDefId::EnumVariantId(it) => it.into(),
                _ => return,
            };
            let _ = db.borrowck(def.into());
        });
    }
}

// ---------------------------------------------------------------------------
// "field FieldIndex(_) out of range: ..." — crates/hir-ty/src/mir.rs,
// `PlaceTy::field_ty`, tuple arm.
//
// A tuple field access whose index is out of bounds still gets lowered to a
// MIR `Field` projection (tuple indices need no `field_resolution` and the
// bad index does not mark the body as having type mismatches), so borrowck's
// place-type recomputation indexes past the tuple arity.
// ---------------------------------------------------------------------------

#[test]
#[should_panic(expected = "field FieldIndex(1) out of range: (i32,)")]
fn out_of_bounds_tuple_field_literal() {
    borrowck_all(
        r#"
//- minicore: sized
fn f() {
    let t = (1,);
    let x = t.1;
}
"#,
    );
}

/// Same, with a reference + undeclared lifetime as the first tuple element so
/// the panic message reads `... out of range: (&'{region error} str, u32)`,
/// matching the message shape observed in the wild.
#[test]
#[should_panic(expected = "field FieldIndex(99) out of range: (&'{region error} str, u32)")]
fn out_of_bounds_tuple_field_region_error() {
    borrowck_all(
        r#"
//- minicore: sized
fn f(s: &'missing str) {
    let t = (s, 1u32);
    let x = t.99;
}
"#,
    );
}

// ---------------------------------------------------------------------------
// "can't project out of ..." — crates/hir-ty/src/mir.rs, `PlaceTy::field_ty`,
// fallthrough arm.
//
// `projection_ty_core` only guards against a base type that is *already*
// `TyKind::Error`; when the base is a `TyKind::Alias` the `structurally_
// normalize` callback runs first and may *produce* `Ty::new_error` (failed
// normalization), a rigid alias, or an unconstrained infer var — all of which
// reach `field_ty` and hit the fallthrough panic.
// ---------------------------------------------------------------------------

/// The alias' self type is an unresolved path, so the parameter's type is
/// `<{error} as Tr>::A` (an Alias whose *top-level* kind is not Error, thus
/// not caught by the `is_ty_error` guard). Tuple-pattern destructuring emits
/// `Field` projections on it; borrowck's normalization fails and returns
/// `{type error}`. This is the exact "can't project out of {type error}"
/// message observed in the wild.
#[test]
#[should_panic(expected = "can't project out of {type error}")]
fn tuple_pattern_on_error_self_alias() {
    borrowck_all(
        r#"
//- minicore: sized
trait Tr { type A; }
fn f(x: <Missing as Tr>::A) {
    let (mut a, b) = x;
    a = 1;
}
"#,
    );
}

/// `T::A` bound only by `T: Tr` is a rigid alias: it cannot normalize to a
/// tuple, but the (wrong) tuple pattern still emits `Field` projections, and
/// inference does not record this as a type mismatch, so MIR is built.
#[test]
#[should_panic(expected = "can't project out of Alias")]
fn tuple_pattern_on_rigid_alias() {
    borrowck_all(
        r#"
//- minicore: sized
trait Tr { type A; }
fn f<T: Tr>(x: T::A) {
    let (mut a, b) = x;
    a = 1;
}
"#,
    );
}

/// Conflicting associated-type equality bounds make borrowck's normalization
/// ambiguous; `structurally_normalize_ty` then hands back an unconstrained
/// inference variable, which also lands in the fallthrough arm.
#[test]
#[should_panic(expected = "can't project out of ?0t")]
fn tuple_pattern_conflicting_alias_bounds() {
    borrowck_all(
        r#"
//- minicore: sized
trait Tr { type A; }
fn f<T: Tr<A = (u32, u32)> + Tr<A = (u32, u32, u32)>>(x: T::A) {
    let (mut a, b, c) = x;
    a = 1;
}
"#,
    );
}

/// A cyclic equality bound (`T: Tr<A = (T::A, u32)>`) has the same effect.
#[test]
#[should_panic(expected = "can't project out of")]
fn tuple_pattern_cyclic_alias_bound() {
    borrowck_all(
        r#"
//- minicore: sized
trait Tr { type A; }
fn f<T: Tr<A = (T::A, u32)>>(x: T::A) {
    let (mut a, b) = x;
    a = 1;
}
"#,
    );
}

// ---------------------------------------------------------------------------
// "Can't merge cycle heads variances_of_query(...)" (salsa fixpoint
// assertion, salsa src/cycle.rs `CycleHeads::insert`).
//
// The exact assertion is NOT yet reproduced, but this soak test does
// intermittently (roughly once per few thousand rounds on an 8-core machine)
// trip a *sibling* invariant in the same salsa parallel-cycle machinery:
//
//     panicked at salsa-0.27.2/src/runtime/dependency_graph.rs:422:
//     Circular reference between blocked edges: Edges({ThreadId(..): ...})
//
// `variances_of` is rust-analyzer's only fixpoint-cycle salsa query; the
// cycle-heads assertion fires when two live copies of the same cycle head
// carry different iteration counts, which salsa's own regression test
// (tests/parallel/cycle_iteration_mismatch.rs) drives with a deterministic
// interleaving explorer (shuttle). This test mirrors that test's dependency
// graph as mutually recursive generic ADTs and hammers it from 8 threads.
// Run with VARIANCE_ROUNDS=10000 for a longer soak.
// ---------------------------------------------------------------------------

#[test]
#[ignore = "nondeterministic soak test; fails intermittently with salsa parallel-cycle panics"]
fn parallel_variances_cycle_heads() {
    let fixture = r#"
//- minicore: sized
struct A<T>(B<T>, C<T>);
struct B<T>(fn(A<C<T>>));
struct C<T>(A<T>, *mut E<T>);
struct D<T>(E<T>);
struct E<T>(fn(D<T>), F<T>);
struct F<T>(B<T>, *mut E<T>);
"#;
    let rounds: usize =
        std::env::var("VARIANCE_ROUNDS").ok().and_then(|v| v.parse().ok()).unwrap_or(200);
    for _round in 0..rounds {
        let (db, files) = TestDB::with_many_files(fixture);
        let mut adts: Vec<GenericDefId> = Vec::new();
        crate::attach_db(&db, || {
            for &file_id in &files {
                let module = match db.module_for_file_opt(file_id.file_id(&db)) {
                    Some(it) => it,
                    None => continue,
                };
                let def_map = module.def_map(&db);
                visit_module(&db, def_map, module, &mut |it| {
                    if let ModuleDefId::AdtId(AdtId::StructId(id)) = it {
                        adts.push(GenericDefId::AdtId(AdtId::StructId(id)));
                    }
                });
            }
        });
        assert_eq!(adts.len(), 6);
        // Entry points chosen like salsa's test: A, D, F, B (doubled up for
        // more contention), synchronized with a barrier.
        let entries =
            [adts[0], adts[3], adts[5], adts[1], adts[4], adts[2], adts[0], adts[5]];
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(entries.len()));
        let handles: Vec<_> = entries
            .iter()
            .enumerate()
            .map(|(i, &def)| {
                let db = db.clone();
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    crate::attach_db(&db, || {
                        barrier.wait();
                        // Stagger half the threads so they join cycles that
                        // are already mid-iteration on other threads.
                        if i % 2 == 1 {
                            std::thread::sleep(std::time::Duration::from_micros(i as u64 * 10));
                        }
                        let _ = crate::variance::variances_of(&db, def);
                    });
                })
            })
            .collect();
        for h in handles {
            h.join().unwrap();
        }
    }
}
