//! Reproductions for panics observed in the wild in LSP request handlers
//! ("request handler panicked: ..."). Each test is an intentional crash,
//! asserted via `#[should_panic]`; see docs/lsp-panics/ for the analysis.

use ide_db::ra_fixture::RaFixtureConfig;
use syntax::{TextRange, TextSize};

use crate::{
    CallableSnippets, CompletionConfig, CompletionFieldsToResolve, FilePosition, FileRange,
    fixture,
};
use hir::PrefixKind;
use ide_db::SnippetCap;
use ide_db::imports::insert_use::{ImportGranularity, InsertUseConfig};

const COMPLETION_CONFIG: CompletionConfig<'_> = CompletionConfig {
    enable_postfix_completions: true,
    enable_imports_on_the_fly: true,
    enable_self_on_the_fly: true,
    enable_private_editable: false,
    enable_term_search: true,
    term_search_fuel: 200,
    full_function_signatures: false,
    callable: Some(CallableSnippets::FillArguments),
    add_colons_to_module: true,
    add_semicolon_to_unit: true,
    snippet_cap: SnippetCap::new(true),
    insert_use: InsertUseConfig {
        granularity: ImportGranularity::Crate,
        prefix_kind: PrefixKind::Plain,
        enforce_granularity: true,
        group: true,
        skip_glob_imports: true,
    },
    prefer_no_std: false,
    prefer_prelude: true,
    prefer_absolute: false,
    snippets: Vec::new(),
    limit: None,
    fields_to_resolve: CompletionFieldsToResolve::empty(),
    exclude_flyimport: vec![],
    exclude_traits: &[],
    enable_auto_await: true,
    enable_auto_iter: true,
    ra_fixture: RaFixtureConfig::default(),
};

/// "failed to unify type owners" (crates/hir/src/lib.rs `TypeOwnerId::must_unify`).
///
/// Completing an expression runs term search; its lookup table compares
/// `hir::Type`s originating from items with different `TypeOwnerId`s
/// (`could_unify_with_deeply` -> `must_unify`), which panics instead of
/// returning `false`.
#[test]
#[should_panic(expected = "failed to unify type owners")]
fn term_search_unify_type_owners() {
    let (analysis, position) = fixture::position(
        "static X: i32 = 5;\nconst Y: u32 = 1;\nfn g(x: i32) -> u32 { 0 $0}\nfn f() -> u32 { g(_) }",
    );
    let _ = analysis.completions(&COMPLETION_CONFIG, position, None);
}

/// "Bad offset"/"Bad range" (rowan `token_at_offset`/`covering_element`
/// asserts) when a position/range past EOF reaches the syntax tree.
///
/// `from_proto::offset` clamps columns and errors on out-of-range *lines*, so
/// a single stale request on the same file is usually caught. The remaining
/// in-the-wild paths hand the tree a range computed against different text:
/// stale resolve-request data, client/server version skew while a request is
/// retried, and macro span maps resolved through `hir_expand::db::resolve_span`
/// (which adds `anchor_offset` without clamping to the file length). This test
/// demonstrates the invariant violation at the `Analysis` layer, where all
/// those paths converge.
#[test]
fn out_of_bounds_offset_panics() {
    let (analysis, file_id) = fixture::file("fn f() { let x = 1; }\n");
    let len: u32 = analysis.file_text(file_id).unwrap().len() as u32;
    let off = TextSize::new(len + 2);
    let position = FilePosition { file_id, offset: off };
    let frange = FileRange { file_id, range: TextRange::empty(off) };

    let mut panics = Vec::new();
    macro_rules! probe {
        ($name:literal, $e:expr) => {
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = $e;
            }))
            .is_err()
            {
                panics.push($name);
            }
        };
    }
    probe!("completions", analysis.completions(&COMPLETION_CONFIG, position, None));
    probe!("extend_selection", analysis.extend_selection(frange));
    probe!("matching_brace", analysis.matching_brace(position));

    assert!(
        !panics.is_empty(),
        "expected at least one feature to panic on an out-of-bounds offset"
    );
    eprintln!("panicking features: {panics:?}");
}
