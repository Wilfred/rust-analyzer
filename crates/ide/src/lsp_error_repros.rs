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

/// Brute-force probe for "unmatched closing delimiter from syntax fixup"
/// (crates/syntax-bridge/src/lib.rs:243): run diagnostics over macro calls
/// whose token trees contain incomplete syntax, so that syntax fixup's
/// appended delimiter punct leaves flow through `convert_tokens`.
#[test]
fn syntax_fixup_unmatched_closing_delimiter_probe() {
    let broken = [
        "if x {",
        "if x { else",
        "match x {",
        "match x { _ =>",
        "while x {",
        "loop {",
        "fn g(",
        "fn g( {",
        "let x = {",
        "struct S {",
        "impl T {",
        "x.  ",
        "if x {) ",
        "{ if x { } ",
        "if {x} {",
        "if x { } else {",
        "unsafe {",
        "|| {",
        "async {",
        "for i in x {",
        "if let Some(_) = x {",
        "x[0",
        "f(1,",
        "(a, b",
        "[1, 2",
    ];
    // Syntax fixup runs on attribute/derive macro *inputs*
    // (attr_macro_input_to_token_tree). The cfg_attr-expansion callback
    // additionally inserts raw `#`/`[`/`]` punct leaves, so malformed
    // cfg_attr is part of the corpus.
    let attr_variants = [
        "#[proc_macros::identity]",
        "#[cfg_attr(not(never), proc_macros::identity)]",
        "#[proc_macros::identity] #[cfg_attr(never, allow(unused))]",
        "#[cfg_attr(not(never), proc_macros::identity, allow(unused))]",
        "#[cfg_attr(not(never), proc_macros::identity,)]",
        "#[cfg_attr(not(never), proc_macros::identity) ]",
        "#[cfg_attr(not(never), proc_macros::identity]",
        "#[cfg_attr(not(never), proc_macros::identity(]",
        "#[cfg_attr(not(never), proc_macros::identity(broken(]",
        "#[cfg_attr] #[proc_macros::identity]",
        "#[cfg_attr(] #[proc_macros::identity]",
        "#[cfg_attr()] #[proc_macros::identity]",
        "#[cfg_attr(never)] #[proc_macros::identity]",
        "#[cfg_attr(not(never), cfg_attr(not(never), proc_macros::identity))]",
    ];
    // Targeted case: a cfg_attr on an element *inside a braced body* whose
    // meta contains a stray `}`. The cfg_attr reconstruction inserts leaf
    // `[`/`]` puncts; the stray `}` triggers the multi-close recovery which
    // sweeps the `[` subtree, orphaning the inserted `]`.
    let targeted = [
        r#"//- proc_macros: identity
#[proc_macros::identity]
fn f() {
    #[cfg_attr(not(never), allow(]))]
    let x = 1;
}
"#,
        r#"//- proc_macros: derive_identity
#[derive(proc_macros::DeriveIdentity)]
struct S {
    #[cfg_attr(not(never), allow(]))]
    field: u32,
}
"#,
        r#"//- proc_macros: identity
#[proc_macros::identity]
fn f() {
    #[cfg_attr(not(never), allow(unused, ]))]
    { }
}
"#,
    ];
    let mut panicked = Vec::new();
    for (i, src) in targeted.iter().enumerate() {
        let res = std::panic::catch_unwind(|| {
            let (analysis, file_id) = fixture::file(src);
            let _ = analysis.full_diagnostics(
                &crate::DiagnosticsConfig::test_sample(),
                crate::AssistResolveStrategy::All,
                file_id,
            );
        });
        if res.is_err() {
            panicked.push(("targeted", ["0", "1", "2"][i]));
        }
    }
    for attrs in attr_variants {
        for b in broken {
            let src = format!(
                r#"//- proc_macros: identity
{attrs}
fn f() {{ {b} }}

#[derive(proc_macros::DeriveIdentity)]
struct S {{
    #[cfg_attr(never, allow(unused))]
    field: ({b}),
}}
"#
            );
            let res = std::panic::catch_unwind(|| {
                let (analysis, file_id) = fixture::file(&src);
                let _ = analysis.full_diagnostics(
                    &crate::DiagnosticsConfig::test_sample(),
                    crate::AssistResolveStrategy::All,
                    file_id,
                );
            });
            if res.is_err() {
                panicked.push((attrs, b));
            }
        }
    }
    assert!(panicked.is_empty(), "panicking combinations: {panicked:#?}");
}

/// Probe for "used an incorrect AttrId" (crates/hir-expand/src/attrs.rs:400):
/// AttrIds index the cfg_attr-expanded attribute list, which depends on the
/// crate's cfg options, so the same file shared by two crates with different
/// cfgs yields different indices for the same attribute. Sweep IDE features
/// over every offset of the shared file looking for a resolve path that
/// mixes the crates up.
#[test]
fn incorrect_attr_id_probe() {
    let src = r#"
//- proc_macros: identity, derive_identity
//- /a.rs crate:a cfg:feature=on
#[path = "shared.rs"] mod shared;
//- /b.rs crate:b
#[path = "shared.rs"] mod shared;
//- /shared.rs
#[cfg_attr(feature = "on", some::unknown_attr)]
#[derive(proc_macros::DeriveIdentity)]
pub struct S;

#[cfg_attr(feature = "on", proc_macros::identity)]
pub fn f() { let s = S; }
"#;
    let mut host = crate::AnalysisHost::default();
    let change_fixture = test_fixture::ChangeFixture::parse(src);
    host.db.enable_proc_attr_macros();
    host.db.apply_change(change_fixture.change);
    let analysis = host.analysis();
    let files: Vec<crate::FileId> =
        change_fixture.files.iter().map(|it| it.file_id()).collect();

    let goto_config = crate::GotoDefinitionConfig { ra_fixture: RaFixtureConfig::default() };
    let hover_config = crate::HoverConfig {
        links_in_hover: true,
        memory_layout: None,
        documentation: true,
        format: crate::HoverDocFormat::Markdown,
        keywords: true,
        max_trait_assoc_items_count: None,
        max_fields_count: Some(5),
        max_enum_variants_count: Some(5),
        max_subst_ty_len: crate::SubstTyLen::Unlimited,
        show_drop_glue: true,
        ra_fixture: RaFixtureConfig::default(),
    };
    let mut panics = Vec::new();
    for &file_id in &files {
        let len: u32 = analysis.file_text(file_id).unwrap().len() as u32;
        for off in 0..len {
            let position = FilePosition { file_id, offset: TextSize::new(off) };
            let frange = FileRange { file_id, range: TextRange::empty(TextSize::new(off)) };
            let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = analysis.hover(&hover_config, frange);
                let _ = analysis.goto_definition(position, &goto_config);
            }));
            if r.is_err() {
                panics.push((file_id, off));
            }
        }
        let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = analysis.full_diagnostics(
                &crate::DiagnosticsConfig::test_sample(),
                crate::AssistResolveStrategy::All,
                file_id,
            );
        }));
        if r.is_err() {
            panics.push((file_id, u32::MAX));
        }
    }
    assert!(panics.is_empty(), "panics at: {panics:?}");
}

/// "used an incorrect AttrId" (crates/hir-expand/src/attrs.rs:400).
///
/// `AttrId`s index the cfg_attr-expanded attribute list of an item.
/// Completion inside attribute-macro arguments runs `expand_speculative`
/// (crates/hir-expand/src/db.rs:228), which resolves the stored AttrId via
/// `find_attr_range_with_source` against the *speculatively edited* copy of
/// the item. With the completion's dummy token spliced into the cfg_attr
/// predicate, the speculative copy's attribute walk no longer reaches the
/// minted index, and the resolution panics instead of degrading. Typing
/// inside a `cfg_attr` on any attribute-macro'd item hits this.
/// See docs/lsp-panics/06-used-an-incorrect-attr-id.md.
#[test]
#[should_panic(expected = "used an incorrect `AttrId`")]
fn incorrect_attr_id_speculative_cfg_attr() {
    let (analysis, position) = fixture::position(
        r#"//- proc_macros: identity
//- /lib.rs crate:a cfg:feature=on
#[cfg_attr(feat$0ure = "on", proc_macros::identity)]
fn f() {}
"#,
    );
    let _ = analysis.completions(&COMPLETION_CONFIG, position, None);
}

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
