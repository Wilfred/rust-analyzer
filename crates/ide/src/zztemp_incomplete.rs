//! Temporary brute-force harness: run many IDE features at every offset of
//! snippets representing legitimate intermediate states of unfinished code,
//! and report any panics.

use ide_db::ra_fixture::RaFixtureConfig;
use syntax::{TextRange, TextSize};

use crate::inlay_hints::InlayHintsConfig;
use crate::syntax_highlighting::HighlightConfig;
use crate::{
    AdjustmentHints, AdjustmentHintsMode, AssistResolveStrategy, CallableSnippets,
    ClosureReturnTypeHints, CompletionConfig, CompletionFieldsToResolve,
    DiagnosticsConfig, DiscriminantHints, FilePosition, FileRange, FileStructureConfig,
    GenericParameterHints, GotoDefinitionConfig, HoverConfig, HoverDocFormat, InlayFieldsToResolve,
    LifetimeElisionHints, MemoryLayoutHoverConfig, MemoryLayoutHoverRenderKind, SubstTyLen,
    TypeHintsPlacement, fixture,
};
use hir::PrefixKind;
use ide_db::SnippetCap;
use ide_db::imports::insert_use::{ImportGranularity, InsertUseConfig};

const HOVER_CONFIG: HoverConfig<'_> = HoverConfig {
    links_in_hover: true,
    memory_layout: Some(MemoryLayoutHoverConfig {
        size: Some(MemoryLayoutHoverRenderKind::Both),
        offset: Some(MemoryLayoutHoverRenderKind::Both),
        alignment: Some(MemoryLayoutHoverRenderKind::Both),
        padding: Some(MemoryLayoutHoverRenderKind::Both),
        niches: true,
    }),
    documentation: true,
    format: HoverDocFormat::Markdown,
    keywords: true,
    max_trait_assoc_items_count: None,
    max_fields_count: Some(5),
    max_enum_variants_count: Some(5),
    max_subst_ty_len: SubstTyLen::Unlimited,
    show_drop_glue: true,
    ra_fixture: RaFixtureConfig::default(),
};

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

const INLAY_CONFIG: InlayHintsConfig<'_> = InlayHintsConfig {
    discriminant_hints: DiscriminantHints::Always,
    render_colons: true,
    type_hints: true,
    type_hints_placement: TypeHintsPlacement::Inline,
    parameter_hints: true,
    parameter_hints_for_missing_arguments: true,
    sized_bound: true,
    generic_parameter_hints: GenericParameterHints {
        type_hints: true,
        lifetime_hints: true,
        const_hints: true,
    },
    chaining_hints: true,
    lifetime_elision_hints: LifetimeElisionHints::Always,
    closure_return_type_hints: ClosureReturnTypeHints::Always,
    closure_capture_hints: true,
    adjustment_hints: AdjustmentHints::Always,
    adjustment_hints_disable_reborrows: false,
    adjustment_hints_mode: AdjustmentHintsMode::Prefix,
    adjustment_hints_hide_outside_unsafe: false,
    binding_mode_hints: true,
    hide_inferred_type_hints: false,
    hide_named_constructor_hints: false,
    hide_closure_initialization_hints: false,
    hide_closure_parameter_hints: false,
    closure_style: hir::ClosureStyle::ImplFn,
    param_names_for_lifetime_elision_hints: true,
    max_length: None,
    closing_brace_hints_min_lines: Some(0),
    fields_to_resolve: InlayFieldsToResolve::empty(),
    implicit_drop_hints: true,
    implied_dyn_trait_hints: true,
    range_exclusive_hints: true,
    ra_fixture: RaFixtureConfig::default(),
};

const HL_CONFIG: HighlightConfig<'_> = HighlightConfig {
    strings: true,
    comments: true,
    punctuation: true,
    specialize_punctuation: true,
    specialize_operator: true,
    operator: true,
    inject_doc_comment: true,
    macro_bang: true,
    syntactic_name_ref_highlighting: false,
    ra_fixture: RaFixtureConfig::default(),
};

const SNIPPETS: &[&str] = &[
    "fn foo(x: i32,",
    "#[repr(, C)] struct Foo;",
    "//- minicore: option\nfn f() -> Option<()> { match 0 { _ => Some(()), } }",
    "struct S { field: i32 }\nimpl S",
    "enum E { A, B }\nimpl E",
    "struct S { field: i32 }\nimpl S {",
    "fn f() { match x { Some(y) } }",
    "fn f() { match x { } }",
    "fn f() { match x { _ => } }",
    "fn f() { let x = ",
    "fn f(s: S) { s.  }\nstruct S { a: i32 }",
    "fn f() { let  = 5; }",
    "impl<  Type {}",
    "fn foo<T: >() {}",
    "use std::{",
    "use ",
    "fn f() { if let  = x {} }",
    "trait T { fn f(&self",
    "struct S(",
    "fn f() { |x| }",
    "fn f() { let c = |x: i32| x; c(",
    "fn f() { for x in }",
    "fn f() { loop { break ' } }",
    "fn f() -> { }",
    "#[derive()] struct D;",
    "#[derive(] struct D;",
    "fn f() { let s = \"",
    "fn f() { let c = '' ; }",
    "fn f() { let x = 340282366920938463463374607431768211456; }",
    "fn f() { let x = 1e999999; }",
    "fn f() { let t = (1, 2); t.99999999999999999999 }",
    "fn f() { 1 + }",
    "fn f() { x as }",
    "fn f() { let r#  = 1; }",
    "mod m { pub struct S; }\nfn f(s: m::",
    "fn f() where ",
    "static X: i32 = ;",
    "const  : i32 = 1;",
    "type A = ",
    "enum E { A(",
    "fn f() { return ",
    "fn f() { async }",
    "fn f() { let _: dyn = 1; }",
    "fn f(x: &dyn) {}",
    "fn f() { if x {  } else",
    "trait Tr {}\nfn f(x: impl ) {}",
    "extern \"C\" {",
    "fn f() { m!( }",
    "macro_rules! m { () => ",
    "fn f() { let Some(x) = y else ",
    // type-system flavored intermediate states
    "struct S<T>(T);\nfn f() { let x: S::<5> = todo!(); }",
    "struct S<const N: usize>;\nfn f() { let x: S<i32> = todo!(); }",
    "type A = A;",
    "type A = B;\ntype B = A;",
    "struct R { r: R }\nfn f(r: R) { let x = r; }",
    "trait Tr { fn m(&self); }\nfn f(x: &dyn Tr) { x.m() }",
    "fn f() { (|| ()).0 }",
    "//- minicore: deref\nuse core::ops::Deref;\nstruct D;\nimpl Deref for D { type Target = D; fn deref(&self) -> &D { self } }\nfn f(d: D) { d.  }",
    "//- minicore: sized\nfn f() -> impl ",
    "fn f<T>() where T: ",
    "fn f() { struct Inner",
    "union U ",
    "fn f(self",
    "impl Trait for ",
    "fn f() { yeet }",
    "fn f() { let x: fn( = 1; }",
    // multi-file / cyclic states
    "//- /lib.rs\npub mod a;\npub use crate::a::*;\n//- /a.rs\npub use crate::*;\npub struct InA;",
    "//- /lib.rs\ninclude!(\"lib.rs\");",
    "//- /lib.rs\ninclude!(\"a.rs\");\n//- /a.rs\ninclude!(\"lib.rs\");",
    "macro_rules! m { () => { m!{} } }\nm!{}",
    "macro_rules! m { () => { m! } }\nfn f() { m!{} }",
    "//- /lib.rs\n#[path = \"lib.rs\"]\nmod a;",
    "fn f() { core:: }",
    "//- minicore: iterator\nfn f() { (0..10).map(|x| ).  }",
    "trait A: B {}\ntrait B: A {}\nfn f(x: &dyn A) { x. }",
    "const A: i32 = B;\nconst B: i32 = A;\nfn f() { let x = [0; A]; }",
    "struct S<T: >(T);\nfn f() { let s = S(S(S(1))); s.0. }",
    "//- minicore: deref\nuse core::ops::Deref;\nstruct A;\nstruct B;\nimpl Deref for A { type Target = B; fn deref(&self) -> &B { todo!() } }\nimpl Deref for B { type Target = A; fn deref(&self) -> &A { todo!() } }\nfn f(a: A) { a.missing() }",
    "trait Tr { type T; }\nimpl Tr for () { type T = <() as Tr>::T; }\nfn f(x: <() as Tr>::T) { x }",
    "enum E { V(Box<E>) }\nfn f(e: E) { match e { } }",
    "fn f() { let v = vec![vec![vec![vec![]]]]; v[0][0][0][0] }",
    "struct S;\nimpl S { fn s(&self) -> S { S } }\nfn f(s: S) { s.s().s().s().s().s().s().s().s().s().s() }",
];

#[test]
fn zztemp_rename_self_chained_method_calls() {
    let (analysis, position) = fixture::position(
        "struct S;\nimpl S { fn s(&$0self) -> S { S } }\nfn f(s: S) { s.s().s(); }",
    );
    let _ = analysis
        .rename(
            position,
            "this",
            &crate::rename::RenameConfig {
                prefer_no_std: false,
                prefer_prelude: true,
                prefer_absolute: false,
                show_conflicts: true,
            },
        )
        .unwrap();
}

#[test]
fn zztemp_ide_brute_force_incomplete_code() {
    static LAST_PANIC: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|info| {
        let bt = std::backtrace::Backtrace::force_capture();
        let bt = bt
            .to_string()
            .lines()
            .filter(|l| {
                l.contains("ide") || l.contains("hir") || l.contains("syntax") || l.contains("mbe")
            })
            .take(14)
            .collect::<Vec<_>>()
            .join("\n");
        *LAST_PANIC.lock().unwrap() = Some(format!("{info}\n{bt}"));
    }));

    let mut failures: Vec<String> = Vec::new();
    let record = |failures: &mut Vec<String>, what: &str, i: usize, offset: usize| {
        let msg = LAST_PANIC.lock().unwrap().take().unwrap_or_default();
        let loc_key = msg.lines().next().unwrap_or("").to_owned();
        if loc_key.is_empty() || !failures.iter().any(|f| f.contains(&loc_key)) {
            failures.push(format!("[{what}] snippet #{i} offset {offset}: {msg}"));
        }
    };

    for (i, &snip) in SNIPPETS.iter().enumerate() {
        let (analysis, file_id) = fixture::file(snip);
        let text = analysis.file_text(file_id).unwrap();

        macro_rules! run {
            ($what:literal, $i:expr, $off:expr, $e:expr) => {
                let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let _ = $e;
                }));
                if r.is_err() {
                    record(&mut failures, $what, $i, $off);
                }
            };
        }

        run!("diagnostics", i, 0, {
            analysis.full_diagnostics(
                &DiagnosticsConfig::test_sample(),
                AssistResolveStrategy::All,
                file_id,
            )
        });
        run!("inlay_hints", i, 0, analysis.inlay_hints(&INLAY_CONFIG, file_id, None));
        run!("highlight", i, 0, analysis.highlight(HL_CONFIG, file_id));
        run!("folding", i, 0, analysis.folding_ranges(file_id, true));
        run!("runnables", i, 0, analysis.runnables(file_id));
        run!(
            "structure",
            i,
            0,
            analysis.file_structure(&FileStructureConfig { exclude_locals: false }, file_id)
        );

        for offset in 0..=text.len() {
            if !text.is_char_boundary(offset) {
                continue;
            }
            let off = TextSize::new(offset as u32);
            let position = FilePosition { file_id, offset: off };
            let frange = FileRange { file_id, range: TextRange::empty(off) };

            run!("hover", i, offset, analysis.hover(&HOVER_CONFIG, frange));
            run!(
                "goto_def",
                i,
                offset,
                analysis.goto_definition(
                    position,
                    &GotoDefinitionConfig { ra_fixture: RaFixtureConfig::default() },
                )
            );
            run!(
                "completions",
                i,
                offset,
                analysis.completions(&COMPLETION_CONFIG, position, None)
            );
            run!("sig_help", i, offset, analysis.signature_help(position));
            run!("extend_sel", i, offset, analysis.extend_selection(frange));
            if let Some(c) = text[offset..].chars().next() {
                run!("on_char_typed", i, offset, analysis.on_char_typed(position, c));
            }
            run!("prepare_rename", i, offset, analysis.prepare_rename(position));
            run!(
                "rename",
                i,
                offset,
                analysis.rename(
                    position,
                    "new_name",
                    &crate::rename::RenameConfig {
                        prefer_no_std: false,
                        prefer_prelude: true,
                        prefer_absolute: false,
                        show_conflicts: true,
                    },
                )
            );
            run!(
                "rename_raw",
                i,
                offset,
                analysis.rename(
                    position,
                    "r#type",
                    &crate::rename::RenameConfig {
                        prefer_no_std: false,
                        prefer_prelude: true,
                        prefer_absolute: false,
                        show_conflicts: true,
                    },
                )
            );
            run!(
                "find_refs",
                i,
                offset,
                analysis.find_all_refs(
                    position,
                    &crate::references::FindAllRefsConfig {
                        search_scope: None,
                        ra_fixture: RaFixtureConfig::default(),
                        exclude_imports: false,
                        exclude_tests: false,
                    },
                )
            );
            run!(
                "hl_related",
                i,
                offset,
                analysis.highlight_related(
                    crate::HighlightRelatedConfig {
                        references: true,
                        exit_points: true,
                        break_points: true,
                        closure_captures: true,
                        yield_points: true,
                        branch_exit_points: true,
                    },
                    position,
                )
            );
            run!(
                "join_lines",
                i,
                offset,
                analysis.join_lines(
                    &crate::JoinLinesConfig {
                        join_else_if: true,
                        remove_trailing_comma: true,
                        unwrap_trivial_blocks: true,
                        join_assignments: true,
                    },
                    FileRange {
                        file_id,
                        range: {
                            let mut end = text.len().min(offset + 40);
                            while !text.is_char_boundary(end) {
                                end -= 1;
                            }
                            TextRange::new(off, TextSize::new(end as u32))
                        },
                    },
                )
            );
            run!(
                "call_hierarchy",
                i,
                offset,
                analysis.call_hierarchy(
                    position,
                    &crate::CallHierarchyConfig {
                        exclude_tests: false,
                        ra_fixture: RaFixtureConfig::default(),
                    },
                )
            );
            run!("expand_macro", i, offset, analysis.expand_macro(position));
            run!("matching_brace", i, offset, analysis.matching_brace(position));
        }
    }
    std::panic::set_hook(prev_hook);
    if !failures.is_empty() {
        panic!("found {} panics:\n{}", failures.len(), failures.join("\n---\n"));
    }
}
