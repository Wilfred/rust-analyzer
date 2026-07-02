//! Brute-force hunt for "Bad range" / "Bad offset" panics (rowan
//! `covering_element` / `token_at_offset` asserts) in assists, focused on
//! span-mapping through macros: token reordering/duplication/dropping,
//! def-site tokens, unresolved macros, `include!`, doc-comment desugaring,
//! and proc macros. Every assist is run over every selection range of each
//! snippet. See docs/lsp-panics/01-bad-range-node-range.md.

use ide_db::RootDatabase;
use ide_db::base_db::SourceDatabase;
use test_fixture::WithFixture;
use syntax::{TextRange, TextSize};

use crate::AssistResolveStrategy;
use crate::tests::{TEST_CONFIG, assists};

fn dbs_for(snippet: &str) -> (RootDatabase, Vec<ide_db::base_db::EditionedFileId>) {
    RootDatabase::with_many_files(snippet)
}

#[test]
fn bad_range_hunt_assists_macro_corpus() {
    let snippets: &[&str] = &[
        // token duplication: one arg token maps to two expansion sites
        "macro_rules! m { ($e:expr) => { ($e) + ($e) }; }\nfn f() { let x = m!(1 + 2); }",
        // tokens dropped entirely
        "macro_rules! m { ($e:expr) => {}; }\nfn f() { m!(1 + 2); }",
        // tokens reordered
        "macro_rules! m { ($a:tt $b:tt) => { $b $a }; }\nfn f() { m!({ 1 } { 2 }); }",
        // expr wrapped into an item (def-site fn, call-site tokens)
        "macro_rules! m { ($e:expr) => { fn g() -> i32 { $e } }; }\nm!(1 + 2);",
        // purely def-site expansion, call carries no tokens
        "macro_rules! m { () => { fn generated() -> i32 { 1 + 2 } }; }\nm!();",
        // nested macros
        "macro_rules! n { ($e:expr) => { $e }; }\nmacro_rules! m { ($e:expr) => { n!($e) }; }\nfn f() { let x = m!(1 + 2); }",
        // unresolved macros, various delimiters and breakage
        "fn f() { m!( }",
        "fn f() { m![ }",
        "fn f() { m!{ }",
        "fn f() { m!(1 + 2) }",
        "macro_rules! m { ($($t:tt)*) => { $($t)* }; }\nfn f() { m!(let x = 1 + ); }",
        "macro_rules! m { ($($t:tt)*) => { $($t)* }; }\nfn f() { m!(if x { ); }",
        // stmts macro with trailing broken tokens
        "macro_rules! m { ($($t:tt)*) => { $($t)* }; }\nfn f() { m! { let a = 1; let b = } }",
        // doc comments desugared in macro input
        "macro_rules! m { ($($t:tt)*) => { $($t)* }; }\nm! { /// docs\n fn g() { let x = 1 + 2; } }",
        // include! maps original ranges into another file
        "//- /main.rs\ninclude!(\"other.rs\");\nfn f() { let x = g(); }\n//- /other.rs\nfn g() -> i32 { 1 + 2 }",
        // asm! (special span handling)
        "//- minicore: asm\nfn f() { unsafe { core::arch::asm!(\"nop\") } }",
        // format_args! with implicit captures (synthesized spans)
        "//- minicore: fmt\nfn f() { let a = 1; format_args!(\"{} {a}\", 1 + 2); }",
        // proc macros: identity attr, token-mangling variants
        "//- proc_macros: identity\n#[proc_macros::identity]\nfn f() { let x = (1 + 2); }",
        "//- proc_macros: input_replace\n#[proc_macros::input_replace(fn f() { let x = (1 + 2); })]\nfn dummy() {}",
        "//- proc_macros: derive_identity\n#[derive(proc_macros::DeriveIdentity)]\nstruct S { field: (i32, i32) }",
        "//- proc_macros: shorten\nfn f() { let x = proc_macros::shorten!(1 + 2); }",
        "//- proc_macros: mirror\nfn f() { proc_macros::mirror! { field: (i32, i32) } }",
        // macro-generated function, assist at the call site (inline_call paths)
        "macro_rules! m { () => { fn add(a: i32) -> i32 { a + a } }; }\nm!();\nfn f() { let x = add(1); }",
        // function whose param is used inside a macro call in its body
        "macro_rules! m { ($e:expr) => { $e }; }\nfn add(a: i32) -> i32 { m!(a) }\nfn f() { let x = add(1); }",
        // cfg_attr'd attr macro input (leaf-token insertion paths)
        "//- proc_macros: identity\n#[cfg_attr(true, proc_macros::identity)]\nfn f() { let x = (1 + 2); }",
    ];

    static LAST_PANIC: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|info| {
        *LAST_PANIC.lock().unwrap() = Some(format!("{info}"));
    }));

    let mut failures: Vec<String> = Vec::new();
    for (i, &snip) in snippets.iter().enumerate() {
        let (db, files) = dbs_for(snip);
        for file_id in files {
            let text = SourceDatabase::file_text(&db, file_id.file_id(&db)).text(&db).to_string();
            for start in 0..=text.len() {
                if !text.is_char_boundary(start) {
                    continue;
                }
                for end in start..=text.len() {
                    if !text.is_char_boundary(end) {
                        continue;
                    }
                    let range = TextRange::new(
                        TextSize::new(start as u32),
                        TextSize::new(end as u32),
                    );
                    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        assists(
                            &db,
                            &TEST_CONFIG,
                            AssistResolveStrategy::All,
                            ide_db::FileRange { file_id: file_id.file_id(&db), range },
                        )
                    }));
                    if result.is_err() {
                        let msg = LAST_PANIC.lock().unwrap().take().unwrap_or_default();
                        // Known panic classes, pinned by dedicated repro tests:
                        // - extract_variable "Bad range" family
                        //   (repro_bad_range_unresolved_macro_paren / _duplicated /
                        //   _dropped; fixed by the pending containment guard)
                        // - extract_function edit_algo unimplemented
                        // - TtTreeSink empty-literal debug assert
                        //   (ide lsp_error_repros::repro_proc_macro_empty_literal_token_sink)
                        let known = msg.contains("Bad range: node range")
                            || msg.contains("cannot resolve changes that depend")
                            || msg.contains("assertion `left != right` failed");
                        // Empty message = salsa re-panic via resume_unwind
                        // (hook not invoked); the first occurrence of any
                        // class always fires the hook with its message.
                        if known || msg.is_empty() {
                            continue;
                        }
                        let entry = format!("snippet #{i} range {start}..{end}: {msg}");
                        let loc_key = msg.lines().next().unwrap_or("").to_owned();
                        if !failures.iter().any(|f| f.contains(&loc_key) && !loc_key.is_empty())
                        {
                            failures.push(entry);
                        }
                    }
                }
            }
        }
    }
    std::panic::set_hook(prev_hook);
    assert!(failures.is_empty(), "found {} panics:\n{}", failures.len(), failures.join("\n---\n"));
}
