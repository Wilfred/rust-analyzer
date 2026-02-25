use test_utils::skip_slow_tests;

use crate::{support::Project, testdir::TestDir};

#[test]
fn test_flycheck_diagnostics_for_unused_variable() {
    if skip_slow_tests() {
        return;
    }

    let server = Project::with_fixture(
        r#"
//- /Cargo.toml
[package]
name = "foo"
version = "0.0.0"

//- /src/main.rs
fn main() {
    let x = 1;
}
"#,
    )
    .with_config(serde_json::json!({
        "checkOnSave": true,
    }))
    .server()
    .wait_until_workspace_is_loaded();

    let diagnostics = server.wait_for_diagnostics();
    assert!(
        diagnostics.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic, got: {:?}",
        diagnostics.diagnostics,
    );
}

#[test]
fn test_flycheck_diagnostic_cleared_after_fix() {
    if skip_slow_tests() {
        return;
    }

    let server = Project::with_fixture(
        r#"
//- /Cargo.toml
[package]
name = "foo"
version = "0.0.0"

//- /src/main.rs
fn main() {
    let x = 1;
}
"#,
    )
    .with_config(serde_json::json!({
        "checkOnSave": true,
    }))
    .server()
    .wait_until_workspace_is_loaded();

    // Wait for the unused variable diagnostic to appear.
    let diagnostics = server.wait_for_diagnostics();
    assert!(
        diagnostics.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic, got: {:?}",
        diagnostics.diagnostics,
    );

    // Fix the code by removing the unused variable.
    server.write_file_and_save("src/main.rs", "fn main() {}\n".to_owned());

    // Wait for diagnostics to be cleared.
    server.wait_for_diagnostics_cleared();
}

#[test]
fn test_flycheck_diagnostics_cleared_after_fix_rust_project_wilfred() {
    if skip_slow_tests() {
        return;
    }

    let tmp_dir = TestDir::new();
    let path = tmp_dir.path();

    let project1 = serde_json::json!({
        "crates": [{
            "root_module": path.join("ws1/src/main.rs"),
            "deps": [],
            "edition": "2021",
            "cfg": [],
            "is_workspace_member": true,
            "build": {
                "label": "//ws1:main",
                "build_file": path.join("ws1/BUILD"),
                "target_kind": "bin",
            },
        }]
    });

    let code = format!(
        r#"
//- /ws1/.rust-project.json
{project1}

//- /ws1/src/main.rs
fn main() {{}}

"#,
    );

    let server = Project::with_fixture(&code)
        .tmp_dir(tmp_dir)
        .root("ws1")
        .with_config(serde_json::json!({
            "checkOnSave": true,
            "check": {
            "overrideCommand": ["rustc", "--error-format=json", "$saved_file"]
            }
        }))
        .server()
        .wait_until_workspace_is_loaded();

    // Introduce an unused variable in ws1. The source file is outside the
    // workspace roots, so didChangeWatchedFiles will also trigger a
    // workspace flycheck with saved_file: None.
    server.write_file_and_save("ws1/src/main.rs", "fn main() {\n    let x = 1;\n}\n".to_owned());

    let diag1 = server.wait_for_diagnostics();
    assert!(
        diag1.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic from ws1, got: {:?}",
        diag1.diagnostics,
    );

    dbg!();

    server.write_file_and_save("ws1/src/main.rs", "fn main() {\n    let _x = 1;\n}\n".to_owned());

    dbg!();
    server.wait_for_diagnostics_cleared();
    dbg!();
}

#[test]
fn test_flycheck_diagnostic_with_override_command() {
    if skip_slow_tests() {
        return;
    }

    let server = Project::with_fixture(
        r#"
//- /Cargo.toml
[package]
name = "foo"
version = "0.0.0"

//- /src/main.rs
fn main() {}
"#,
    )
    .with_config(serde_json::json!({
        "checkOnSave": true,
        "check": {
            "overrideCommand": ["rustc", "--error-format=json", "$saved_file"]
        }
    }))
    .server()
    .wait_until_workspace_is_loaded();

    server.write_file_and_save("src/main.rs", "fn main() {\n    let x = 1;\n}\n".to_owned());

    let diagnostics = server.wait_for_diagnostics();
    assert!(
        diagnostics.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic, got: {:?}",
        diagnostics.diagnostics,
    );
}
