use std::time::Duration;

use serde_json::json;
use test_utils::skip_slow_tests;

use crate::support::Project;
use crate::testdir::TestDir;

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

#[test]
fn test_flycheck_diagnostic_with_override_command_write_twice() {
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

    server.write_file_and_save(
        "src/main.rs",
        "fn main() {\n    foo;\n}\n".to_owned(),
    );

    let diag2 = server.wait_for_diagnostics();
    assert!(
        diag2.diagnostics.iter().any(|d| d.message.contains("cannot find value")),
        "expected 'cannot find value' diagnostic, got: {:?}",
        diag2.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>(),
    );
}

#[test]
fn test_flycheck_diagnostics_for_json_project() {
    if skip_slow_tests() {
        return;
    }

    let tmp_dir = TestDir::new();
    let path = tmp_dir.path();

    let project = json!({
        "crates": [{
            "root_module": path.join("src/main.rs"),
            "deps": [],
            "edition": "2021",
            "cfg": [],
            "build": {
                "label": "//main:main",
                "build_file": path.join("BUILD"),
                "target_kind": "bin",
            },
        }]
    });

    let code = format!(
        r#"
//- /.rust-project.json
{project}

//- /src/main.rs
fn main() {{}}
"#,
    );

    let server = Project::with_fixture(&code)
        .tmp_dir(tmp_dir)
        .with_config(serde_json::json!({
            "checkOnSave": true,
            "check": {
                "overrideCommand": ["rustc", "--error-format=json", "$saved_file"]
            }
        }))
        .server()
        .wait_until_workspace_is_loaded();

    server.write_file_and_save(
        "src/main.rs",
        "fn main() {\n    let x = 1;\n}\n".to_owned(),
    );

    let diagnostics = server.wait_for_diagnostics();
    assert!(
        diagnostics.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic, got: {:?}",
        diagnostics.diagnostics,
    );
}

#[test]
fn test_flycheck_diagnostics_for_json_project_multiple_workspaces() {
    if skip_slow_tests() {
        return;
    }

    // Set up two rust-project.json workspaces where the source files live
    // OUTSIDE the workspace roots. This means didChangeWatchedFiles will
    // see the saved file as outside all workspace roots and trigger a
    // workspace-level flycheck with saved_file: None, which races with
    // the didSave flycheck that has saved_file set.
    let tmp_dir = TestDir::new();
    let path = tmp_dir.path();

    let project1 = json!({
        "crates": [{
            "root_module": path.join("src1/main.rs"),
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

    let project2 = json!({
        "crates": [{
            "root_module": path.join("src2/main.rs"),
            "deps": [],
            "edition": "2021",
            "cfg": [],
            "is_workspace_member": true,
            "build": {
                "label": "//ws2:main",
                "build_file": path.join("ws2/BUILD"),
                "target_kind": "bin",
            },
        }]
    });

    let code = format!(
        r#"
//- /ws1/.rust-project.json
{project1}

//- /src1/main.rs
fn main() {{}}

//- /ws2/.rust-project.json
{project2}

//- /src2/main.rs
fn main() {{}}
"#,
    );

    let server = Project::with_fixture(&code)
        .tmp_dir(tmp_dir)
        .root("ws1")
        .root("ws2")
        .with_config(serde_json::json!({
            "checkOnSave": true,
            "check": {
                "overrideCommand": ["sh", "-c", "sleep 1 && rustc --error-format=json {saved_file}"],
            }
        }))
        .server()
        .wait_until_workspace_is_loaded();

    // Introduce an unused variable in ws1. The source file is outside the
    // workspace roots, so didChangeWatchedFiles will also trigger a
    // workspace flycheck with saved_file: None.
    server.write_file_and_save(
        "src1/main.rs",
        "fn main() {\n    let x = 1;\n}\n".to_owned(),
    );

    let diag1 = server.wait_for_diagnostics();
    assert!(
        diag1.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic from ws1, got: {:?}",
        diag1.diagnostics,
    );
}

/// Regression test: when source files are outside the workspace roots,
/// didChangeWatchedFiles triggers a workspace-level flycheck with
/// saved_file: None. If this arrives after the didSave flycheck has
/// already started running, it cancels the running check and tries to
/// start a new one — but $saved_file / {saved_file} can't be
/// substituted, so no check runs and diagnostics are lost.
///
/// This test separates the two notifications with a delay so the first
/// flycheck command is actively running when the second restart arrives.
#[test]
fn test_flycheck_json_project_didchangewatchedfiles_cancels_save() {
    if skip_slow_tests() {
        return;
    }

    let tmp_dir = TestDir::new();
    let path = tmp_dir.path();

    let project = json!({
        "crates": [{
            "root_module": path.join("src/main.rs"),
            "deps": [],
            "edition": "2021",
            "cfg": [],
            "is_workspace_member": true,
            "build": {
                "label": "//main:main",
                "build_file": path.join("project/BUILD"),
                "target_kind": "bin",
            },
        }]
    });

    let code = format!(
        r#"
//- /project/.rust-project.json
{project}

//- /src/main.rs
fn main() {{}}
"#,
    );

    // Workspace root is "project/", but source files are in "src/"
    // which is outside the workspace root. This makes
    // didChangeWatchedFiles trigger a workspace flycheck.
    let server = Project::with_fixture(&code)
        .tmp_dir(tmp_dir)
        .root("project")
        .with_config(serde_json::json!({
            "checkOnSave": true,
            "check": {
                "overrideCommand": [
                    "sh", "-c",
                    "sleep 1 && rustc --error-format=json {saved_file}"
                ],
            }
        }))
        .server()
        .wait_until_workspace_is_loaded();

    // Step 1: Write the file and send didSave only. This triggers
    // run_flycheck which (after async Salsa queries) starts the check
    // command. The command sleeps for 1s so it will still be running
    // when we send didChangeWatchedFiles.
    let text = "fn main() {\n    let x = 1;\n}\n".to_owned();
    std::fs::write(server.path().join("src/main.rs"), &text).unwrap();
    server.notification::<lsp_types::notification::DidSaveTextDocument>(
        lsp_types::DidSaveTextDocumentParams {
            text_document: server.doc_id("src/main.rs"),
            text: Some(text),
        },
    );

    // Step 2: Wait for the flycheck command to start running.
    std::thread::sleep(Duration::from_millis(500));

    // Step 3: Send didChangeWatchedFiles. Because src/main.rs is
    // outside the workspace root "project/", this triggers
    // restart_workspace(None) which cancels the running check and
    // attempts to start a new one with saved_file: None.
    // The {saved_file} substitution fails, so no check runs.
    server.notification::<lsp_types::notification::DidChangeWatchedFiles>(
        lsp_types::DidChangeWatchedFilesParams {
            changes: vec![lsp_types::FileEvent {
                uri: server.doc_id("src/main.rs").uri,
                typ: lsp_types::FileChangeType::CHANGED,
            }],
        },
    );

    let diagnostics = server.wait_for_diagnostics();
    assert!(
        diagnostics.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic, got: {:?}",
        diagnostics.diagnostics,
    );
}

#[test]
fn test_flycheck_override_command_multiple_workspaces() {
    if skip_slow_tests() {
        return;
    }

    // This is probably failing with the patch to track packages on rust-project configured crates.
    let server = Project::with_fixture(
        r#"
//- /ws1/Cargo.toml
[package]
name = "ws1"
version = "0.0.0"

//- /ws1/src/main.rs
fn main() {}

//- /ws2/Cargo.toml
[package]
name = "ws2"
version = "0.0.0"

//- /ws2/src/main.rs
fn main() {}
"#,
    )
    .root("ws1")
    .root("ws2")
    .with_config(serde_json::json!({
        "checkOnSave": true,
        "check": {
            "overrideCommand": ["rustc", "--error-format=json", "$saved_file"],
        }
    }))
    .server()
    .wait_until_workspace_is_loaded();

    // Introduce an unused variable in ws1.
    server.write_file_and_save(
        "ws1/src/main.rs",
        "fn main() {\n    let x = 1;\n}\n".to_owned(),
    );

    let diag1 = server.wait_for_diagnostics();
    assert!(
        diag1.diagnostics.iter().any(|d| d.message.contains("unused variable")),
        "expected unused variable diagnostic from ws1, got: {:?}",
        diag1.diagnostics,
    );
}
