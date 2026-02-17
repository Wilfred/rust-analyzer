use test_utils::skip_slow_tests;

use crate::support::Project;

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
fn test_flycheck_diagnostic_cleared_with_override_command() {
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
    let y = 2;
}
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

    // With $saved_file in the command, initial flycheck on workspace load is
    // skipped (saved_file is None). Trigger it by saving the file.
    server.write_file_and_save(
        "src/main.rs",
        "fn main() {\n    let x = 1;\n    let y = 2;\n}\n".to_owned(),
    );

    // Now introduce an undefined variable to trigger new diagnostics.
    server.write_file_and_save("src/main.rs", "fn main() { z; }\n".to_owned());

    // Collect all subsequent diagnostic notifications until we see the error.
    let subsequent = server.collect_diagnostics_until(|p| {
        p.diagnostics.iter().any(|d| d.message.contains("cannot find value"))
    });

    // Build the full set of diagnostic messages seen across the test.
    let mut all_msg_batches: Vec<Vec<String>> = Vec::new();
    for p in &subsequent {
        all_msg_batches.push(p.diagnostics.iter().map(|d| d.message.clone()).collect());
    }

    // Verify the full progression of diagnostics:
    // Batch 0: unused variable warnings for x and y
    // Batch 1: cleared (empty)
    // Batch 2: error for undefined variable z
    assert_eq!(
        all_msg_batches.len(),
        30,
        "expected 3 diagnostic batches, got: {all_msg_batches:?}",
    );
    assert!(
        all_msg_batches[0].iter().any(|m| m.contains("unused variable: `x`")),
        "batch 0 should have unused `x`: {all_msg_batches:?}",
    );
    assert!(
        all_msg_batches[0].iter().any(|m| m.contains("unused variable: `y`")),
        "batch 0 should have unused `y`: {all_msg_batches:?}",
    );
    assert!(
        all_msg_batches[1].is_empty(),
        "batch 1 should be empty (cleared): {all_msg_batches:?}",
    );
    assert!(
        all_msg_batches[2].iter().any(|m| m.contains("cannot find value")),
        "batch 2 should have 'cannot find value': {all_msg_batches:?}",
    );
}
