# Diagnosis: "failed fetching data layout for ManifestPath { ... } workspace" logged during workspace loading

## Summary

During cargo-workspace loading, rust-analyzer queries the toolchain for the target's data
layout by running `--print target-spec-json` — first through `cargo rustc`, then falling back
to plain `rustc`. When **both** invocations fail, it logs
`ERROR failed fetching data layout for ManifestPath { ... } workspace` (shown by default,
since the default `RA_LOG` filter is `warn`). Analysis then proceeds with `target: Err(...)`,
which degrades everything downstream that needs layout (`size_of`-style hovers, layout-related
diagnostics, `#[cfg]` correctness if the sibling queries also failed).

This is an environment/configuration failure rather than an r-a code bug, but the error is
easy to hit, fires on **every workspace (re)load**, and the message buries the actionable
cause. Three concrete reproducible situations are documented below, plus fix directions for
making the failure visible/actionable instead of a log-only error.

## Exact symptom

```
ERROR failed fetching data layout for ManifestPath { file: AbsPathBuf("/path/to/Cargo.toml") } workspace
e=unable to fetch target-data-layout via `cd "/path/to" && RUSTC_BOOTSTRAP="1" "rustc" "-Z" "unstable-options" "--print" "target-spec-json" ...`: ...
```

It usually co-occurs with the sibling errors from the same loading phase:
`failed fetching toolchain version for ... workspace` and rustc-cfg fetch warnings.

## Where the message is emitted

- Log site: `crates/project-model/src/workspace.rs:316-327` (thread
  `ProjectWorkspace::target_data` inside `ProjectWorkspace::load`, cargo-workspace path):

  ```rust
  let target_data = Builder::new()
      .name("ProjectWorkspace::target_data".to_owned())
      .spawn_scoped(s, || {
          target_data::get(toolchain_config, targets.first().map(Deref::deref), extra_env)
              .inspect_err(|e| {
                  tracing::error!(%e,
                      "failed fetching data layout for \
                      {cargo_toml:?} workspace"
                  )
              })
      })
  ```

  (Other callers at `workspace.rs:498` (rust-project.json) and `:567` (detached files) do not
  log; the `ManifestPath {...}` debug formatting pins the observed message to this site.)

- Query implementation: `crates/project-model/src/toolchain_info/target_data.rs::get`:
  1. Attempt 1 (only a `tracing::warn!` if it fails, line 65):
     `cargo rustc -Z unstable-options --print target-spec-json [--target <T>] -- -Z unstable-options`
     with `RUSTC_BOOTSTRAP=1`, cwd = manifest dir.
  2. Attempt 2 (failure propagates to the `error!` above):
     `rustc -Z unstable-options --print target-spec-json [--target <T>]`
     with `RUSTC_BOOTSTRAP=1`, using `Sysroot::tool(Tool::Rustc, ...)`.
  3. The output must parse as JSON with `data-layout` and `arch` keys
     (`could not parse target-spec-json from command output` otherwise).

- The `--target <T>` value comes from `target_tuple::get`
  (`crates/project-model/src/toolchain_info/target_tuple.rs`): the
  `rust-analyzer.cargo.target` setting if set, otherwise the `build.target` from
  `.cargo/config.toml` (via cargo config), otherwise host.

## Verified reproducing situations

All verified end-to-end (the exact ERROR line appears on stderr of
`rust-analyzer diagnostics <project>` or an LSP session):

1. **Invalid/unknown target triple in configuration.** Either
   `rust-analyzer.cargo.target = "bogus"` or:

   ```toml
   # .cargo/config.toml
   [build]
   target = "riscv64gc-unknown-fantasy-os"
   ```

   Both attempts receive `--target riscv64gc-unknown-fantasy-os` and fail with
   `error loading target specification: could not find specification for target ...`.
   This also breaks with a *custom target JSON path* that is relative (resolved against
   different cwds by cargo vs rustc) or missing.
   Note: a merely *uninstalled* (but known) target is fine — `--print target-spec-json` does
   not need the target's std (verified with `thumbv6m-none-eabi` absent from rustup).

2. **No `rustc`/`cargo` on rust-analyzer's PATH** — GUI-launched editors that don't inherit a
   shell environment, Nix/direnv setups, containers. Attempt 1 fails to spawn cargo; attempt 2
   fails to spawn rustc (unless sysroot discovery found one). Verified with
   `env -i PATH=/usr/bin:/bin rustc ...` → `rustc: not found`.

3. **`rust-toolchain.toml` pinning a toolchain that rustup cannot provide.** With rustup
   online, missing toolchains are auto-installed (verified: pinning `1.70.0` triggered a
   download and then succeeded — even 1.70 supports `--print target-spec-json` under
   `RUSTC_BOOTSTRAP=1`). But offline / behind a blocking proxy / in a sandbox, the rustup
   shim fails both attempts (verified by pointing `RUSTUP_DIST_SERVER` at a dead address:
   `error: could not download file ... channel-rust-1.81.0.toml.sha256`).

Non-causes worth knowing (checked): virtual workspaces work with `cargo rustc --print`; an
MSRV (`rust-version`) newer than the active toolchain does *not* break the `--print` queries;
old toolchains (≥1.70 at least) support the query with `RUSTC_BOOTSTRAP=1`.

## Consequences when it fires

`ProjectWorkspace.target` is set to `Err(msg)` (`workspace.rs:435-441` area / `:538`), so
`TargetData` (arch + data-layout) is unavailable for the workspace. Layout queries
(`layout_of_ty`) error out; anything needing target arch behaves as "unknown". If the sibling
queries also failed (same root cause), cfgs fall back to defaults and the sysroot may be
missing entirely — which cascades into bogus type-inference diagnostics (see the companion
report `01-inference-diagnostic-in-desugared-expr.md`: a broken sysroot makes those far more
likely).

## Fix guidance

This one is mostly about **surfacing**, not about the query logic:

1. Attach the failure to the workspace-load status the user can see (the `rust-analyzer:
   status`/notification path already carries `ProjectWorkspace.target: Result<_, Box<str>>` —
   ensure the error string is included in the status report / shown in the "Failed to load
   workspace" flow instead of only an `ERROR` log line). Grep for consumers of
   `ProjectWorkspace::target` to find where an `Err` is currently swallowed.
2. Deduplicate/rate-limit: the error re-fires on every workspace reload with identical
   content; consider logging at `warn!` after the first occurrence per manifest, or only
   logging when the error string changes.
3. Improve the message: prefix with the actionable cause (the captured stderr of the failing
   command is already inside `e`, but after the long command line). Something like
   `failed fetching data layout for <path>: could not find specification for target "bogus"`
   ordered cause-first would make user reports self-diagnosing. The `{cargo_toml:?}` debug
   formatting (`ManifestPath { file: AbsPathBuf(...) }`) should be `%` display formatting —
   `ManifestPath` implements `Display` (it derefs to `AbsPath`).
4. Possible behavioral improvement: when `--target` came from configuration and the spec
   lookup fails, retry the query without `--target` (host) so layout data is at least
   host-correct, and report the config error separately.

## Repro harness used for verification

```sh
# scratch project
cargo new dl-test && cd dl-test
mkdir -p .cargo && printf '[build]\ntarget = "riscv64gc-unknown-fantasy-os"\n' > .cargo/config.toml
RA_LOG=error rust-analyzer diagnostics . 2>&1 >/dev/null | grep "failed fetching"
# → ERROR failed fetching data layout for ManifestPath { file: AbsPathBuf(".../dl-test/Cargo.toml") } workspace e=...

# the exact commands rust-analyzer runs, for manual debugging:
RUSTC_BOOTSTRAP=1 cargo rustc -Z unstable-options --print target-spec-json --target <T> -- -Z unstable-options
RUSTC_BOOTSTRAP=1 rustc -Z unstable-options --print target-spec-json --target <T>
```

Verified on rust-analyzer commit `28ccb8be` (master, 2026-07) with rustc 1.94.1/1.97.0-beta.6.
