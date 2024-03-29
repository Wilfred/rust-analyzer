import * as vscode from "vscode";
import * as toolchain from "./toolchain";
import type { Config } from "./config";
import { log } from "./util";
import { unwrapUndefinable } from "./undefinable";

// This ends up as the `type` key in tasks.json. RLS also uses `cargo` and
// our configuration should be compatible with it so use the same key.
export const CARGO_TASK_TYPE = "cargo";

export const RUST_SHELL_TASK_TYPE = "rust_shell";

export const TASK_SOURCE = "rust";

export interface CargoTaskDefinition extends vscode.TaskDefinition {
    // The cargo command, such as "run" or "check".
    command: string;
    // Additional arguments passed to the cargo command.
    args?: string[];
    // The working directory to run the cargo command in.
    cwd?: string;
    // The shell environment.
    env?: { [key: string]: string };
    // Override the cargo executable name, such as
    // "my_custom_cargo_bin".
    overrideCargo?: string;
}

export interface RustShellTaskDefinition extends vscode.TaskDefinition {
    // The CLI program to run, such as "cargo" or "rustc".
    program: string;
    // CLI arguments.
    args: string[];
    // The working directory to run this shell command in.
    cwd?: string;
    // The shell environment.
    env?: { [key: string]: string };
}

class RustTaskProvider implements vscode.TaskProvider {
    private readonly config: Config;

    constructor(config: Config) {
        this.config = config;
    }

    async provideTasks(): Promise<vscode.Task[]> {
        // Detect Rust tasks. Currently we do not do any actual detection
        // of tasks (e.g. aliases in .cargo/config) and just return a fixed
        // set of tasks that always exist. These tasks cannot be removed in
        // tasks.json - only tweaked.

        const defs = [
            { command: "build", group: vscode.TaskGroup.Build },
            { command: "check", group: vscode.TaskGroup.Build },
            { command: "clippy", group: vscode.TaskGroup.Build },
            { command: "test", group: vscode.TaskGroup.Test },
            { command: "clean", group: vscode.TaskGroup.Clean },
            { command: "run", group: undefined },
        ];

        const tasks: vscode.Task[] = [];
        for (const workspaceTarget of vscode.workspace.workspaceFolders || []) {
            for (const def of defs) {
                const vscodeTask = await buildRustTask(
                    workspaceTarget,
                    { type: CARGO_TASK_TYPE, command: def.command },
                    `cargo ${def.command}`,
                    this.config,
                );
                vscodeTask.group = def.group;
                tasks.push(vscodeTask);
            }
        }

        return tasks;
    }

    async resolveTask(task: vscode.Task): Promise<vscode.Task | undefined> {
        // VSCode calls this for every cargo task in the user's tasks.json,
        // we need to inform VSCode how to execute that command by creating
        // a ShellExecution for it.

        if (task.definition.type === CARGO_TASK_TYPE || task.definition.type === RUST_SHELL_TASK_TYPE) {
            return await buildRustTask(
                task.scope,
                task.definition as CargoTaskDefinition | RustShellTaskDefinition,
                task.name,
                this.config,
            );
        }

        return undefined;
    }
}

export async function buildRustTask(
    scope: vscode.WorkspaceFolder | vscode.TaskScope | undefined,
    taskDefinition: CargoTaskDefinition | RustShellTaskDefinition,
    name: string,
    config: Config,
    throwOnError: boolean = false,
): Promise<vscode.Task> {
    let exec: vscode.ProcessExecution | vscode.ShellExecution;
    if (taskDefinition.command) {
        exec = await cargoToExecution(
            taskDefinition as CargoTaskDefinition,
            config.cargoRunner,
            throwOnError,
        );
    } else {
        exec = shellToExecution(taskDefinition as RustShellTaskDefinition);
    }

    return new vscode.Task(
        taskDefinition,
        // scope can sometimes be undefined. in these situations we default to the workspace taskscope as
        // recommended by the official docs: https://code.visualstudio.com/api/extension-guides/task-provider#task-provider)
        scope ?? vscode.TaskScope.Workspace,
        name,
        TASK_SOURCE,
        exec,
        config.problemMatcher,
    );
}

async function cargoToExecution(
    cargoTask: CargoTaskDefinition,
    customRunner: string | undefined,
    throwOnError: boolean,
): Promise<vscode.ProcessExecution | vscode.ShellExecution> {
    if (customRunner) {
        const runnerCommand = `${customRunner}.buildShellExecution`;

        try {
            const runnerArgs = {
                kind: CARGO_TASK_TYPE,
                args: cargoTask.args,
                cwd: cargoTask.cwd,
                env: cargoTask.env,
            };
            const customExec = await vscode.commands.executeCommand(runnerCommand, runnerArgs);
            if (customExec) {
                if (customExec instanceof vscode.ShellExecution) {
                    return customExec;
                } else {
                    log.debug("Invalid cargo ShellExecution", customExec);
                    throw "Invalid cargo ShellExecution.";
                }
            }
            // fallback to default processing
        } catch (e) {
            if (throwOnError) throw `Cargo runner '${customRunner}' failed! ${e}`;
            // fallback to default processing
        }
    }

    // Check whether we must use a user-defined substitute for cargo.
    // Split on spaces to allow overrides like "wrapper cargo".
    const cargoPath = await toolchain.cargoPath();
    const cargoCommand = cargoTask.overrideCargo?.split(" ") ?? [cargoPath];

    const args = [cargoTask.command].concat(cargoTask.args ?? []);
    const fullCommand = [...cargoCommand, ...args];

    const processName = unwrapUndefinable(fullCommand[0]);

    return new vscode.ProcessExecution(processName, fullCommand.slice(1), {
        cwd: cargoTask.cwd,
        env: cargoTask.env,
    });
}

function shellToExecution(shellTask: RustShellTaskDefinition): vscode.ProcessExecution {
    return new vscode.ProcessExecution(shellTask.program, shellTask.args, {
        cwd: shellTask.cwd,
        env: shellTask.env,
    });
}

export function activateTaskProvider(config: Config): vscode.Disposable {
    const provider = new RustTaskProvider(config);
    return vscode.tasks.registerTaskProvider(CARGO_TASK_TYPE, provider);
}
