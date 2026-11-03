import * as assert from "node:assert/strict";
import * as vscode from "vscode";

import { applySnippetTextEdits } from "../../src/snippets";
import type { Context } from ".";

export async function getTests(ctx: Context) {
    await ctx.suite("Snippet edits", (suite) => {
        suite.addTest("preserves tab indentation in multiline snippets", async () => {
            const document = await vscode.workspace.openTextDocument({
                content: "\tfn old() {}\n",
                language: "plaintext",
            });
            const editor = await vscode.window.showTextDocument(document);
            editor.options = { insertSpaces: false, tabSize: 4 };

            await applySnippetTextEdits(editor, [
                vscode.TextEdit.replace(
                    new vscode.Range(0, 1, 0, 12),
                    "fn new() {\n\t\t$0true\n\t}",
                ),
            ]);

            assert.equal(document.getText(), "\tfn new() {\n\t\ttrue\n\t}\n");
        });
    });
}
