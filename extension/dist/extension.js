"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const child_process_1 = require("child_process");
const vscode = __importStar(require("vscode"));
const LANGUAGE_ID = "nyx";
const OUTPUT_CHANNEL_NAME = "Nyx";
const COMMAND_TYPECHECK = "nyx.typecheckFile";
function activate(context) {
    const output = vscode.window.createOutputChannel(OUTPUT_CHANNEL_NAME);
    const diagnostics = vscode.languages.createDiagnosticCollection(LANGUAGE_ID);
    const typecheckCommand = vscode.commands.registerCommand(COMMAND_TYPECHECK, async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showInformationMessage("No active editor to typecheck.");
            return;
        }
        await typecheckDocument(editor.document, diagnostics, output);
    });
    const onSave = vscode.workspace.onDidSaveTextDocument(async (document) => {
        if (!shouldTypecheckOnSave()) {
            return;
        }
        await typecheckDocument(document, diagnostics, output);
    });
    const onOpen = vscode.workspace.onDidOpenTextDocument(async (document) => {
        if (!shouldTypecheckOnSave()) {
            return;
        }
        await typecheckDocument(document, diagnostics, output);
    });
    const onClose = vscode.workspace.onDidCloseTextDocument((document) => {
        if (document.languageId === LANGUAGE_ID) {
            diagnostics.delete(document.uri);
        }
    });
    context.subscriptions.push(diagnostics, output, typecheckCommand, onSave, onOpen, onClose);
}
function deactivate() {
    // No-op
}
function shouldTypecheckOnSave() {
    const config = vscode.workspace.getConfiguration("nyx");
    return config.get("typecheckOnSave", true);
}
async function typecheckDocument(document, diagnostics, output) {
    if (document.languageId !== LANGUAGE_ID) {
        return;
    }
    if (document.isUntitled) {
        output.appendLine("Skipping typecheck for untitled document.");
        return;
    }
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
    if (!workspaceFolder) {
        output.appendLine("Nyx typecheck skipped: document is not in a workspace folder.");
        return;
    }
    const workspaceRoot = workspaceFolder.uri.fsPath;
    const config = vscode.workspace.getConfiguration("nyx", document.uri);
    const dotnetPath = config.get("compilerDotnetPath", "dotnet");
    const projectPathSetting = config.get("compilerProject", "parser/NyxCompiler.Cli/NyxCompiler.Cli.fsproj");
    const projectPath = resolveProjectPath(workspaceRoot, projectPathSetting);
    if (!fs.existsSync(projectPath)) {
        const message = `Nyx compiler project not found at ${projectPath}`;
        output.appendLine(message);
        vscode.window.showErrorMessage(message);
        return;
    }
    output.appendLine(`Typechecking ${document.fileName}`);
    try {
        const result = await runCompiler(dotnetPath, projectPath, document.fileName, output);
        const parsed = parseDiagnostics(result);
        diagnostics.set(document.uri, parsed);
        if (parsed.length === 0) {
            output.appendLine("No diagnostics reported.");
        }
    }
    catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        output.appendLine(`Nyx typecheck failed: ${message}`);
        vscode.window.showErrorMessage(`Nyx typecheck failed: ${message}`);
    }
}
function resolveProjectPath(workspaceRoot, projectSetting) {
    if (!projectSetting) {
        return path.join(workspaceRoot, "parser", "NyxCompiler.Cli", "NyxCompiler.Cli.fsproj");
    }
    if (path.isAbsolute(projectSetting)) {
        return projectSetting;
    }
    return path.join(workspaceRoot, projectSetting);
}
function runCompiler(dotnetPath, projectPath, filePath, output) {
    const args = ["run", "--project", projectPath, "--", filePath];
    output.appendLine(`Running: ${dotnetPath} ${args.join(" ")}`);
    return new Promise((resolve, reject) => {
        const child = (0, child_process_1.spawn)(dotnetPath, args, { cwd: path.dirname(projectPath) });
        let stdout = "";
        let stderr = "";
        child.stdout.on("data", (data) => {
            stdout += data.toString();
        });
        child.stderr.on("data", (data) => {
            stderr += data.toString();
        });
        child.on("error", (error) => {
            reject(error);
        });
        child.on("close", (code) => {
            const combined = [stdout.trim(), stderr.trim()].filter(Boolean).join("\n");
            if (code !== 0 && !combined) {
                reject(new Error(`Compiler exited with code ${code}`));
                return;
            }
            resolve(combined || stdout);
        });
    });
}
function parseDiagnostics(output) {
    if (!output) {
        return [];
    }
    const diagnostics = [];
    const lines = output.split(/\r?\n/);
    for (const line of lines) {
        const match = line.match(/^\[(error|warning)\]\s*(.+)$/i);
        if (!match) {
            continue;
        }
        const severityText = match[1].toLowerCase();
        const message = match[2].trim();
        const severity = severityText === "warning" ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Error;
        const range = new vscode.Range(0, 0, 0, 1);
        diagnostics.push(new vscode.Diagnostic(range, message, severity));
    }
    return diagnostics;
}
//# sourceMappingURL=extension.js.map