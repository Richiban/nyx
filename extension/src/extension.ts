import * as fs from "fs";
import * as path from "path";
import { spawn } from "child_process";
import * as vscode from "vscode";

const LANGUAGE_ID = "nyx";
const OUTPUT_CHANNEL_NAME = "Nyx";
const COMMAND_TYPECHECK = "nyx.typecheckFile";

export function activate(context: vscode.ExtensionContext) {
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

  const onSave = vscode.workspace.onDidSaveTextDocument(async (document: vscode.TextDocument) => {
    if (!shouldTypecheckOnSave()) {
      return;
    }

    await typecheckDocument(document, diagnostics, output);
  });

  const onOpen = vscode.workspace.onDidOpenTextDocument(async (document: vscode.TextDocument) => {
    if (!shouldTypecheckOnSave()) {
      return;
    }

    await typecheckDocument(document, diagnostics, output);
  });

  const onClose = vscode.workspace.onDidCloseTextDocument((document: vscode.TextDocument) => {
    if (document.languageId === LANGUAGE_ID) {
      diagnostics.delete(document.uri);
    }
  });

  context.subscriptions.push(diagnostics, output, typecheckCommand, onSave, onOpen, onClose);
}

export function deactivate() {
  // No-op
}

function shouldTypecheckOnSave(): boolean {
  const config = vscode.workspace.getConfiguration("nyx");
  return config.get<boolean>("typecheckOnSave", true);
}

async function typecheckDocument(
  document: vscode.TextDocument,
  diagnostics: vscode.DiagnosticCollection,
  output: vscode.OutputChannel
): Promise<void> {
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
  const dotnetPath = config.get<string>("compilerDotnetPath", "dotnet");
  const projectPathSetting = config.get<string>("compilerProject", "parser/NyxCompiler.Cli/NyxCompiler.Cli.fsproj");
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
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    output.appendLine(`Nyx typecheck failed: ${message}`);
    vscode.window.showErrorMessage(`Nyx typecheck failed: ${message}`);
  }
}

function resolveProjectPath(workspaceRoot: string, projectSetting: string): string {
  if (!projectSetting) {
    return path.join(workspaceRoot, "parser", "NyxCompiler.Cli", "NyxCompiler.Cli.fsproj");
  }

  if (path.isAbsolute(projectSetting)) {
    return projectSetting;
  }

  return path.join(workspaceRoot, projectSetting);
}

function runCompiler(
  dotnetPath: string,
  projectPath: string,
  filePath: string,
  output: vscode.OutputChannel
): Promise<string> {
  const args = ["run", "--project", projectPath, "--", filePath];
  output.appendLine(`Running: ${dotnetPath} ${args.join(" ")}`);

  return new Promise((resolve, reject) => {
    const child = spawn(dotnetPath, args, { cwd: path.dirname(projectPath) });

    let stdout = "";
    let stderr = "";

  child.stdout.on("data", (data: Uint8Array) => {
      stdout += data.toString();
    });

  child.stderr.on("data", (data: Uint8Array) => {
      stderr += data.toString();
    });

  child.on("error", (error: Error) => {
      reject(error);
    });

  child.on("close", (code: number | null) => {
      const combined = [stdout.trim(), stderr.trim()].filter(Boolean).join("\n");
      if (code !== 0 && !combined) {
        reject(new Error(`Compiler exited with code ${code}`));
        return;
      }

      resolve(combined || stdout);
    });
  });
}

function parseDiagnostics(output: string): vscode.Diagnostic[] {
  if (!output) {
    return [];
  }

  const diagnostics: vscode.Diagnostic[] = [];
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
