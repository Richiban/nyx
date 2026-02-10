# Nyx VS Code Extension

This extension provides basic editor support for the Nyx language, including syntax highlighting and compiler-driven diagnostics.

## Features

- Syntax highlighting for `.nyx` files.
- Typechecking diagnostics via `NyxCompiler.Cli`.
- Command palette action: **Nyx: Typecheck Current File**.

## Settings

- `nyx.typecheckOnSave`: Run the compiler on save/open to surface diagnostics (default: `true`).
- `nyx.compilerProject`: Path (relative to workspace root) to `NyxCompiler.Cli.fsproj`.
- `nyx.compilerDotnetPath`: Path to the `dotnet` executable (default: `dotnet`).

## How it works

The extension executes `dotnet run --project <NyxCompiler.Cli.fsproj> -- <file>` and parses `[error]` and `[warning]` lines from stdout to populate VS Code diagnostics.

## Troubleshooting

- If you see **"Nyx compiler project not found"**, update `nyx.compilerProject` to match your workspace layout.
- If `dotnet` is not on your PATH, set `nyx.compilerDotnetPath` to the full path.
