# Nanyx VS Code Extension

This extension provides editor support for the Nanyx language, including syntax highlighting and compiler-driven diagnostics via a language server.

## Features

- Syntax highlighting for `.nyx` files.
- Typechecking diagnostics via the Nanyx language server (powered by `NyxCompiler.Cli`).
- Command palette action: **Nanyx: Typecheck Current File**.

## Settings

- `nyx.typecheckOnSave`: Run the compiler on save/open to surface diagnostics (default: `true`).
- `Nanyx.CompilerProject`: Path (relative to workspace root) to `NyxCompiler.Cli.fsproj`.
- `Nanyx.CompilerDotnetPath`: Path to the `dotnet` executable (default: `dotnet`).

## How it works

The language server executes `dotnet run --project <NyxCompiler.Cli.fsproj> -- <file>` and parses `[error]` and `[warning]` lines from stdout to populate VS Code diagnostics.

## Troubleshooting

- If you see **"Nanyx compiler project not found"**, update `Nanyx.CompilerProject` to match your workspace layout.
- If `dotnet` is not on your PATH, set `Nanyx.CompilerDotnetPath` to the full path.
