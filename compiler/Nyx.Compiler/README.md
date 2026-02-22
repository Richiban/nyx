# NyxCompiler

Skeleton for the Nanyx compiler pipeline.

## Goals
- Parse Nanyx source into an AST
- Desugar into a canonical AST
- Typecheck with HM inference + structural equality
- Lower to IR for codegen

## Current API
- `NyxCompiler.Compiler.compile : string -> CompileResult`

## Phases
- `Parsed` → parse only
- `Desugared` → normalized AST (placeholder)
- `Typed` → typed AST (placeholder)
- `Lowered` → IR (placeholder)
- `Emitted` → codegen output (placeholder)
