namespace NyxCompiler

open Parser.Program

type Severity =
    | ErrorSeverity
    | WarningSeverity

type Diagnostic =
    { Severity: Severity
      Message: string
      Range: (int * int) option }

type CompilationPhase =
    | Parsed
    | Desugared
    | Typed
    | Lowered
    | Emitted

type TypedModule =
    { Module: Module
      Types: Map<string, TypeExpr> }

type CompileResult =
    { Phase: CompilationPhase
      Diagnostics: Diagnostic list
      Typed: TypedModule option }

module Diagnostics =
    let error message =
        { Severity = ErrorSeverity
          Message = message
          Range = None }

    let warning message =
        { Severity = WarningSeverity
          Message = message
          Range = None }

module Compiler =
    let parse (source: string) : Result<Module, Diagnostic list> =
        match parseModule source with
        | Result.Ok ast -> Ok ast
        | Result.Error err -> Error [ Diagnostics.error err ]

    let desugar (module': Module) =
        module'

    let typecheck (module': Module) : Result<TypedModule, Diagnostic list> =
        Ok { Module = module'
             Types = Map.empty }

    let compile (source: string) : CompileResult =
        match parse source with
        | Result.Error diagnostics ->
            { Phase = Parsed
              Diagnostics = diagnostics
              Typed = None }
        | Result.Ok ast ->
            let desugared = desugar ast
            match typecheck desugared with
            | Result.Error diagnostics ->
                { Phase = Typed
                  Diagnostics = diagnostics
                  Typed = None }
            | Result.Ok typed ->
                { Phase = Typed
                  Diagnostics = []
                  Typed = Some typed }
