namespace NyxCompiler

open Parser.Program

type CompilationPhase =
    | Parsed
    | Desugared
    | Typed
    | Lowered
    | Emitted

type CompileResult =
    { Phase: CompilationPhase
      Diagnostics: Diagnostic list
      Typed: TypedModule option }

module Compiler =
    let parse (source: string) : Result<Module, Diagnostic list> =
        match parseModule source with
        | Result.Ok ast -> Ok ast
        | Result.Error err -> Error [ Diagnostics.error err ]

    let desugar (module': Module) =
        module'

    let typecheck (desugared: Module) : Result<TypedModule, Diagnostic list> =
        match NyxCompiler.Infer.inferModule desugared with
        | Error diagnostics -> Error diagnostics
        | Ok (types, items, (state: NyxCompiler.Infer.InferState)) ->
            match Unifier.unify state.Constraints with
            | Ok subst ->
                let resolvedTypes =
                    types
                    |> Map.map (fun _ ty -> Unifier.apply subst ty)
                let resolvedItems =
                    items
                    |> List.map (fun item ->
                        let applyExpr (typedExpr: TypedExpr) =
                            { typedExpr with Type = Unifier.apply subst typedExpr.Type }
                        match item with
                        | TypedExprItem expr -> TypedExprItem (applyExpr expr)
                        | TypedDef (TypedValueDef(name, typeOpt, expr)) ->
                            TypedDef (TypedValueDef(name, typeOpt, applyExpr expr))
                        | TypedDef (TypedTypeDef _)
                        | TypedImport _
                        | TypedModuleDecl _ -> item)
                Ok { Module = desugared
                     Types = resolvedTypes
                     Items = resolvedItems }
            | Error message ->
                Error [ Diagnostics.error message ]

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
