namespace NyxCompiler

open System
open System.IO
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
    let private sanitizeIdentifier (name: string) =
        let sanitized =
            name
            |> Seq.map (fun c -> if Char.IsLetterOrDigit c || c = '_' then c else '_')
            |> Seq.toArray
            |> String
        if String.IsNullOrWhiteSpace sanitized then
            "_"
        elif Char.IsDigit sanitized.[0] then
            "_" + sanitized
        else
            sanitized

    let private importPrefix (item: ImportItem) =
        match item.Alias with
        | Some alias -> sanitizeIdentifier alias
        | None ->
            match item.Source with
            | ModuleImport name ->
                let parts = name.Split([| '/'; '.' |], StringSplitOptions.RemoveEmptyEntries)
                parts |> Array.tryLast |> Option.defaultValue name |> sanitizeIdentifier
            | PathImport path ->
                Path.GetFileNameWithoutExtension path |> sanitizeIdentifier

    let private resolveImportPath (baseDir: string) (item: ImportItem) : Result<string, Diagnostic> =
        let rawPath =
            match item.Source with
            | PathImport path ->
                if Path.IsPathRooted path then path else Path.Combine(baseDir, path)
            | ModuleImport name ->
                let relative =
                    name.Replace(".", string Path.DirectorySeparatorChar)
                        .Replace("/", string Path.DirectorySeparatorChar)
                Path.Combine(baseDir, relative + ".nyx")
        let fullPath =
            if Path.GetExtension rawPath = "" then rawPath + ".nyx" else rawPath
        if File.Exists fullPath then
            Ok (Path.GetFullPath fullPath)
        else
            Error (Diagnostics.error ($"Import not found: {rawPath}"))

    let private collectImports (module': Module) =
        module'
        |> List.choose (function
            | Import items -> Some items
            | _ -> None)
        |> List.collect id

    let private exportedValueNames (module': Module) =
        module'
        |> List.choose (function
            | Def (ValueDef(true, name, _, _)) -> Some name
            | _ -> None)
        |> Set.ofList

    let private exportedTypeNames (module': Module) =
        module'
        |> List.choose (function
            | Def (TypeDef(name, modifiers, _, _)) when modifiers |> List.contains Export -> Some name
            | _ -> None)
        |> Set.ofList

    let private mergeTypeDefs (target: Map<string, Ty * bool>) (incoming: Map<string, Ty * bool>) =
        incoming
        |> Map.fold (fun acc key value ->
            if Map.containsKey key acc then acc else acc |> Map.add key value) target

    let private extendEnvWithImport (env: TypeEnv) (prefix: string) (types: Map<string, Ty>) =
        types
        |> Map.fold (fun acc name ty ->
            let qualified = $"{prefix}.{name}"
            acc |> TypeEnv.extend qualified (TypeEnv.mono ty)) env

    let private parseWithDiagnostics (source: string) : Result<Module, Diagnostic list> =
        match parseModule source with
        | Result.Ok ast -> Ok ast
        | Result.Error err -> Error [ Diagnostics.error err ]

    let rec private typecheckFile (filePath: string) (visited: Set<string>) : Result<TypedModule, Diagnostic list> =
        let fullPath = Path.GetFullPath filePath
        if visited.Contains fullPath then
            Error [ Diagnostics.error ($"Import cycle detected for {fullPath}") ]
        else
            let source = File.ReadAllText fullPath
            match parseWithDiagnostics source with
            | Result.Error diagnostics -> Error diagnostics
            | Result.Ok ast ->
                let desugared = ast
                let imports = collectImports desugared
                let baseDir = Path.GetDirectoryName fullPath
                let mutable env = TypeEnv.empty
                let mutable state = NyxCompiler.Infer.emptyState
                let mutable errors: Diagnostic list = []
                for item in imports do
                    match resolveImportPath baseDir item with
                    | Error diag -> errors <- errors @ [ diag ]
                    | Ok importPath ->
                        match typecheckFile importPath (visited.Add fullPath) with
                        | Error diags -> errors <- errors @ diags
                        | Ok typed ->
                            let prefix = importPrefix item
                            let exportedValues = exportedValueNames typed.Module
                            let exportedTypes = exportedTypeNames typed.Module
                            let filteredTypes =
                                typed.Types
                                |> Map.filter (fun name _ -> exportedValues.Contains name)
                            let filteredTypeDefs =
                                typed.TypeDefs
                                |> Map.filter (fun name _ -> exportedTypes.Contains name)
                            env <- extendEnvWithImport env prefix filteredTypes
                            state <- { state with TypeDefs = mergeTypeDefs state.TypeDefs filteredTypeDefs }
                if not errors.IsEmpty then
                    Error errors
                else
                    match NyxCompiler.Infer.inferModuleWithEnv env state desugared with
                    | Error diagnostics -> Error diagnostics
                    | Ok (types, items, (state: NyxCompiler.Infer.InferState)) ->
                        match Unifier.unify state.Constraints with
                        | Ok subst ->
                            let resolvedTypes =
                                types
                                |> Map.map (fun _ ty -> Unifier.apply subst ty)
                            let resolvedTypeDefs =
                                state.TypeDefs
                                |> Map.map (fun _ (ty, isPrivate) -> Unifier.apply subst ty, isPrivate)
                            let rec applyTypedExpr (typedExpr: TypedExpr) =
                                let mappedBody = typedExpr.Body |> Option.map applyTypedExpr
                                let mappedStatements =
                                    typedExpr.Statements
                                    |> Option.map (List.map applyTypedStatement)
                                let mappedMatchArms =
                                    typedExpr.MatchArms
                                    |> Option.map (List.map applyTypedMatchArm)
                                { typedExpr with
                                    Type = Unifier.apply subst typedExpr.Type
                                    Body = mappedBody
                                    Statements = mappedStatements
                                    MatchArms = mappedMatchArms }
                            and applyTypedStatement statement =
                                match statement with
                                | TypedDefStatement(isExport, name, typeOpt, expr) ->
                                    TypedDefStatement(isExport, name, typeOpt, applyTypedExpr expr)
                                | TypedExprStatement expr -> TypedExprStatement (applyTypedExpr expr)
                                | TypedUseStatement(binding, exprOpt) ->
                                    let mappedExpr = exprOpt |> Option.map applyTypedExpr
                                    TypedUseStatement(binding, mappedExpr)
                                | TypedImportStatement _
                                | TypedTypeDefStatement _ -> statement
                            and applyTypedMatchArm (patterns, expr) =
                                let mappedPatterns =
                                    patterns
                                    |> List.map (fun pattern ->
                                        { pattern with Type = Unifier.apply subst pattern.Type })
                                (mappedPatterns, applyTypedExpr expr)

                            let resolvedItems =
                                items
                                |> List.map (fun item ->
                                    match item with
                                    | TypedExprItem expr -> TypedExprItem (applyTypedExpr expr)
                                    | TypedDef (TypedValueDef(isExport, name, typeOpt, expr)) ->
                                        TypedDef (TypedValueDef(isExport, name, typeOpt, applyTypedExpr expr))
                                    | TypedDef (TypedTypeDef _)
                                    | TypedImport _
                                    | TypedModuleDecl _ -> item)
                            Ok { Module = desugared
                                 Types = resolvedTypes
                                 TypeDefs = resolvedTypeDefs
                                 Items = resolvedItems }
                        | Error message ->
                            Error [ Diagnostics.error message ]
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
                let rec applyTypedExpr (typedExpr: TypedExpr) =
                    let mappedBody = typedExpr.Body |> Option.map applyTypedExpr
                    let mappedStatements =
                        typedExpr.Statements
                        |> Option.map (List.map applyTypedStatement)
                    let mappedMatchArms =
                        typedExpr.MatchArms
                        |> Option.map (List.map applyTypedMatchArm)
                    { typedExpr with
                        Type = Unifier.apply subst typedExpr.Type
                        Body = mappedBody
                        Statements = mappedStatements
                        MatchArms = mappedMatchArms }
                and applyTypedStatement statement =
                    match statement with
                    | TypedDefStatement(isExport, name, typeOpt, expr) ->
                        TypedDefStatement(isExport, name, typeOpt, applyTypedExpr expr)
                    | TypedExprStatement expr -> TypedExprStatement (applyTypedExpr expr)
                    | TypedUseStatement(binding, exprOpt) ->
                        let mappedExpr = exprOpt |> Option.map applyTypedExpr
                        TypedUseStatement(binding, mappedExpr)
                    | TypedImportStatement _
                    | TypedTypeDefStatement _ -> statement
                and applyTypedMatchArm (patterns, expr) =
                    let mappedPatterns =
                        patterns
                        |> List.map (fun pattern ->
                            { pattern with Type = Unifier.apply subst pattern.Type })
                    (mappedPatterns, applyTypedExpr expr)

                let resolvedTypeDefs =
                    state.TypeDefs
                    |> Map.map (fun _ (ty, isPrivate) -> Unifier.apply subst ty, isPrivate)
                let resolvedItems =
                    items
                    |> List.map (fun item ->
                        match item with
                        | TypedExprItem expr -> TypedExprItem (applyTypedExpr expr)
                        | TypedDef (TypedValueDef(isExport, name, typeOpt, expr)) ->
                            TypedDef (TypedValueDef(isExport, name, typeOpt, applyTypedExpr expr))
                        | TypedDef (TypedTypeDef _)
                        | TypedImport _
                        | TypedModuleDecl _ -> item)
                Ok { Module = desugared
                     Types = resolvedTypes
                     TypeDefs = resolvedTypeDefs
                     Items = resolvedItems }
            | Error message ->
                Error [ Diagnostics.error message ]

    let compileFile (filePath: string) : CompileResult =
        if not (File.Exists filePath) then
            { Phase = Parsed
              Diagnostics = [ Diagnostics.error ($"File not found: {filePath}") ]
              Typed = None }
        else
            match typecheckFile filePath Set.empty with
            | Result.Error diagnostics ->
                { Phase = Typed
                  Diagnostics = diagnostics
                  Typed = None }
            | Result.Ok typed ->
                { Phase = Typed
                  Diagnostics = []
                  Typed = Some typed }

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
