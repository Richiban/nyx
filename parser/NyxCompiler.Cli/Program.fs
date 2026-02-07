module NyxCompiler.Cli

open System
open System.IO
open NyxCompiler

let private printDiagnostics (diagnostics: Diagnostic list) =
    if diagnostics.IsEmpty then
        printfn "No diagnostics."
    else
        diagnostics
        |> List.iter (fun diag ->
            let severity =
                match diag.Severity with
                | ErrorSeverity -> "error"
                | WarningSeverity -> "warning"
            printfn "[%s] %s" severity diag.Message)

let private printTypes (types: Map<string, Ty>) =
    if types.IsEmpty then
        printfn "(no inferred types)"
    else
        types
        |> Map.toList
        |> List.sortBy fst
        |> List.iter (fun (name, ty) -> printfn "%s : %A" name ty)

[<EntryPoint>]
let main args =
    match args with
    | [| filePath |] ->
        if not (File.Exists filePath) then
            printfn "File not found: %s" filePath
            2
        else
            let source = File.ReadAllText filePath
            let result = Compiler.compile source
            printfn "Phase: %A" result.Phase
            printDiagnostics result.Diagnostics
            match result.Typed with
            | Some typed ->
                printfn "Inferred types:"
                printTypes typed.Types
                0
            | None -> 1
    | _ ->
        printfn "Usage: nyx-compiler <path-to-file.nyx>"
        2
