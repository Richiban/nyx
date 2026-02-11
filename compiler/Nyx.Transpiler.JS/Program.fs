open System
open System.IO
open Parser.Program
open Transpiler.CodeGen
open NyxCompiler

let private printDiagnostics (diagnostics: Diagnostic list) =
    diagnostics
    |> List.iter (fun diag ->
        let severity =
            match diag.Severity with
            | ErrorSeverity -> "error"
            | WarningSeverity -> "warning"
        printfn "[%s] %s" severity diag.Message)

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Usage: Transpiler <input.nyx> [output.js] [--no-typecheck]"
        printfn "  Transpiles a Nyx source file to JavaScript"
        1
    else
        let args = argv |> Array.toList
        let skipTypecheck = args |> List.exists (fun arg -> arg = "--no-typecheck" || arg = "--skip-typecheck")
        let fileArgs = args |> List.filter (fun arg -> not (arg.StartsWith("--")))
        let inputFile = fileArgs |> List.tryHead |> Option.defaultValue ""
        let outputFile =
            match fileArgs with
            | _ :: output :: _ -> output
            | _ -> inputFile + ".js"
        
        if String.IsNullOrWhiteSpace inputFile || not (File.Exists inputFile) then
            printfn "Error: Input file not found: %s" inputFile
            1
        else
            try
                if skipTypecheck then
                    let sourceCode = File.ReadAllText(inputFile)
                    match parseModule sourceCode with
                    | Result.Ok ast ->
                        let desugared = Compiler.desugar ast
                        let jsCode = transpileModule desugared
                        File.WriteAllText(outputFile, jsCode)
                        printfn "Successfully transpiled %s -> %s" inputFile outputFile
                        0
                    | Result.Error err ->
                        printfn "Parse error: %s" err
                        1
                else
                    let result = Compiler.compileFile inputFile
                    if not result.Diagnostics.IsEmpty then
                        printDiagnostics result.Diagnostics
                        1
                    else
                        match result.Typed with
                        | Some typed ->
                            let jsCode = transpileModule typed.Module
                            File.WriteAllText(outputFile, jsCode)
                            printfn "Successfully transpiled %s -> %s" inputFile outputFile
                            0
                        | None ->
                            printfn "Error: Unable to transpile without a typed module."
                            1
            with
            | ex ->
                printfn "Error: %s" ex.Message
                1
