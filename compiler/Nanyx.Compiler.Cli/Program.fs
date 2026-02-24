module NyxCompiler.Cli

open System
open System.IO
open NyxCompiler
open Transpiler.Wasm.CodeGen

type CliOptions =
    { InputFile: string option
      Target: string
      OutputPath: string option }

let private defaultOptions =
    { InputFile = None
      Target = "check"
      OutputPath = None }

let rec private parseArgs (options: CliOptions) (args: string list) : Result<CliOptions, string> =
    match args with
    | [] -> Ok options
    | "--target" :: value :: tail ->
        parseArgs { options with Target = value.Trim().ToLowerInvariant() } tail
    | "--out" :: value :: tail ->
        parseArgs { options with OutputPath = Some value } tail
    | flag :: _ when flag.StartsWith("--") ->
        Error ($"Unknown option: {flag}")
    | input :: tail ->
        match options.InputFile with
        | Some _ -> Error "Multiple input files provided."
        | None -> parseArgs { options with InputFile = Some input } tail

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
            let rangeText =
                match diag.Range with
                | Some (line, col) -> $"({line + 1}:{col + 1}) "
                | None -> ""
            printfn "[%s] %s%s" severity rangeText diag.Message)

let private printTypes (types: Map<string, Ty>) =
    if types.IsEmpty then
        printfn "(no inferred types)"
    else
        types
        |> Map.toList
        |> List.sortBy fst
        |> List.iter (fun (name, ty) -> printfn "%s : %A" name ty)

let private defaultWasmOutputPath (inputFile: string) =
    let directory = Path.GetDirectoryName inputFile
    let stem = Path.GetFileNameWithoutExtension inputFile
    Path.Combine(directory, stem + ".wat")

let private usage () =
    printfn "Usage: nyx-compiler <path-to-file.nyx> [--target check|wasm] [--out output-path]"
    printfn "  --target check   Runs parse/typecheck and prints inferred types (default)"
    printfn "  --target wasm    Emits WebAssembly text format (.wat)"

[<EntryPoint>]
let main args =
    match parseArgs defaultOptions (args |> Array.toList) with
    | Error message ->
        printfn "%s" message
        usage ()
        2
    | Ok options ->
        match options.InputFile with
        | None ->
            usage ()
            2
        | Some filePath ->
            if not (File.Exists filePath) then
                printfn "File not found: %s" filePath
                2
            else
                let result = Compiler.compileFile filePath
                printfn "Phase: %A" result.Phase
                printDiagnostics result.Diagnostics

                match options.Target with
                | "check" ->
                    match result.Typed with
                    | Some typed ->
                        printfn "Inferred types:"
                        printTypes typed.Types
                        0
                    | None -> 1
                | "wasm" ->
                    match result.Typed with
                    | Some typed when result.Diagnostics |> List.exists (fun d -> d.Severity = ErrorSeverity) ->
                        1
                    | Some typed ->
                        let outputPath = options.OutputPath |> Option.defaultValue (defaultWasmOutputPath filePath)
                        let wat = transpileModuleToWat typed.Module
                        File.WriteAllText(outputPath, wat)
                        printfn "WASM (WAT) emitted: %s" outputPath
                        0
                    | None -> 1
                | other ->
                    printfn "Unsupported target: %s" other
                    usage ()
                    2
