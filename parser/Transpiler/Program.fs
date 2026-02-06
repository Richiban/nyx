open System
open System.IO
open Parser.Program
open Transpiler.CodeGen

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Usage: Transpiler <input.nyx> [output.js]"
        printfn "  Transpiles a Nyx source file to JavaScript"
        1
    else
        let inputFile = argv.[0]
        let outputFile = 
            if argv.Length > 1 then argv.[1]
            else Path.ChangeExtension(inputFile, ".js")
        
        if not (File.Exists inputFile) then
            printfn "Error: Input file not found: %s" inputFile
            1
        else
            try
                let sourceCode = File.ReadAllText(inputFile)
                
                match parseModule sourceCode with
                | Result.Ok ast ->
                    let jsCode = transpileModule ast
                    File.WriteAllText(outputFile, jsCode)
                    printfn "Successfully transpiled %s -> %s" inputFile outputFile
                    0
                | Result.Error err ->
                    printfn "Parse error: %s" err
                    1
            with
            | ex ->
                printfn "Error: %s" ex.Message
                1
