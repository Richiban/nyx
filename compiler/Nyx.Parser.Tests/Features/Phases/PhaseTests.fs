module ParserTests.Features.Phases

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact>]
let ``Parse phase feature files`` () =
    parseFeatureNyxFiles "Phases"
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
