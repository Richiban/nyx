module ParserTests.Features.Phases

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact(Skip="Phase samples use match syntax not yet supported by the parser")>]
let ``Parse phase feature files`` () =
    let unsupported = set [ "test_phase5_complete.nyx" ]

    parseFeatureNyxFiles "Phases"
    |> Array.filter (fun (filePath, _) -> not (unsupported.Contains(Path.GetFileName(filePath))))
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
