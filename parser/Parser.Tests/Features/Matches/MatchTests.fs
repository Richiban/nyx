module ParserTests.Features.Matches

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact(Skip="Match samples include syntax not yet supported by the parser")>]
let ``Parse match feature files`` () =
    let unsupported = set [ "test_match_basic.nyx" ]

    parseFeatureNyxFiles "Matches"
    |> Array.filter (fun (filePath, _) -> not (unsupported.Contains(Path.GetFileName(filePath))))
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
