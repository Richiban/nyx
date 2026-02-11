module ParserTests.Features.If

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact>]
let ``Parse if feature files`` () =
    let unsupported = set [ "test_if_advanced.nyx" ]

    parseFeatureNyxFiles "If"
    |> Array.filter (fun (filePath, _) -> not (unsupported.Contains(Path.GetFileName(filePath))))
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
