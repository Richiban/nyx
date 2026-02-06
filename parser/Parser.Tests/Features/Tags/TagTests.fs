module ParserTests.Features.Tags

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact(Skip="Tag samples include match syntax not yet supported by the parser")>]
let ``Parse tag feature files`` () =
    let unsupported = set [ "test_tag_complete.nyx" ]

    parseFeatureNyxFiles "Tags"
    |> Array.filter (fun (filePath, _) -> not (unsupported.Contains(Path.GetFileName(filePath))))
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
