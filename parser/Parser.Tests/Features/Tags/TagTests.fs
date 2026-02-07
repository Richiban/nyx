module ParserTests.Features.Tags

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact>]
let ``Parse tag feature files`` () =
    parseFeatureNyxFiles "Tags"
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
