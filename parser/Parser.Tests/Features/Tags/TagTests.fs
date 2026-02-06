module ParserTests.Features.Tags

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact(Skip="Tag fixtures include inline match forms not yet parsed")>]
let ``Parse tag feature files`` () =
    parseFeatureNyxFiles "Tags"
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
