module ParserTests.Features.Multiline

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse multiline feature files`` () =
    parseFeatureNyxFiles "Multiline"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
