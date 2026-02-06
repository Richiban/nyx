module ParserTests.Features.Records

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse record feature files`` () =
    parseFeatureNyxFiles "Records"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
