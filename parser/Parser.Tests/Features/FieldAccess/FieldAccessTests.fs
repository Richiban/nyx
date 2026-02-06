module ParserTests.Features.FieldAccess

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse field access feature files`` () =
    parseFeatureNyxFiles "FieldAccess"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
