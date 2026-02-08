module ParserTests.Features.Contexts

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse context feature files`` () =
    parseFeatureNyxFiles "Contexts"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
