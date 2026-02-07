module ParserTests.Features.Imports

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse import feature files`` () =
    parseFeatureNyxFiles "Imports"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
