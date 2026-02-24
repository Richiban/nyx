module ParserTests.Features.Tuples

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse tuple feature files`` () =
    parseFeatureNyxFiles "Tuples"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
