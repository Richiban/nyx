module ParserTests.Features.Types

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse type definition feature files`` () =
    parseFeatureNyxFiles "Types"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
