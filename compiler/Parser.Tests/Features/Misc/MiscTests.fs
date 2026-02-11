module ParserTests.Features.Misc

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse misc feature files`` () =
    parseFeatureNyxFiles "Misc"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
