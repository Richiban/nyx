module ParserTests.Features.Bindings

open Xunit
open FsUnit.Xunit
open ParserTestHelpers

[<Fact>]
let ``Parse binding feature files`` () =
    parseFeatureNyxFiles "Bindings"
    |> Array.iter (fun (filePath, result) ->
        result |> isOk |> should equal true
        match result with
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
        | _ -> ()
    )
