module ParserTests.Features.Samples

open Xunit
open FsUnit.Xunit
open ParserTestHelpers
open System.IO

[<Fact>]
let ``Parse sample feature files`` () =
    let unsupported = set [ "real-world.nyx" ]

    parseFeatureNyxFiles "Samples"
    |> Array.filter (fun (filePath, _) -> not (unsupported.Contains(Path.GetFileName(filePath))))
    |> Array.iter (fun (filePath, result) ->
        match result with
        | Result.Ok _ -> ()
        | Result.Error err -> failwith $"Parse failed for {filePath}: {err}"
    )
