module ParserTests.Features.Literals

open Xunit
open FsUnit.Xunit
open Parser.Program
open ParserTestHelpers

[<Fact>]
let ``Parse literals.nyx file`` () =
    let result = parseTestFile "literals.nyx"

    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 6  // 1 module + 5 defs
        match module'.[0] with
        | ModuleDecl name -> name |> should equal "Literals"
        | _ -> failwith "Expected ModuleDecl"

        let defs = module' |> List.tail
        defs |> should haveLength 5
    | Result.Error err -> failwith $"Parse failed: {err}"
