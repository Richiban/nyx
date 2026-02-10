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

[<Fact>]
let ``Parse interpolated string literal`` () =
    let source = "def msg = \"Hello {name}\""
    match parseModule source with
    | Result.Error err -> failwith $"Parse failed: {err}"
    | Result.Ok module' ->
        match module' with
        | [Def (ValueDef(_, "msg", _, InterpolatedString parts))] ->
            parts |> should haveLength 2
        | _ -> failwith "Expected interpolated string"

[<Fact>]
let ``Parse single-quoted string without interpolation`` () =
    let source = "def msg = 'Hello {name}'"
    match parseModule source with
    | Result.Error err -> failwith $"Parse failed: {err}"
    | Result.Ok module' ->
        match module' with
        | [Def (ValueDef(_, "msg", _, LiteralExpr (StringLit value)))] ->
            value |> should equal "Hello {name}"
        | _ -> failwith "Expected plain string literal"

[<Fact>]
let ``Parse multiline string literal`` () =
    let source = "def msg = \"Hello\nworld\""
    match parseModule source with
    | Result.Error err -> failwith $"Parse failed: {err}"
    | Result.Ok module' ->
        match module' with
        | [Def (ValueDef(_, "msg", _, LiteralExpr (StringLit value)))] ->
            value |> should equal "Hello\nworld"
        | _ -> failwith "Expected multiline string literal"
