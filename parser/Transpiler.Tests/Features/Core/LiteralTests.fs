module TranspilerTests.Features.Core.Literals

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile integer literal`` () =
    let expr = LiteralExpr (IntLit 42)
    let result = transpileExpression expr
    result |> should equal "42"

[<Fact>]
let ``Transpile string literal`` () =
    let expr = LiteralExpr (StringLit "hello")
    let result = transpileExpression expr
    result |> should equal "\"hello\""

[<Fact>]
let ``Transpile boolean literal`` () =
    let expr = LiteralExpr (BoolLit true)
    let result = transpileExpression expr
    result |> should equal "true"

[<Fact>]
let ``Transpile float literal`` () =
    let expr = LiteralExpr (FloatLit 3.14)
    let result = transpileExpression expr
    result |> should equal "3.14"
