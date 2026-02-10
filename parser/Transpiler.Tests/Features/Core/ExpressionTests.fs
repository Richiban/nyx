module TranspilerTests.Features.Core.Expressions

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile identifier`` () =
    let expr = IdentifierExpr("x", None)
    let result = transpileExpression expr
    result |> should equal "x"

[<Fact>]
let ``Transpile member access`` () =
    let expr = MemberAccess(IdentifierExpr("point", None), "x", None)
    let result = transpileExpression expr
    result |> should equal "point.x"

[<Fact>]
let ``Transpile nested member access`` () =
    let expr = MemberAccess(MemberAccess(IdentifierExpr("outer", None), "inner", None), "value", None)
    let result = transpileExpression expr
    result |> should equal "outer.inner.value"

[<Fact>]
let ``Transpile binary operation`` () =
    let expr = BinaryOp("+", LiteralExpr (IntLit 1), LiteralExpr (IntLit 2))
    let result = transpileExpression expr
    result |> should equal "(1 + 2)"

[<Fact>]
let ``Transpile equality to triple equals`` () =
    let expr = BinaryOp("==", IdentifierExpr("x", None), LiteralExpr (IntLit 5))
    let result = transpileExpression expr
    result |> should equal "(x === 5)"

[<Fact>]
let ``Transpile not equals to not triple equals`` () =
    let expr = BinaryOp("!=", IdentifierExpr("x", None), LiteralExpr (IntLit 5))
    let result = transpileExpression expr
    result |> should equal "(x !== 5)"
