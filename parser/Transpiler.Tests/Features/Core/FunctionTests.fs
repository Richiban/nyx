module TranspilerTests.Features.Core.Functions

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile function call with no args`` () =
    let expr = FunctionCall("getCurrentTime", [])
    let result = transpileExpression expr
    result |> should equal "getCurrentTime()"

[<Fact>]
let ``Transpile function call with tuple args`` () =
    let expr = FunctionCall("add", [TupleExpr [LiteralExpr (IntLit 5); LiteralExpr (IntLit 10)]])
    let result = transpileExpression expr
    result |> should equal "add(5, 10)"

[<Fact>]
let ``Transpile lambda`` () =
    let expr = Lambda(["x"; "y"], BinaryOp("+", IdentifierExpr "x", IdentifierExpr "y"))
    let result = transpileExpression expr
    result |> should equal "(x, y) => (x + y)"
