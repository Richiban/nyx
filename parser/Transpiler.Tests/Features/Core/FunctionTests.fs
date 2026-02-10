module TranspilerTests.Features.Core.Functions

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile function call with no args`` () =
    let expr = FunctionCall("getCurrentTime", None, [])
    let result = transpileExpression expr
    result |> should equal "getCurrentTime()"

[<Fact>]
let ``Transpile function call with tuple args`` () =
    let expr = FunctionCall("add", None, [TupleExpr [LiteralExpr (IntLit 5); LiteralExpr (IntLit 10)]])
    let result = transpileExpression expr
    result |> should equal "add(5, 10)"

[<Fact>]
let ``Transpile lambda`` () =
    let expr = Lambda([("x", None); ("y", None)], BinaryOp("+", IdentifierExpr("x", None), IdentifierExpr("y", None)))
    let result = transpileExpression expr
    result |> should equal "(x, y) => (x + y)"

[<Fact>]
let ``Transpile dbg`` () =
    let expr = FunctionCall("dbg", None, [IdentifierExpr("x", None)])
    let result = transpileExpression expr
    result |> should equal "(() => { const __dbg = x; console.log(__dbg); return __dbg; })()"
