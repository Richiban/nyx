module TranspilerTests.Features.Core.Pipes

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile simple pipe`` () =
    let expr = Pipe(IdentifierExpr("value", None), "double", None, [])
    let result = transpileExpression expr
    result |> should equal "double(value)"

[<Fact>]
let ``Transpile pipe with args`` () =
    let expr = Pipe(
        LiteralExpr (IntLit 5),
        "add",
        None,
        [TupleExpr [LiteralExpr (IntLit 10)]]
    )
    let result = transpileExpression expr
    result |> should equal "add(5, 10)"
