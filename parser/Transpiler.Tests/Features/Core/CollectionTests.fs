module TranspilerTests.Features.Core.Collections

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile tuple to array`` () =
    let expr = TupleExpr [LiteralExpr (IntLit 1); LiteralExpr (IntLit 2); LiteralExpr (IntLit 3)]
    let result = transpileExpression expr
    result |> should equal "[1, 2, 3]"

[<Fact>]
let ``Transpile list to array`` () =
    let expr = ListExpr [LiteralExpr (IntLit 1); LiteralExpr (IntLit 2)]
    let result = transpileExpression expr
    result |> should equal "[1, 2]"

[<Fact>]
let ``Transpile record to object`` () =
    let expr = RecordExpr [
        NamedField("x", LiteralExpr (IntLit 10))
        NamedField("y", LiteralExpr (IntLit 20))
    ]
    let result = transpileExpression expr
    result |> should equal "{ x: 10, y: 20 }"
