module TranspilerTests.Features.Core.ControlFlow

open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile if expression`` () =
    let expr = IfExpr(
        LiteralExpr (BoolLit true),
        LiteralExpr (IntLit 1),
        LiteralExpr (IntLit 2)
    )
    let result = transpileExpression expr
    result |> should equal "(true ? 1 : 2)"

[<Fact>]
let ``Transpile tag without payload`` () =
    let expr = TagExpr("None", None)
    let result = transpileExpression expr
    result |> should equal "{ tag: \"None\" }"

[<Fact>]
let ``Transpile tag with payload`` () =
    let expr = TagExpr("Some", Some (LiteralExpr (IntLit 42)))
    let result = transpileExpression expr
    result |> should equal "{ tag: \"Some\", value: 42 }"
