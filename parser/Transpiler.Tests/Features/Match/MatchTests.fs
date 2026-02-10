module TranspilerTests.Features.Match

open Xunit
open Parser.Program
open Transpiler.CodeGen

[<Fact>]
let ``Transpile match expression with literals`` () =
    let expr = Match(
        [IdentifierExpr("x", None)],
        [
            ([LiteralPattern (IntLit 1)], LiteralExpr (IntLit 10))
            ([WildcardPattern], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("const _match0 = x;", result)
    Assert.Contains("if (_match0 === 1)", result)
    Assert.Contains("return 10;", result)
    Assert.Contains("return 0;", result)

[<Fact>]
let ``Transpile match expression with tag payload`` () =
    let expr = Match(
        [IdentifierExpr("value", None)],
        [
            ([TagPattern ("Some", Some (IdentifierPattern "x"))], IdentifierExpr("x", None))
            ([TagPattern ("None", None)], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("_match0.tag === \"Some\"", result)
    Assert.Contains("const x = _match0.value;", result)
    Assert.Contains("_match0.tag === \"None\"", result)

[<Fact>]
let ``Transpile match expression with guard`` () =
    let expr = Match(
        [IdentifierExpr("x", None)],
        [
            ([GuardPattern (">", LiteralExpr (IntLit 5))], LiteralExpr (IntLit 1))
            ([ElsePattern], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("_match0 > 5", result)
    Assert.Contains("return 1;", result)
    Assert.Contains("return 0;", result)

[<Fact>]
let ``Transpile match expression with list splat middle`` () =
    let expr = Match(
        [IdentifierExpr("values", None)],
        [
            ([ListSplatMiddle ([LiteralPattern (IntLit 1)], [LiteralPattern (IntLit 3)])], LiteralExpr (IntLit 1))
            ([ElsePattern], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("Array.isArray(_match0)", result)
    Assert.Contains("_match0.length >= 2", result)
    Assert.Contains("_match0[0] === 1", result)
    Assert.Contains("_match0[_match0.length - 1] === 3", result)

[<Fact>]
let ``Transpile match expression with record member patterns`` () =
    let expr = Match(
        [IdentifierExpr("point", None)],
        [
            ([RecordMemberPattern [("x", LiteralPattern (IntLit 1)); ("y", IdentifierPattern "y")]], IdentifierExpr("y", None))
            ([ElsePattern], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("\"x\" in _match0", result)
    Assert.Contains("\"y\" in _match0", result)
    Assert.Contains("_match0.x === 1", result)
    Assert.Contains("const y = _match0.y;", result)

[<Fact>]
let ``Transpile match expression with record positional patterns`` () =
    let expr = Match(
        [IdentifierExpr("point", None)],
        [
            ([RecordPattern ("Point", [LiteralPattern (IntLit 1); IdentifierPattern "y"])], IdentifierExpr("y", None))
            ([ElsePattern], LiteralExpr (IntLit 0))
        ]
    )
    let result = transpileExpression expr
    Assert.Contains("_match0[0] === 1", result)
    Assert.Contains("const y = _match0[1];", result)
