module Nyx.Parser.Tests.ExpressionsTests

open NUnit.Framework

open Nyx.Parser.Statements
open Nyx.Parser.AST
open Nyx.Parser.Tests.Utils

[<Test>]
let ``Test int literal expression``() =
    let actual =
        runParser pstatement "1"

    let expected =
        Expr (IntLiteral 1)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test expression block``() =
    let actual =
        runParser blockParser "
        1
        2
        3
"

    let expected = Block [Expr (IntLiteral 1); Expr (IntLiteral 2); Expr (IntLiteral 3)]

    Assert.AreEqual(expected, actual)