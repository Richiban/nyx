module Nyx.Parser.Tests.FunctionDefinitions

open NUnit.Framework

open Nyx.Parser.Statements
open Nyx.Parser.AST
open Nyx.Parser.Tests.Utils

[<Test>]
let ``Test a``() =
    let actual =
        runParser definitionParser "def f() -> 1"

    let expected =
        ImportSection
            [ImportTarget "test1"
             ImportTarget "test2"]

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test b``() =
    let actual =
        runParser definitionParser "def a = 1"

    let expected =
        ImportSection [
            ImportTarget "test1"
            ImportTarget "test2"
        ]

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test c``() =
    let actual =
        runParser definitionParser @"def a = 
        1"

    let expected =
        ImportSection
            [ImportTarget "test1"
             ImportTarget "test2"]

    Assert.AreEqual(expected, actual)
