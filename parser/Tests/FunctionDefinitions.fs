module NyxParser.Tests.FunctionDefinitions

open NUnit.Framework

open NyxParser.Statements
open NyxParser.Types
open NyxParser.Tests.Utils

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
