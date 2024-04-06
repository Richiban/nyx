module NyxParser.Tests.Types

open NUnit.Framework

open NyxParser.Types
open NyxParser.TypeExpressions
open NyxParser.Tests.Utils

[<Test>]
let ``Test a type call in a function``() =
    let actual =
        runParser
            typeParser
            "string -> list(string)"

    let expected =
        FunctionType (
            Identifier (TypeIdentifier "string"), 
            TypeCall (
                Identifier (TypeIdentifier "list"),
                Identifier (TypeIdentifier "string")
            )
        )
    
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Failure: Testing attempted typecall without parens`` () =
    let actual =
        runFailingParser
            typeParser
            "string -> list string"

    let expected = [|
        "Error in Ln: 1 Col: 16"
        "string -> list string"
        "               ^"
        "Expecting: end of input, '&', '->' or '|'"
        ""
    |]

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test simple function type with extra parens``() =
    let actual = 
        runParser
            typeParser
            "string -> (string)"

    let expected =
        FunctionType
            (Identifier (TypeIdentifier "string"), 
             Identifier (TypeIdentifier "string"))

    Assert.AreEqual(expected, actual)


[<Test>]
let ``Test simple function type``() =
    let actual = 
        runParser
            typeParser
            "string -> string"

    let expected =
        FunctionType
            (Identifier (TypeIdentifier "string"), 
             Identifier (TypeIdentifier "string"))

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Test a tuple in a type call in a function type``() =
    let actual =
        runParser
            typeParser
            "string -> list(string, int)"

    let expected =
        FunctionType (
            Identifier (TypeIdentifier "string"), 
            TypeCall (
                Identifier (TypeIdentifier "list"),
                Tuple [
                    Identifier (TypeIdentifier "string")
                    Identifier (TypeIdentifier "int")
                ]
            )
        )

    Assert.AreEqual(expected, actual)
    