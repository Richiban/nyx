module Nyx.Parser.Tests.TypeExpressions

open NUnit.Framework

open FParsec
open Nyx.Parser.AST
open Nyx.Parser.Common
open Nyx.Parser.TypeExpressions
open Nyx.Parser.Modules 
open Nyx.Parser.Statements
open Nyx.Parser.Tests.Utils

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1() =
    let actual =
        runParser 
            typeDefinitionParser
            "type A = (filename: string, mode: #readonly | #readwrite) -> ()"

    let expected =
        { 
            name = TypeIdentifier "A"
            value = FunctionType
                (Tuple
                    [NamedField
                        (CommonIdentifier "filename",
                        Tuple
                            [Identifier (TypeIdentifier "string");
                            NamedField
                                (CommonIdentifier "mode",
                                    Union
                                        (Tag (CommonIdentifier "readonly"),
                                        Tag (CommonIdentifier "readwrite")))])], Unit) }

    Assert.AreEqual(expected, actual)

[<Test>]
let Test2() =
    let actual = 
        runParser 
            typeDefinitionParser
            "type OptionString = (#none | (#some, string))"

    let expected = 
        { 
            name = TypeIdentifier "OptionString"
            value = Union(
                (Tag (CommonIdentifier "none")),
                Tuple [
                        Tag (CommonIdentifier "some")
                        Identifier (TypeIdentifier "string")])
        }

    Assert.AreEqual(expected, actual)

[<Test>]
let Test3() =
    let actual =
        runParser
            typeParser
            "#none | (#some, string) -> ()"

    let expected =
        FunctionType
            (Union(
                (Tag (CommonIdentifier "none")),
                Tuple [
                        Tag (CommonIdentifier "some")
                        Identifier (TypeIdentifier "string")]),
            Unit)

    Assert.AreEqual(expected, actual)

[<Test>]
let Test4() =
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
    

[<Test>]
let Test5() =
    let actual =
        runParser
            typeParser
            "string -> list(string)"

    let expected =
        FunctionType
            (Identifier (TypeIdentifier "string"), 
             TypeCall (Identifier (TypeIdentifier "list"), Identifier (TypeIdentifier "string")))
    
    Assert.AreEqual(expected, actual)

[<Test>]
let Test6() =
    let actual =
        runFailingParser
            typeParser
            "string -> list string"

    let expected = 
        """Error in Ln: 1 Col: 16
string -> list string
               ^
Expecting: end of input, '&', '->' or '|'
"""

    Assert.AreEqual(expected, actual)

[<Test>]
let Test7() =
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
let Test8() =
    let actual =
        runParser
            typeParser
            "(((string)))"

    let expected =
        Identifier (TypeIdentifier "string")
    
    Assert.AreEqual(expected, actual)


[<Test>]
let Test9() =
    let actual =
        runParser 
            importSectionParser
            @"import 
            ""test1""
            ""test2""
        "

    let expected =
        ImportSection
            [ImportTarget "test1"
             ImportTarget "test2"]

    Assert.AreEqual(expected, actual)

[<Test>]
let Test10() =
    let actual = 
        runParser moduleParser @"module Test

import
    ""test1""
    ""test2""
    test3
"
    let expected = { 
        name = ModuleIdentifier "Test"
        imports = Some (ImportSection [ImportTarget "test1"; ImportTarget "test2"; ImportTarget "test3"])
        definitions = []
    }

    Assert.AreEqual(expected, actual)


[<Test>]
let Test11 () =
    let actual =
        runParser 
            typeDefinitionParser
            "type A = (filename: string, mode: #readonly | #readwrite) -> ()"

    

    let expected = 
        { 
            name = TypeIdentifier "A"
            value = FunctionType
                (Tuple
                    [NamedField
                        (CommonIdentifier "filename",
                        Tuple
                            [Identifier (TypeIdentifier "string");
                            NamedField
                                (CommonIdentifier "mode",
                                    Union
                                        (Tag (CommonIdentifier "readonly"),
                                        Tag (CommonIdentifier "readwrite")))])], Unit) }

    Assert.AreEqual(expected, actual)
