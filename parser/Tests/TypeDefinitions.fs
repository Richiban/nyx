module NyxParser.Tests.TypeDefinitions

open NUnit.Framework

open NyxParser.Types
open NyxParser.TypeExpressions
open NyxParser.Modules 
open NyxParser.Statements
open NyxParser.Tests.Utils


[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test functions, tuples, unions and tags``() =
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
let ``Test tag types in a nested union``() =
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
let ``Test that extra parens do not affect outcome``() =
    let actual =
        runParser
            typeParser
            "(((string)))"

    let expected =
        Identifier (TypeIdentifier "string")
    
    Assert.AreEqual(expected, actual)


[<Test>]
let ``Test tuples, unions and tags in a function type`` () =
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
