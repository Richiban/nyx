module Nyx.Parser.Tests.ModuleDefinitions

open NUnit.Framework

open Nyx.Parser.Modules
open Nyx.Parser.AST
open Nyx.Parser.Tests.Utils


[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test module definition with imports only``() =
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
let ``Test module definition with imports and type definitions``() =
    let actual = 
        runParser moduleParser @"module Test

import
    ""test1""
    ""test2""
    test3

type A = string

type B = (a: string, b: int) -> (c: string, d: int)
"
    let expected = { 
        name = ModuleIdentifier "Test"
        imports = Some (ImportSection [ImportTarget "test1"; ImportTarget "test2"; ImportTarget "test3"])
        definitions = []
    }

    Assert.AreEqual(expected, actual)


    