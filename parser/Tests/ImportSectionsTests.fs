module Nyx.Parser.Tests.ImportSections

open NUnit.Framework

open Nyx.Parser.Modules
open Nyx.Parser.AST
open Nyx.Parser.Tests.Utils

[<Test>]
let ``Test imports``() =
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
