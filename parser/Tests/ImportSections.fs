module NyxParser.Tests.ImportSections

open NUnit.Framework

open NyxParser.Modules
open NyxParser.Types
open NyxParser.Tests.Utils

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
