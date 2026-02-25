module TranspilerWasmTests.E2E

open System.IO
open Xunit
open TranspilerWasmTestHelpers

let private e2eRootDir = Path.Combine(__SOURCE_DIRECTORY__)

let private loadE2EFile testName fileName =
    let filePath = Path.Combine(e2eRootDir, testName, fileName)
    if not (File.Exists(filePath)) then
        failwith $"E2E test file not found: {filePath}"
    File.ReadAllText(filePath)

let private runE2ETest testName =
    let source = loadE2EFile testName "input.nyx"
    let expected = loadE2EFile testName "expected.wat" |> normalizeOutput
    let actual = transpileWat source |> normalizeOutput
    Assert.Equal(expected, actual)

[<Fact>]
let ``E2E - one - hello world`` () =
    runE2ETest "one"

[<Fact>]
let ``E2E - two - pipe expression`` () =
    runE2ETest "two"

[<Fact>]
let ``E2E - three - booleans and conditionals`` () =
    runE2ETest "three"
