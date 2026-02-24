module TranspilerWasmTestHelpers

open System.IO
open Parser.Program
open Transpiler.Wasm.CodeGen

let private testRootDir = __SOURCE_DIRECTORY__
let private featureRootDir = Path.Combine(testRootDir, "Features")

let normalizeOutput (value: string) =
    value.Replace("\r\n", "\n").TrimEnd()

let loadFeatureFile featureName fileName =
    let filePath = Path.Combine(featureRootDir, featureName, fileName)
    if not (File.Exists(filePath)) then
        failwith $"Test data file not found: {filePath}"
    File.ReadAllText(filePath)

let transpileWat (source: string) =
    match parseModule source with
    | Result.Ok ast -> transpileModuleToWat ast
    | Result.Error err -> failwith $"Parse error: {err}"

let assertFixture featureName baseName =
    let source = loadFeatureFile featureName (baseName + ".nyx")
    let expected = loadFeatureFile featureName (baseName + ".wat") |> normalizeOutput
    let actual = transpileWat source |> normalizeOutput
    if actual <> expected then
        failwith $"WAT output mismatch for {featureName}/{baseName}."
