module TranspilerTestHelpers

open System.IO
open Parser.Program
open Transpiler.CodeGen

let private testRootDir = __SOURCE_DIRECTORY__
let private featureRootDir = Path.Combine(testRootDir, "Features")

let normalizeOutput (value: string) =
    value.Replace("\r\n", "\n").TrimEnd()

let loadTextFile (path: string) =
    File.ReadAllText(path)

let loadFeatureFile featureName fileName =
    let filePath = Path.Combine(featureRootDir, featureName, fileName)
    if not (File.Exists(filePath)) then
        failwith $"Test data file not found: {filePath}"
    loadTextFile filePath

let transpileSource source =
    match parseModule source with
    | Result.Ok ast -> transpileModule ast
    | Result.Error err -> failwith $"Parse error: {err}"

let transpileFeatureFile featureName fileName =
    loadFeatureFile featureName fileName
    |> transpileSource

let assertFixture featureName baseName =
    let actual = transpileFeatureFile featureName (baseName + ".nyx") |> normalizeOutput
    let expected = loadFeatureFile featureName (baseName + ".js") |> normalizeOutput
    if actual <> expected then
        failwith $"Transpiled output mismatch for {featureName}/{baseName}."
