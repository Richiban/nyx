module TranspilerWasmTestHelpers

open Parser.Program
open Transpiler.Wasm.CodeGen

let transpileWat (source: string) =
    match parseModule source with
    | Result.Ok ast -> transpileModuleToWat ast
    | Result.Error err -> failwith $"Parse error: {err}"
