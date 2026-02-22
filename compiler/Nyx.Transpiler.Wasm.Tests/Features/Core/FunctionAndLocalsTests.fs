module TranspilerWasmTests.Features.Core.FunctionAndLocals

open Xunit
open TranspilerWasmTestHelpers

[<Fact>]
let ``Transpile function with parameters`` () =
    let source =
        "module M\n\n" +
        "def add = { x, y -> x + y }"

    let wat = transpileWat source

    Assert.Contains("(func $add (param $x i32) (param $y i32) (result i32)", wat)
    Assert.Contains("local.get $x", wat)
    Assert.Contains("local.get $y", wat)
    Assert.Contains("i32.add", wat)

[<Fact>]
let ``Transpile function call with arguments`` () =
    let source =
        "module M\n\n" +
        "def add = { x, y -> x + y }\n\n" +
        "def main = add(1, 2)"

    let wat = transpileWat source

    Assert.Contains("(func $main (result i32)", wat)
    Assert.Contains("i32.const 1", wat)
    Assert.Contains("i32.const 2", wat)
    Assert.Contains("call $add", wat)

[<Fact>]
let ``Transpile block local variable`` () =
    let source =
        "module M\n\n" +
        "def incTwice = { x ->\n" +
        "  def y = x + 1\n" +
        "  y + 1\n" +
        "}"

    let wat = transpileWat source

    Assert.Contains("(func $incTwice (param $x i32) (result i32) (local $y i32)", wat)
    Assert.Contains("local.set $y", wat)
    Assert.Contains("local.get $y", wat)

[<Fact>]
let ``Transpile top-level constant def`` () =
    let source =
        "module M\n\n" +
        "def one = 1"

    let wat = transpileWat source

    Assert.Contains("(func $one (result i32)", wat)
    Assert.Contains("i32.const 1", wat)
    Assert.Contains("(export \"one\" (func $one))", wat)
