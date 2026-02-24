module TranspilerWasmTests.Features.ControlFlow.ControlFlowFixtureTests

open Xunit
open TranspilerWasmTestHelpers

[<Fact>]
let ``Fixture if-expression function`` () =
    assertFixture "ControlFlow" "if-expression"

[<Fact>]
let ``Fixture nested block locals`` () =
    assertFixture "ControlFlow" "nested-block-locals"

[<Fact>]
let ``Fixture comparison if-expression`` () =
    assertFixture "ControlFlow" "comparison-if"

[<Fact>]
let ``Fixture call chaining`` () =
    assertFixture "ControlFlow" "call-chaining"

[<Fact>]
let ``Fixture tag comparison if-expression`` () =
    assertFixture "ControlFlow" "tag-comparison"

[<Fact>]
let ``Fixture payload tag comparison if-expression`` () =
    assertFixture "ControlFlow" "payload-tag-comparison"

[<Fact>]
let ``Fixture tag match function`` () =
    assertFixture "ControlFlow" "tag-match"

[<Fact>]
let ``Fixture guard match`` () =
    assertFixture "ControlFlow" "guard-match"
