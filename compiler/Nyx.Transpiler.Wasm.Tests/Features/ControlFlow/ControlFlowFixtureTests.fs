module TranspilerWasmTests.Features.ControlFlow.ControlFlowFixtureTests

open Xunit
open TranspilerWasmTestHelpers

[<Fact>]
let ``Fixture if-expression function`` () =
    assertFixture "ControlFlow" "if-expression"

[<Fact>]
let ``Fixture nested block locals`` () =
    assertFixture "ControlFlow" "nested-block-locals"
