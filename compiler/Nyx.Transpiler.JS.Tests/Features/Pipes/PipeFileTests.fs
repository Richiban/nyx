module TranspilerTests.Features.Pipes.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile pipes fixture`` () =
    assertFixture "Pipes" "test_pipes"
