module TranspilerTests.Features.Blocks.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile blocks fixture`` () =
    assertFixture "Blocks" "test_blocks"
