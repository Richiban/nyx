module TranspilerTests.Features.Chaining.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile chaining fixture`` () =
    assertFixture "Chaining" "test_chaining"
