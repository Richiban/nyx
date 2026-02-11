module TranspilerTests.Features.Lists.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile lists fixture`` () =
    assertFixture "Lists" "test_lists"
