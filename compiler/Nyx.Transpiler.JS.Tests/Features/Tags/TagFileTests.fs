module TranspilerTests.Features.Tags.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile tags fixture`` () =
    assertFixture "Tags" "test_tags"
