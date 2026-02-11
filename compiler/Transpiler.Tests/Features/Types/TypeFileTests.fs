module TranspilerTests.Features.Types.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile types fixture`` () =
    assertFixture "Types" "test_type_defs"
