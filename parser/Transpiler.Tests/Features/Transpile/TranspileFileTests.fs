module TranspilerTests.Features.Transpile.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile basic fixture`` () =
    assertFixture "Transpile" "test_transpile"
