module TranspilerTests.Features.Operators.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile operators fixture`` () =
    assertFixture "Operators" "test_operators"
