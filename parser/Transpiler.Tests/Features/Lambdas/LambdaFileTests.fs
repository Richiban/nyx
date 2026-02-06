module TranspilerTests.Features.Lambdas.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile lambdas fixture`` () =
    assertFixture "Lambdas" "test_lambdas"
