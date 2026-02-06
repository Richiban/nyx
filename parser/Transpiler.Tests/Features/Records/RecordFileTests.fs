module TranspilerTests.Features.Records.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile records fixture`` () =
    assertFixture "Records" "test_records"
