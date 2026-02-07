module TranspilerTests.Features.Transpile.Files

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile basic fixture`` () =
    assertFixture "Transpile" "test_transpile"

[<Fact>]
let ``Transpile qualified call fixture`` () =
    assertFixture "Transpile" "test_qualified_call"

[<Fact>]
let ``Transpile imports fixture`` () =
    assertFixture "Transpile" "test_imports"
