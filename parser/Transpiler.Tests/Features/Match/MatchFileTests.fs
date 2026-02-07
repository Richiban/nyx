module TranspilerTests.Features.MatchFileTests

open Xunit
open TranspilerTestHelpers

[<Fact>]
let ``Transpile match basic fixture`` () =
    assertFixture "Match" "test_match_basic"

[<Fact>]
let ``Transpile match patterns fixture`` () =
    assertFixture "Match" "test_match_patterns"
