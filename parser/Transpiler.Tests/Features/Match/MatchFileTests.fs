module TranspilerTests.Features.MatchFileTests

open Xunit
open TranspilerTestHelpers

[<Fact(Skip = "Match syntax is not yet supported by the parser")>]
let ``Transpile match basic fixture`` () =
    assertFixture "Match" "test_match_basic"

[<Fact(Skip = "Match syntax is not yet supported by the parser")>]
let ``Transpile match patterns fixture`` () =
    assertFixture "Match" "test_match_patterns"
