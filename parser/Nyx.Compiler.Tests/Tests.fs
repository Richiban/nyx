module Nyx.Compiler.Tests

open Xunit
open NyxCompiler

[<Fact>]
let ``Compile returns typed module for valid source`` () =
    let source = "def message = \"Hello\""
    let result = Compiler.compile source
    Assert.Equal(CompilationPhase.Typed, result.Phase)
    Assert.True(result.Diagnostics.IsEmpty)
    Assert.True(result.Typed.IsSome)

[<Fact>]
let ``Compile reports diagnostics on parse error`` () =
    let source = "def message = "
    let result = Compiler.compile source
    Assert.Equal(CompilationPhase.Parsed, result.Phase)
    Assert.False(result.Diagnostics.IsEmpty)
