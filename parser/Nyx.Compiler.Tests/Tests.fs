module Nyx.Compiler.Tests

open Xunit
open NyxCompiler
open Parser.Program

[<Fact>]
let ``Compile returns typed module for valid source`` () =
    let source = "def message = \"Hello\""
    let result = Compiler.compile source
    Assert.Equal(CompilationPhase.Typed, result.Phase)
    Assert.True(result.Diagnostics.IsEmpty)
    Assert.True(result.Typed.IsSome)

[<Fact>]
let ``Typecheck infers literal types`` () =
    let source = "def message = \"Hello\""
    let result = Compiler.compile source
    let typed = result.Typed.Value
    Assert.Equal(TyPrimitive "string", typed.Types.["message"])

[<Fact>]
let ``Typecheck supports polymorphic let`` () =
    let source =
        "def id = { x -> x }\n" +
        "def a = id(1)\n" +
        "def b = id(\"hi\")"
    let result = Compiler.compile source
    let typed = result.Typed.Value
    Assert.Equal(TyPrimitive "int", typed.Types.["a"])
    Assert.Equal(TyPrimitive "string", typed.Types.["b"])

[<Fact>]
let ``Typecheck match arms agree`` () =
    let source =
        "def result = match 1\n" +
        "  | 1 -> \"one\"\n" +
        "  | _ -> \"other\""
    let result = Compiler.compile source
    let typed = result.Typed.Value
    Assert.Equal(TyPrimitive "string", typed.Types.["result"])

[<Fact>]
let ``Typecheck populates typed AST items`` () =
    let source = "def message = \"Hello\""
    let result = Compiler.compile source
    let typed = result.Typed.Value
    let hasMessage =
        typed.Items
        |> List.exists (function
            | TypedDef (TypedValueDef(name, _, typedExpr)) ->
                name = "message" && typedExpr.Type = TyPrimitive "string"
            | _ -> false)
    Assert.True(hasMessage)

[<Fact>]
let ``Typecheck captures typed block statements in lambda`` () =
    let source =
        "def f = { x ->\n" +
        "  def y = x\n" +
        "  y\n" +
        "}"
    let result = Compiler.compile source
    let typed = result.Typed.Value
    let hasBlockStatements =
        typed.Items
        |> List.exists (function
            | TypedDef (TypedValueDef(name, _, typedExpr)) when name = "f" ->
                match typedExpr.Body with
                | Some bodyExpr ->
                    match bodyExpr.Statements with
                    | Some statements ->
                        statements
                        |> List.exists (function
                            | TypedDefStatement(defName, _, _) -> defName = "y"
                            | _ -> false)
                    | None -> false
                | None -> false
            | _ -> false)
    Assert.True(hasBlockStatements)

[<Fact>]
let ``Typecheck records typed match patterns`` () =
    let source =
        "def result = match 1\n" +
        "  | 1 -> \"one\"\n" +
        "  | _ -> \"other\""
    let result = Compiler.compile source
    let typed = result.Typed.Value
    let matchHasPattern =
        typed.Items
        |> List.exists (function
            | TypedDef (TypedValueDef(name, _, typedExpr)) when name = "result" ->
                match typedExpr.MatchArms with
                | Some arms ->
                    arms
                    |> List.exists (fun (patterns, _) ->
                        patterns
                        |> List.exists (fun pattern ->
                            match pattern.Pattern with
                            | LiteralPattern (IntLit 1) -> pattern.Type = TyPrimitive "int"
                            | _ -> false))
                | None -> false
            | _ -> false)
    Assert.True(matchHasPattern)

[<Fact>]
let ``Compile reports diagnostics on parse error`` () =
    let source = "def message = "
    let result = Compiler.compile source
    Assert.Equal(CompilationPhase.Parsed, result.Phase)
    Assert.False(result.Diagnostics.IsEmpty)

[<Fact>]
let ``Unifier binds type variables`` () =
    let tvar = TyVar { Id = 1; Name = None }
    let constraints: ConstraintSet = [ (tvar, TyPrimitive "string") ]
    let result = Unifier.unify constraints
    match result with
    | Ok subst -> Assert.Equal(Some (TyPrimitive "string"), subst |> Map.tryFind 1)
    | Error message -> Assert.True(false, message)

[<Fact>]
let ``Unifier rejects occurs check`` () =
    let tvar = TyVar { Id = 2; Name = None }
    let constraintSet: ConstraintSet = [ (tvar, TyFunc([ tvar ], TyPrimitive "int")) ]
    let result = Unifier.unify constraintSet
    match result with
    | Ok _ -> Assert.True(false, "Expected occurs check failure")
    | Error message -> Assert.Contains("Occurs", message)
