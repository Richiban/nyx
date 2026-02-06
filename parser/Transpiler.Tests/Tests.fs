module TranspilerTests

open System.IO
open Xunit
open FsUnit.Xunit
open Parser.Program
open Transpiler.CodeGen

// Helper to load test file
let loadTestFile filename =
    let testDataDir = Path.Combine(__SOURCE_DIRECTORY__, "testdata")
    let filePath = Path.Combine(testDataDir, filename)
    File.ReadAllText(filePath)

// Helper to parse and transpile
let transpileFile filename =
    let source = loadTestFile filename
    match parseModule source with
    | Result.Ok ast -> transpileModule ast
    | Result.Error err -> failwith $"Parse error: {err}"

// Literal Tests
[<Fact>]
let ``Transpile integer literal`` () =
    let expr = LiteralExpr (IntLit 42)
    let result = transpileExpression expr
    result |> should equal "42"

[<Fact>]
let ``Transpile string literal`` () =
    let expr = LiteralExpr (StringLit "hello")
    let result = transpileExpression expr
    result |> should equal "\"hello\""

[<Fact>]
let ``Transpile boolean literal`` () =
    let expr = LiteralExpr (BoolLit true)
    let result = transpileExpression expr
    result |> should equal "true"

[<Fact>]
let ``Transpile float literal`` () =
    let expr = LiteralExpr (FloatLit 3.14)
    let result = transpileExpression expr
    result |> should equal "3.14"

// Expression Tests
[<Fact>]
let ``Transpile identifier`` () =
    let expr = IdentifierExpr "x"
    let result = transpileExpression expr
    result |> should equal "x"

[<Fact>]
let ``Transpile member access`` () =
    let expr = MemberAccess(IdentifierExpr "point", "x")
    let result = transpileExpression expr
    result |> should equal "point.x"

[<Fact>]
let ``Transpile nested member access`` () =
    let expr = MemberAccess(MemberAccess(IdentifierExpr "outer", "inner"), "value")
    let result = transpileExpression expr
    result |> should equal "outer.inner.value"

[<Fact>]
let ``Transpile binary operation`` () =
    let expr = BinaryOp("+", LiteralExpr (IntLit 1), LiteralExpr (IntLit 2))
    let result = transpileExpression expr
    result |> should equal "(1 + 2)"

[<Fact>]
let ``Transpile equality to triple equals`` () =
    let expr = BinaryOp("==", IdentifierExpr "x", LiteralExpr (IntLit 5))
    let result = transpileExpression expr
    result |> should equal "(x === 5)"

[<Fact>]
let ``Transpile not equals to not triple equals`` () =
    let expr = BinaryOp("!=", IdentifierExpr "x", LiteralExpr (IntLit 5))
    let result = transpileExpression expr
    result |> should equal "(x !== 5)"

// Collection Tests
[<Fact>]
let ``Transpile tuple to array`` () =
    let expr = TupleExpr [LiteralExpr (IntLit 1); LiteralExpr (IntLit 2); LiteralExpr (IntLit 3)]
    let result = transpileExpression expr
    result |> should equal "[1, 2, 3]"

[<Fact>]
let ``Transpile list to array`` () =
    let expr = ListExpr [LiteralExpr (IntLit 1); LiteralExpr (IntLit 2)]
    let result = transpileExpression expr
    result |> should equal "[1, 2]"

[<Fact>]
let ``Transpile record to object`` () =
    let expr = RecordExpr [
        NamedField("x", LiteralExpr (IntLit 10))
        NamedField("y", LiteralExpr (IntLit 20))
    ]
    let result = transpileExpression expr
    result |> should equal "{ x: 10, y: 20 }"

// Function Tests
[<Fact>]
let ``Transpile function call with no args`` () =
    let expr = FunctionCall("getCurrentTime", [])
    let result = transpileExpression expr
    result |> should equal "getCurrentTime()"

[<Fact>]
let ``Transpile function call with tuple args`` () =
    let expr = FunctionCall("add", [TupleExpr [LiteralExpr (IntLit 5); LiteralExpr (IntLit 10)]])
    let result = transpileExpression expr
    result |> should equal "add(5, 10)"

[<Fact>]
let ``Transpile lambda`` () =
    let expr = Lambda(["x"; "y"], BinaryOp("+", IdentifierExpr "x", IdentifierExpr "y"))
    let result = transpileExpression expr
    result |> should equal "(x, y) => (x + y)"

// Pipe Tests
[<Fact>]
let ``Transpile simple pipe`` () =
    let expr = Pipe(IdentifierExpr "value", "double", [])
    let result = transpileExpression expr
    result |> should equal "double(value)"

[<Fact>]
let ``Transpile pipe with args`` () =
    let expr = Pipe(
        LiteralExpr (IntLit 5),
        "add",
        [TupleExpr [LiteralExpr (IntLit 10)]]
    )
    let result = transpileExpression expr
    result |> should equal "add(5, 10)"

// Control Flow Tests
[<Fact>]
let ``Transpile if expression`` () =
    let expr = IfExpr(
        LiteralExpr (BoolLit true),
        LiteralExpr (IntLit 1),
        LiteralExpr (IntLit 2)
    )
    let result = transpileExpression expr
    result |> should equal "(true ? 1 : 2)"

[<Fact>]
let ``Transpile tag without payload`` () =
    let expr = TagExpr("None", None)
    let result = transpileExpression expr
    result |> should equal "{ tag: \"None\" }"

[<Fact>]
let ``Transpile tag with payload`` () =
    let expr = TagExpr("Some", Some (LiteralExpr (IntLit 42)))
    let result = transpileExpression expr
    result |> should equal "{ tag: \"Some\", value: 42 }"

// File Tests
[<Fact>]
let ``Transpile test_transpile.nyx file`` () =
    let result = transpileFile "test_transpile.nyx"
    Assert.Contains("const x = 42;", result)
    Assert.Contains("const y = \"hello\";", result)
    Assert.Contains("const sum = (1 + 2);", result)
    Assert.Contains("const point = { x: 10, y: 20 };", result)
    Assert.Contains("const accessField = point.x;", result)
    Assert.Contains("const tuple = [1, 2, 3];", result)

[<Fact>]
let ``Transpile test_pipes.nyx file`` () =
    let result = transpileFile "test_pipes.nyx"
    Assert.Contains("const add = (a, b) => (a + b);", result)
    Assert.Contains("const double = (x) => (x * 2);", result)
    Assert.Contains("const result = add(5, 10);", result)
    Assert.Contains("const chained = double(double(3));", result)

