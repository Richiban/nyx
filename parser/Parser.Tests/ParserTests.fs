module ParserTests

open Xunit
open FsUnit.Xunit
open Parser.Program

// Helper to check if Result is Ok
let isOk result =
    match result with
    | Result.Ok _ -> true
    | Result.Error _ -> false

// Helper to check if Result is Error
let isError result =
    match result with
    | Result.Ok _ -> false
    | Result.Error _ -> true

// Literal Tests
[<Fact>]
let ``Parse string literal`` () =
    let result = parseModule "def message = \"Hello world\""
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, LiteralExpr (StringLit value))) ->
            name |> should equal "message"
            value |> should equal "Hello world"
        | _ -> failwith "Expected Def with StringLit"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse integer literal`` () =
    let result = parseModule "def count = 42"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, LiteralExpr (IntLit value))) ->
            name |> should equal "count"
            value |> should equal 42
        | _ -> failwith "Expected Def with IntLit"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse float literal`` () =
    let result = parseModule "def pi = 3.14159"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, LiteralExpr (FloatLit value))) ->
            name |> should equal "pi"
            value |> should equal 3.14159
        | _ -> failwith "Expected Def with FloatLit"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse boolean true`` () =
    let result = parseModule "def flag = true"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, LiteralExpr (BoolLit value))) ->
            name |> should equal "flag"
            value |> should equal true
        | _ -> failwith "Expected Def with BoolLit"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse boolean false`` () =
    let result = parseModule "def flag = false"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, LiteralExpr (BoolLit value))) ->
            name |> should equal "flag"
            value |> should equal false
        | _ -> failwith "Expected Def with BoolLit"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

// Module Tests
[<Fact>]
let ``Parse module declaration`` () =
    let result = parseModule "module Test"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | ModuleDecl name ->
            name |> should equal "Test"
        | _ -> failwith "Expected ModuleDecl"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse module with definition`` () =
    let input = "module Test\ndef message = \"Hello\""
    let result = parseModule input
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 2
        match module'.[0] with
        | ModuleDecl _ -> ()
        | _ -> failwith "Expected ModuleDecl"
        match module'.[1] with
        | Def _ -> ()
        | _ -> failwith "Expected Def"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse multiple definitions`` () =
    let input = "def x = 1\ndef y = 2\ndef z = 3"
    let result = parseModule input
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 3
        for item in module' do
            match item with
            | Def _ -> ()
            | _ -> failwith "Expected all Def"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

// Identifier Tests
[<Fact>]
let ``Parse identifier expression`` () =
    let result = parseModule "def x = someValue"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, IdentifierExpr value)) ->
            name |> should equal "x"
            value |> should equal "someValue"
        | _ -> failwith "Expected Def with IdentifierExpr"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Identifier can start with underscore`` () =
    let result = parseModule "def _private = 42"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, _)) ->
            name |> should equal "_private"
        | _ -> failwith "Expected Def"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Identifier can contain digits`` () =
    let result = parseModule "def value123 = 999"
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 1
        match module'.[0] with
        | Def (ValueDef (name, _)) ->
            name |> should equal "value123"
        | _ -> failwith "Expected Def"
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

// Whitespace Tests
[<Fact>]
let ``Parse with leading whitespace`` () =
    let result = parseModule "   def x = 1"
    result |> isOk |> should equal true

[<Fact>]
let ``Parse with trailing whitespace`` () =
    let result = parseModule "def x = 1   "
    result |> isOk |> should equal true

[<Fact>]
let ``Parse with extra whitespace around equals`` () =
    let result = parseModule "def x    =    1"
    result |> isOk |> should equal true

[<Fact>]
let ``Parse empty input`` () =
    let result = parseModule ""
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should be Empty
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse only whitespace`` () =
    let result = parseModule "   \n  \n  "
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should be Empty
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

// Error Tests
[<Fact>]
let ``Error on missing equals`` () =
    let result = parseModule "def x 42"
    result |> isError |> should equal true

[<Fact>]
let ``Error on missing value`` () =
    let result = parseModule "def x ="
    result |> isError |> should equal true

[<Fact>]
let ``Error on missing identifier`` () =
    let result = parseModule "def = 42"
    result |> isError |> should equal true

[<Fact>]
let ``Error on unterminated string`` () =
    let result = parseModule "def x = \"unterminated"
    result |> isError |> should equal true

// Complex Tests
[<Fact>]
let ``Parse multiline module`` () =
    let input = """module MyModule

def x = 1
def y = 2
def message = "Hello"
def flag = true"""
    
    let result = parseModule input
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 5  // 1 module decl + 4 defs
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse with blank lines`` () =
    let input = """module Test

def x = 1

def y = 2"""
    
    let result = parseModule input
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 3
    | Result.Error err -> failwith $"Parse should succeed, got: {err}"

[<Fact>]
let ``Parse sample nyx file`` () =
    let input = """module Test

def message = "Hello world"
def x = 42
def pi = 3.14"""
    
    let result = parseModule input
    
    result |> isOk |> should equal true
    match result with
    | Result.Ok module' ->
        module' |> should haveLength 4
        match module'.[0] with
        | ModuleDecl name -> name |> should equal "Test"
        | _ -> failwith "Expected ModuleDecl"
    | Result.Error err ->
        failwith $"Parse failed: {err}"
