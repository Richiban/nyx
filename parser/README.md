# Nyx Parser

A parser for the Nyx programming language built with FParsec.

## Features

Currently supports parsing:

- **Module declarations**: `module ModuleName`
- **Value definitions**: `def identifier = expression`
- **Literals**:
  - Strings: `"Hello world"`
  - Integers: `42`
  - Floats: `3.14159`
  - Booleans: `true`, `false`
- **Identifiers**: Variable references like `someValue`
- **Whitespace handling**: Flexible whitespace and newlines

## Project Structure

```
parser/
├── Parser/               # Main parser library (F#)
│   ├── Program.fs       # Parser implementation and AST
│   └── NyxParser.fsproj
├── Parser.Tests/        # Unit tests (F#)
│   ├── ParserTests.fs   # 23 comprehensive tests
│   └── Parser.Tests.fsproj
└── sample.nyx           # Sample Nyx file
```

## Building

```powershell
cd parser
dotnet build
```

## Running Tests

```powershell
cd parser
dotnet test
```

All 23 tests should pass:
- Literal parsing (strings, ints, floats, booleans)
- Module declarations
- Value definitions
- Identifier handling (underscores, digits)
- Whitespace handling (leading, trailing, extra)
- Error cases (missing equals, missing value, unterminated strings)
- Complex scenarios (multiple definitions, blank lines)

## Running the Parser

```powershell
dotnet run --project Parser sample.nyx
```

## AST Structure

```fsharp
type Literal =
    | StringLit of string
    | IntLit of int
    | FloatLit of float
    | BoolLit of bool

type Expression =
    | LiteralExpr of Literal
    | IdentifierExpr of Identifier

type Definition =
    | ValueDef of Identifier * Expression

type TopLevelItem =
    | ModuleDecl of ModuleName
    | Def of Definition

type Module = TopLevelItem list
```

## Next Steps

To extend the parser, consider adding:
- Function definitions with parameters
- Lambda expressions
- Function calls
- Operators and binary expressions
- Pattern matching
- Type annotations
- Comments (single-line `--` and multi-line `---`)
- Indentation-based blocks
- Collections (lists, records, tuples)

## Example

Input:
```nyx
module Test

def message = "Hello world"
def count = 42
def pi = 3.14159
def flag = true
```

Parsed AST:
```fsharp
[ModuleDecl "Test";
 Def (ValueDef ("message", LiteralExpr (StringLit "Hello world")));
 Def (ValueDef ("count", LiteralExpr (IntLit 42)));
 Def (ValueDef ("pi", LiteralExpr (FloatLit 3.14159)));
 Def (ValueDef ("flag", LiteralExpr (BoolLit true)))]
```
