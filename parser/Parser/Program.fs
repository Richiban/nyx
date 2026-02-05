module Parser.Program

open System
open FParsec

// AST Types - Simplified for basic parsing
type Identifier = string
type ModuleName = string

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

// Helper: whitespace and comment handling
let ws = spaces
let comment () = pstring "--" >>. skipRestOfLine true
let wsWithComments () = skipMany (skipMany1 spaces1 <|> comment ())

// Parser for identifiers
let identifier: Parser<Identifier, unit> =
    let isIdentStart c = isLetter c || c = '_'
    let isIdentContinue c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isIdentStart isIdentContinue .>> ws
    <?> "identifier"

// Parser for literals
let stringLiteral: Parser<Literal, unit> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))
    |>> StringLit
    .>> ws
    <?> "string literal"

let intLiteral: Parser<Literal, unit> =
    pint32 .>> notFollowedBy (pchar '.') .>> ws |>> IntLit
    <?> "integer literal"

let floatLiteral: Parser<Literal, unit> =
    pfloat .>> ws |>> FloatLit
    <?> "float literal"

let boolLiteral: Parser<Literal, unit> =
    (stringReturn "true" (BoolLit true) <|> stringReturn "false" (BoolLit false))
    .>> ws
    <?> "boolean literal"

let literal: Parser<Literal, unit> =
    choice [
        attempt intLiteral
        attempt floatLiteral
        boolLiteral
        stringLiteral
    ]
    <?> "literal"

// Parser for expressions
let expression: Parser<Expression, unit> =
    choice [
        literal |>> LiteralExpr
        identifier |>> IdentifierExpr
    ]
    <?> "expression"

// Parser for module declaration
let moduleDecl: Parser<TopLevelItem, unit> =
    pstring "module" >>. ws >>. identifier |>> ModuleDecl
    <?> "module declaration"

// Parser for value definition
let valueDef: Parser<TopLevelItem, unit> =
    pipe2
        (pstring "def" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws)
        expression
        (fun name expr -> Def(ValueDef(name, expr)))
    <?> "value definition"

// Parser for top-level items
let topLevelItem: Parser<TopLevelItem, unit> =
    ws >>. choice [
        moduleDecl
        valueDef
    ] .>> ws
    <?> "top-level item"

// Parser for a complete module
let moduleParser: Parser<Module, unit> =
    ws >>. many topLevelItem .>> ws .>> eof
    <?> "module"

// Public API for parsing
let parseModule (input: string) : Result<Module, string> =
    match run moduleParser input with
    | ParserResult.Success(result, _, _) -> Result.Ok result
    | ParserResult.Failure(errorMsg, _, _) -> Result.Error errorMsg

// Main entry point for standalone testing
[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        let file = argv.[0]
        let text = System.IO.File.ReadAllText(file)
        match parseModule text with
        | Result.Ok ast ->
            printfn "Parsed successfully:"
            ast |> List.iter (printfn "  %A")
            0
        | Result.Error err ->
            printfn "Parse error: %s" err
            1
    else
        printfn "Usage: NyxParser <file.nyx>"
        1
