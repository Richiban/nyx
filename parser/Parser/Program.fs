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
    | FunctionCall of Identifier * Expression list
    | Lambda of Identifier list * Expression
    | BinaryOp of string * Expression * Expression
    | Block of Statement list

and Statement =
    | DefStatement of Identifier * Expression
    | ExprStatement of Expression

type Definition =
    | ValueDef of Identifier * Expression

type TopLevelItem =
    | ModuleDecl of ModuleName
    | Def of Definition

type Module = TopLevelItem list

// Helper: whitespace and comment handling
let ws = spaces
let wsNoNl() = skipMany (skipAnyOf " \t") // whitespace without newlines
let comment () = pstring "--" >>. skipRestOfLine true
let wsWithComments () = skipMany (skipMany1 spaces1 <|> comment ())

// Indentation helpers
let getIndentation () = 
    getPosition |>> (fun pos -> int pos.Column - 1)

let skipToColumn (col: int64) =
    getPosition >>= fun pos ->
        let currentCol = pos.Column
        if currentCol < col then
            skipManyTill (pchar ' ') (getPosition >>= fun p -> if p.Column >= col then preturn () else pzero)
        else if currentCol = col then
            preturn ()
        else
            pzero

let atIndentation (refCol: int64) =
    getPosition >>= fun pos ->
        if pos.Column = refCol then preturn ()
        else pzero

let greaterIndentation (refCol: int64) =
    getPosition >>= fun pos ->
        if pos.Column > refCol then preturn ()
        else pzero

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

// Forward references for recursive expression parsing
let expression, expressionRef = createParserForwardedToRef<Expression, unit>()
let lambda, lambdaRef = createParserForwardedToRef<Expression, unit>()
let functionCall, functionCallRef = createParserForwardedToRef<Expression, unit>()

// Operator precedence parser
let opp = new OperatorPrecedenceParser<Expression, unit, unit>()

// Primary expression (literals, identifiers, function calls, lambdas, parenthesized expressions)
let primaryExpr =
    choice [
        attempt lambda
        attempt functionCall
        literal |>> LiteralExpr
        between (pstring "(") (pstring ")") (ws >>. expression .>> ws)
        identifier |>> IdentifierExpr
    ] .>> ws

opp.TermParser <- primaryExpr

// Define operators with precedence
// Multiplicative operators (higher precedence)
opp.AddOperator(InfixOperator("*", ws, 7, Associativity.Left, fun x y -> BinaryOp("*", x, y)))
opp.AddOperator(InfixOperator("/", ws, 7, Associativity.Left, fun x y -> BinaryOp("/", x, y)))

// Additive operators
opp.AddOperator(InfixOperator("+", ws, 6, Associativity.Left, fun x y -> BinaryOp("+", x, y)))
opp.AddOperator(InfixOperator("-", ws, 6, Associativity.Left, fun x y -> BinaryOp("-", x, y)))

// Comparison operators (lower precedence)
opp.AddOperator(InfixOperator("<", ws, 4, Associativity.Left, fun x y -> BinaryOp("<", x, y)))
opp.AddOperator(InfixOperator(">", ws, 4, Associativity.Left, fun x y -> BinaryOp(">", x, y)))
opp.AddOperator(InfixOperator("<=", ws, 4, Associativity.Left, fun x y -> BinaryOp("<=", x, y)))
opp.AddOperator(InfixOperator(">=", ws, 4, Associativity.Left, fun x y -> BinaryOp(">=", x, y)))

// Equality operators (lowest precedence)
opp.AddOperator(InfixOperator("==", ws, 3, Associativity.Left, fun x y -> BinaryOp("==", x, y)))
opp.AddOperator(InfixOperator("!=", ws, 3, Associativity.Left, fun x y -> BinaryOp("!=", x, y)))

// Wire up the expression parser
do expressionRef := opp.ExpressionParser

// Wire up the lambda parser
do 
    let paramList = sepBy identifier (pstring "," .>> ws)
    let arrow = pstring "->" .>> ws
    let lambdaWithParams = pipe2 (paramList .>> arrow) expression (fun parameters body -> Lambda(parameters, body))
    let lambdaNoParams = expression |>> (fun body -> Lambda([], body))
    
    // Shorthand lambda: { * } means { x, y -> x * y }
    let shorthandBinaryOp =
        let operators = ["*"; "/"; "+"; "-"; "<"; ">"; "<="; ">="; "=="; "!="]
        choice (operators |> List.map (fun op ->
            pstring op >>. ws >>. notFollowedBy (skipAnyOf "0123456789.\"'([{") >>% Lambda(["x"; "y"], BinaryOp(op, IdentifierExpr "x", IdentifierExpr "y"))
        ))
    
    // Shorthand lambda: { * 2 } means { x -> x * 2 }
    let shorthandUnaryOp =
        let operators = [("*", "*"); ("/", "/"); ("+", "+"); ("-", "-")]
        choice (operators |> List.map (fun (op, opStr) ->
            pstring op >>. ws >>. primaryExpr |>> (fun rightExpr ->
                Lambda(["x"], BinaryOp(opStr, IdentifierExpr "x", rightExpr))
            )
        ))
    
    // Shorthand lambda: { .name } means { x -> x.name } (property access - for now just parse as identifier)
    // Note: We'll need proper member access syntax later, for now treat .name as accessing a field
    let shorthandPropertyAccess =
        pstring "." >>. identifier |>> (fun propName ->
            // For now, we'll represent this as a function call to a getter
            // Later we can add proper member access to the AST
            Lambda(["x"], FunctionCall(propName, [IdentifierExpr "x"]))
        )
    
    lambdaRef :=
        between (pstring "{") (pstring "}") (
            ws >>. choice [
                attempt shorthandUnaryOp
                attempt shorthandBinaryOp
                attempt shorthandPropertyAccess
                attempt lambdaWithParams
                lambdaNoParams
            ] .>> ws
        )
        <?> "lambda expression"

// Wire up the function call parser
do 
    functionCallRef :=
        pipe2
            identifier
            (between (pstring "(") (pstring ")") 
                (sepBy expression (pstring "," .>> ws)))
            (fun name args -> FunctionCall(name, args))
        <?> "function call"

// Forward reference for statement parsing
let statement, statementRef = createParserForwardedToRef<Statement, unit>()

// Block parser - parses indented statements after a newline
let blockExpr() : Parser<Expression, unit> =
    // After '=', if there's a newline followed by indentation, parse a block
    getPosition >>= fun startPos ->
        newline >>.
        many (pchar ' ' <|> pchar '\t') >>= fun indentChars ->
            let indentLevel = indentChars.Length
            if indentLevel > 0 then
                // We have indentation, parse statements at this level
                let rec parseIndentedStatements acc =
                    (attempt (
                        // Check if we're at the right indentation
                        many (pchar ' ' <|> pchar '\t') >>= fun currentIndent ->
                            if currentIndent.Length = indentLevel then
                                statement >>= fun stmt ->
                                    (attempt newline >>. parseIndentedStatements (stmt :: acc))
                                    <|> preturn (stmt :: acc)
                            else if currentIndent.Length < indentLevel then
                                // Less indented, end the block
                                preturn acc
                            else
                                // More indented than expected, error
                                pzero
                    )) <|> preturn acc
                
                parseIndentedStatements [] |>> (List.rev >> Block)
            else
                pzero

// Wire up statement parser
do
    let defStatement = 
        pipe2
            (pstring "def" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws)
            (attempt (blockExpr()) <|> expression)
            (fun name expr -> DefStatement(name, expr))
    
    let exprStatement = expression |>> ExprStatement
    
    statementRef := (attempt defStatement <|> exprStatement) <?> "statement"

// Parser for module declaration
let moduleDecl: Parser<TopLevelItem, unit> =
    pstring "module" >>. ws >>. identifier |>> ModuleDecl
    <?> "module declaration"

// Parser for value definition (top-level)
let valueDef: Parser<TopLevelItem, unit> =
    pipe2
        (pstring "def" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws)
        (attempt (blockExpr()) <|> expression)
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
