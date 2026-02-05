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
    | Pipe of Expression * Identifier * Expression list  // expr \func or expr \func(args)
    | Block of Statement list
    | Match of Expression list * MatchArm list  // Can match on multiple values
    | TupleExpr of Expression list  // Positional-only tuple (for backward compat with existing tests)
    | RecordExpr of RecordField list  // Mixed tuple/record with named and/or positional fields
    | ListExpr of Expression list
    | TagExpr of Identifier * Expression option  // #tagName or #tagName(expr)
    | IfExpr of Expression * Expression * Expression  // if condition then trueExpr else falseExpr

// Record field: either named (x = expr) or positional (expr)
and RecordField =
    | NamedField of Identifier * Expression
    | PositionalField of Expression

and Pattern =
    | LiteralPattern of Literal
    | IdentifierPattern of Identifier
    | WildcardPattern
    | ElsePattern
    | TuplePattern of Pattern list
    | RecordPattern of Identifier * Pattern list  // Type name and patterns by position
    | RecordMemberPattern of (Identifier * Pattern) list  // Patterns by member name
    | ListPattern of Pattern list * Pattern option  // patterns, optional splat at end
    | ListSplatMiddle of Pattern list * Pattern list  // patterns before ..., patterns after
    | RangePattern of Expression * Expression  // start..end (inclusive)
    | GuardPattern of string * Expression  // operator (>, <, >=, <=, ==, !=) and value
    | TagPattern of Identifier * Pattern option  // #tagName or #tagName(pattern)

and MatchArm = Pattern list * Expression  // Multiple patterns for multi-value match

and Statement =
    | DefStatement of Identifier * Expression
    | ExprStatement of Expression

// Helper type for parsing list patterns
type ListPatternElement =
    | RegularPattern of Pattern
    | Splat of Identifier option

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
let identifierNoWs: Parser<Identifier, unit> =
    let isIdentStart c = isLetter c || c = '_'
    let isIdentContinue c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isIdentStart isIdentContinue
    <?> "identifier"

let identifier: Parser<Identifier, unit> = identifierNoWs .>> ws

// Parser for literals
let stringLiteralNoWs: Parser<Literal, unit> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))
    |>> StringLit
    <?> "string literal"

let stringLiteral: Parser<Literal, unit> = stringLiteralNoWs  // Don't consume ws

let intLiteralNoWs: Parser<Literal, unit> =
    // Allow parsing int even if followed by dots (for range operator "..")
    // Just don't allow a SINGLE dot (which would make it a float)
    let notFloat = followedBy (pstring "..") <|> notFollowedBy (pchar '.')
    pint32 .>> notFloat |>> IntLit
    <?> "integer literal"

let intLiteral: Parser<Literal, unit> = intLiteralNoWs  // Don't consume ws - let caller decide

let floatLiteralNoWs: Parser<Literal, unit> =
    pfloat |>> FloatLit
    <?> "float literal"

let floatLiteral: Parser<Literal, unit> = floatLiteralNoWs  // Don't consume ws - let caller decide

let boolLiteralNoWs: Parser<Literal, unit> =
    (stringReturn "true" (BoolLit true) <|> stringReturn "false" (BoolLit false))
    <?> "boolean literal"

let boolLiteral: Parser<Literal, unit> = boolLiteralNoWs  // Don't consume ws - let caller decide

let literalNoWs: Parser<Literal, unit> =
    choice [
        attempt intLiteralNoWs
        attempt floatLiteralNoWs
        boolLiteralNoWs
        stringLiteralNoWs
    ]
    <?> "literal"

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
let matchExpr, matchExprRef = createParserForwardedToRef<Expression, unit>()

// Pattern parsers
let pattern, patternRef = createParserForwardedToRef<Pattern, unit>()

let wildcardPattern = pstring "_" >>% WildcardPattern
let elsePattern = pstring "else" >>% ElsePattern
let identifierPattern = identifierNoWs |>> IdentifierPattern

// Parse either a range pattern (1..10), an integer literal pattern (1), 
// or other literal types (float, bool, string)
// NOTE: For ranges, we only allow INTEGER literals to avoid ambiguity with floats like "1."
let literalOrRangePattern =
    choice [
        // Try integer with optional range
        attempt (
            intLiteralNoWs >>= fun startLit ->
            opt (pstring ".." >>. ws >>. intLiteralNoWs) >>= fun endLitOpt ->
            match endLitOpt with
            | Some endLit -> preturn (RangePattern(LiteralExpr startLit, LiteralExpr endLit))
            | None -> preturn (LiteralPattern startLit)
        )
        // Or any other literal type
        literalNoWs |>> LiteralPattern
    ]

// Tuple pattern: (pat1, pat2, ...)
let tuplePattern =
    between
        (pstring "(")
        (pstring ")")
        (sepBy1 (ws >>. pattern) (pstring "," .>> ws))
    |>> fun patterns ->
        match patterns with
        | [single] -> single  // Single pattern in parens is just that pattern, not a tuple
        | multiple -> TuplePattern multiple

// Record pattern by position: TypeName(pat1, pat2, ...)
let recordPatternByPosition =
    pipe2
        identifierNoWs
        (between
            (pstring "(")
            (pstring ")")
            (sepBy1 (ws >>. pattern) (pstring "," .>> ws)))
        (fun typeName patterns -> RecordPattern(typeName, patterns))

// Record pattern by member: (member1 = pat1, member2 = pat2, ...)
let recordMemberPattern =
    between
        (pstring "(")
        (pstring ")")
        (sepBy1
            (pipe2
                (ws >>. identifierNoWs .>> ws)
                (pstring "=" >>. ws >>. pattern)
                (fun name pat -> (name, pat)))
            (pstring "," .>> ws))
    |>> RecordMemberPattern

// List pattern: [pat1, pat2, ...] or [pat1, ...rest] or [first, ..., last]
let listPattern =
    let splatPattern = 
        pstring "..." >>. ws >>. opt identifierNoWs
        |>> Splat
    
    let regularPatternOrSplat =
        (attempt splatPattern) <|> (pattern |>> RegularPattern)
    
    between
        (pstring "[")
        (pstring "]")
        (sepBy (ws >>. regularPatternOrSplat) (pstring "," .>> ws))
    |>> fun elements ->
        // Find where the splat is (if any)
        let splatIndex = elements |> List.tryFindIndex (fun e -> 
            match e with Splat _ -> true | _ -> false)
        
        match splatIndex with
        | None ->
            // No splat - regular list pattern
            let patterns = elements |> List.map (fun e -> 
                match e with 
                | RegularPattern p -> p 
                | _ -> failwith "unexpected")
            ListPattern(patterns, None)
        | Some idx when idx = elements.Length - 1 ->
            // Splat at end: [a, b, ...rest] or [a, b, ...]
            let beforeSplat = elements |> List.take idx |> List.map (fun e ->
                match e with 
                | RegularPattern p -> p 
                | _ -> failwith "unexpected")
            let splatName = 
                match elements.[idx] with
                | Splat (Some id) -> Some (IdentifierPattern id)
                | Splat None -> None
                | _ -> failwith "unexpected"
            ListPattern(beforeSplat, splatName)
        | Some idx ->
            // Splat in middle: [first, ..., last]
            let beforeSplat = elements |> List.take idx |> List.map (fun e ->
                match e with 
                | RegularPattern p -> p 
                | _ -> failwith "unexpected")
            let afterSplat = elements |> List.skip (idx + 1) |> List.map (fun e ->
                match e with 
                | RegularPattern p -> p 
                | _ -> failwith "unexpected")
            ListSplatMiddle(beforeSplat, afterSplat)

// Range pattern: 1..10
// Must use attempt to ensure proper backtracking if ".." is not found
// Guard pattern: > 10, < 5, >= 10, <= 5, == 5, != 5
let guardPattern =
    attempt (
        let guardOp = 
            choice [
                pstring ">="
                pstring "<="
                pstring "=="
                pstring "!="
                pstring ">"
                pstring "<"
            ]
        guardOp .>>. (ws >>. (literalNoWs |>> LiteralExpr))
        |>> fun (op, value) -> GuardPattern(op, value)
    )

// Tag pattern: #nil or #some(x) or #error data
// Tag pattern: #tagName or #tagName(pattern) or #tagName(p1, p2, ...)
let tagPattern =
    attempt (
        (pstring "#" >>. identifierNoWs) >>= fun tagName ->
        opt (between 
                (pstring "(") 
                (pstring ")") 
                (sepBy1 (ws >>. pattern) (pstring "," .>> ws))) >>= fun patternsOpt ->
        match patternsOpt with
        | None -> preturn (TagPattern(tagName, None))
        | Some [single] -> preturn (TagPattern(tagName, Some single))
        | Some multiple -> preturn (TagPattern(tagName, Some (TuplePattern multiple)))
    )

do patternRef := 
    choice [
        attempt elsePattern         // "else" must come before identifiers
        attempt tagPattern          // "#tag" must come before literals/identifiers
        attempt guardPattern        // "> 5" must come before operators in other contexts
        attempt literalOrRangePattern  // Handles both literals and ranges (1 or 1..10)
        attempt recordPatternByPosition  // Must come before identifierPattern
        attempt recordMemberPattern
        attempt listPattern
        attempt tuplePattern
        attempt wildcardPattern
        identifierPattern           // Last resort - any identifier
    ] .>> ws
    <?> "pattern"

// Match expression parser
do
    let matchArm =
        pstring "|" >>. ws >>. sepBy1 pattern (pstring "," .>> ws) .>>. (pstring "->" >>. ws >>. expression)
        <?> "match arm"
    
    matchExprRef :=
        pipe2
            (pstring "match" >>. ws >>. sepBy1 expression (pstring "," .>> ws))  // Multiple expressions
            (many1 (ws >>. matchArm))
            (fun scrutinees arms -> Match(scrutinees, arms))
        <?> "match expression"

// Operator precedence parser
let opp = new OperatorPrecedenceParser<Expression, unit, unit>()

// Record field parser: either "name = expr" or just "expr"
let recordField =
    (attempt (
        pipe2
            (identifierNoWs .>> ws .>> pstring "=" .>> ws)
            expression
            (fun name expr -> NamedField(name, expr))
    ) <|> (expression |>> PositionalField))
    <?> "record field"

// Tuple/Record or parenthesized expression parser
// Handles: (x), (x, y), (x = 1), (x = 1, y = 2), (x = 1, 2), etc.
let tupleOrParenExpr =
    (between
        (pstring "(")
        (pstring ")")
        (sepBy1 (ws >>. recordField) (pstring "," .>> ws))
    |>> fun fields ->
        match fields with
        | [PositionalField single] -> single  // Single expression in parens: (x) is just x
        | _ ->
            // Check if all fields are positional (for backward compat)
            let allPositional = fields |> List.forall (function PositionalField _ -> true | _ -> false)
            if allPositional then
                // Pure tuple: extract expressions
                let exprs = fields |> List.map (function PositionalField e -> e | _ -> failwith "unexpected")
                TupleExpr exprs
            else
                // Has named fields: use RecordExpr
                RecordExpr fields)
    <?> "tuple or record"

// List expression parser
let listExpr =
    between
        (pstring "[")
        (pstring "]")
        (sepBy (ws >>. expression) (pstring "," .>> ws))
    |>> ListExpr

// Tag expression parser: #tagName or #tagName(expr)
// The payload can be a tuple-like expression with comma-separated values
let tagExpr =
    pstring "#" >>. identifierNoWs >>= fun tagName ->
    opt (between 
            (pstring "(") 
            (pstring ")") 
            (sepBy1 (ws >>. expression) (pstring "," .>> ws))) >>= fun payloadOpt ->
    match payloadOpt with
    | None -> preturn (TagExpr(tagName, None))
    | Some [single] -> preturn (TagExpr(tagName, Some single))  // Single value
    | Some multiple -> preturn (TagExpr(tagName, Some (TupleExpr multiple)))  // Multiple values as tuple

// If-expression parser: if condition then trueExpr else falseExpr
let ifExpr =
    pipe3
        (pstring "if" >>. ws >>. expression)
        (ws >>. pstring "then" >>. ws >>. expression)
        (ws >>. pstring "else" >>. ws >>. expression)
        (fun cond thenExpr elseExpr -> IfExpr(cond, thenExpr, elseExpr))
    <?> "if expression"

// Primary expression (literals, identifiers, function calls, lambdas, parenthesized expressions)
// Version without trailing whitespace consumption (for use in block contexts)
let primaryExprNoWs =
    choice [
        attempt ifExpr  // if-then-else must be early to avoid partial matches
        attempt matchExpr
        attempt lambda
        attempt functionCall
        attempt tagExpr  // #tag or #tag(expr)
        literalNoWs |>> LiteralExpr
        listExpr
        tupleOrParenExpr
        identifierNoWs |>> IdentifierExpr
    ]

let primaryExpr = primaryExprNoWs .>> ws  // Consume trailing ws for operator parsing

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

// Equality operators 
opp.AddOperator(InfixOperator("==", ws, 3, Associativity.Left, fun x y -> BinaryOp("==", x, y)))
opp.AddOperator(InfixOperator("!=", ws, 3, Associativity.Left, fun x y -> BinaryOp("!=", x, y)))

// Comma operator (very low precedence - creates tuples)
// This allows: def x = 1, 2 to create a tuple without parens
// In function calls like f(x, y), the comma creates a single tuple argument
opp.AddOperator(InfixOperator(",", ws, 1, Associativity.Left, fun x y -> 
    // Only flatten if left side is already a TupleExpr from a comma operation
    // This way: 1, 2, 3 becomes TupleExpr [1; 2; 3]
    // But: (1, 2), 3 stays TupleExpr [TupleExpr [1; 2]; 3]
    match x with
    | TupleExpr xs -> TupleExpr (xs @ [y])
    | _ -> TupleExpr [x; y]
))

// Pipe parser - handles expr \func or expr \func(args)
// Note: With comma operator, \f(x, y) parses as Pipe(expr, "f", [TupleExpr[x, y]])
let pipeTarget =
    pipe2
        identifier
        (opt (between (pstring "(") (pstring ")") (ws >>. expression)))
        (fun funcName argOpt ->
            match argOpt with
            | Some arg -> (funcName, [arg])
            | None -> (funcName, [])
        )

let pipeExpr =
    let pipeOp = pstring "\\" >>. ws >>. pipeTarget
    pipe2
        opp.ExpressionParser
        (many (pipeOp .>> ws))
        (fun expr pipes ->
            pipes |> List.fold (fun acc (funcName, args) -> Pipe(acc, funcName, args)) expr
        )

// Expression parser that stops at newlines (for use in blocks)
let exprWithoutCrossingNewlines =
    let pipeOp = pstring "\\" >>. skipMany (skipAnyOf " \t") >>. pipeTarget
    pipe2
        opp.ExpressionParser
        (many (attempt (notFollowedBy newline >>. pipeOp) .>> skipMany (skipAnyOf " \t")))
        (fun expr pipes ->
            pipes |> List.fold (fun acc (funcName, args) -> Pipe(acc, funcName, args)) expr
        )

// Wire up the expression parser with piping support
do expressionRef := pipeExpr

// Wire up the lambda parser
do 
    let paramList = sepBy identifier (pstring "," .>> ws)
    let arrow = pstring "->" .>> ws
    
    // Block lambda body: two strategies depending on whether there are newlines
    // - Single line: use full expression parser with operators and pipes
    // - Multi-line: parse each line as primary expression only (simplified for now)
    let blockBody =
        // Try multi-line first (has newlines between expressions)
        let multiLine =
            let lineExpr = primaryExprNoWs .>> skipMany (skipAnyOf " \t\r")
            let lineEnd = skipMany1 newline >>. skipMany (skipAnyOf " \t\r")
            
            skipMany (skipAnyOf " \t\r\n") >>.
            lineExpr .>>. (attempt (lineEnd >>. sepEndBy1 lineExpr lineEnd)) .>>
            skipMany (skipAnyOf " \t\r\n")
            |>> (fun (first, rest) ->
                Block ((first :: rest) |> List.map ExprStatement)
            )
        
        // Single line: just parse full expression
        let singleLine =
            skipMany (skipAnyOf " \t\r\n") >>.
            expression .>>
            skipMany (skipAnyOf " \t\r\n")
        
        attempt multiLine <|> singleLine
    
    let lambdaWithParams = pipe2 (paramList .>> arrow .>> skipMany (skipAnyOf " \t\r\n")) blockBody (fun parameters body -> Lambda(parameters, body))
    let lambdaNoParams = blockBody |>> (fun body -> Lambda([], body))
    
    // Shorthand lambda: { * } means { x, y -> x * y }
    let shorthandBinaryOp =
        let operators = ["*"; "/"; "+"; "-"; "<"; ">"; "<="; ">="; "=="; "!="]
        choice (operators |> List.map (fun op ->
            pstring op >>. ws >>. notFollowedBy (skipAnyOf "0123456789.\"'([{") >>% Lambda(["x"; "y"], BinaryOp(op, IdentifierExpr "x", IdentifierExpr "y"))
        ))
    
    // Shorthand lambda: { * 2 } means { x -> x * 2 }
    // Also supports comparison operators: { > 10 } means { x -> x > 10 }
    let shorthandUnaryOp =
        let operators = [
            ("*", "*"); ("/", "/"); ("+", "+"); ("-", "-");
            ("<=", "<="); (">=", ">="); ("<", "<"); (">", ">");
            ("==", "=="); ("!=", "!=")
        ]
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
            skipMany (skipAnyOf " \t\r\n") >>. choice [
                attempt shorthandUnaryOp
                attempt shorthandBinaryOp
                attempt shorthandPropertyAccess
                attempt lambdaWithParams
                lambdaNoParams
            ] .>> skipMany (skipAnyOf " \t\r\n")
        )
        <?> "lambda expression"

// Wire up the function call parser
do 
    // Trailing lambda: function can be called with a lambda after parentheses
    // Examples: f(a, b) { x -> x + 1 } or f { x -> x + 1 } (no parens needed if only lambda arg)
    // Note: With comma operator, f(a, b) parses as f(TupleExpr[a, b]) - single tuple argument
    
    let callWithParensAndTrailing =
        pipe3
            identifier
            (between (pstring "(") (pstring ")") 
                (opt (ws >>. expression .>> ws)))  // Parse optional expression (may be tuple from comma op)
            (opt (attempt (skipMany (skipAnyOf " \t") >>. lambda)))
            (fun name exprOpt trailingLambdaOpt ->
                let baseArgs = match exprOpt with Some e -> [e] | None -> []
                match trailingLambdaOpt with
                | Some lambdaExpr -> FunctionCall(name, baseArgs @ [lambdaExpr])
                | None -> FunctionCall(name, baseArgs))
    
    let callWithOnlyTrailing =
        attempt (
            identifier >>= fun name ->
                skipMany (skipAnyOf " \t") >>.
                lookAhead (pchar '{') >>.
                lambda |>> fun lambdaExpr ->
                    FunctionCall(name, [lambdaExpr])
        )
    
    functionCallRef :=
        callWithOnlyTrailing <|> callWithParensAndTrailing
        <?> "function call"

// Forward reference for statement parsing
let statement, statementRef = createParserForwardedToRef<Statement, unit>()

// Block parser - parses indented statements after a newline
let blockExpr() : Parser<Expression, unit> =
    newline >>.
    many (pchar ' ' <|> pchar '\t') >>= fun indentChars ->
        let indentLevel = indentChars.Length
        if indentLevel > 0 then
            // Simple approach: parse as many statements as we can
            // Rely on statement parser failing when we leave the indented block
            many1 statement |>> Block
        else
            pzero

// Wire up statement parser
do
    let defStatement = 
        pipe2
            (pstring "def" >>. ws >>. identifier .>> wsNoNl() .>> pstring "=" .>> wsNoNl())
            (attempt (blockExpr()) <|> (wsNoNl() >>. expression))  // Don't consume newlines before expression
            (fun name expr -> DefStatement(name, expr))
    
    let exprStatement = wsNoNl() >>. expression |>> ExprStatement  // Only consume spaces/tabs, not newlines
    
    statementRef := (attempt defStatement <|> exprStatement) <?> "statement"

// Parser for module declaration
let moduleDecl: Parser<TopLevelItem, unit> =
    pstring "module" >>. ws >>. identifier |>> ModuleDecl
    <?> "module declaration"

// Parser for value definition (top-level)
let valueDef: Parser<TopLevelItem, unit> =
    pipe2
        (pstring "def" >>. ws >>. identifier .>> wsNoNl() .>> pstring "=")  // Don't consume newlines after =
        (attempt (blockExpr()) <|> (ws >>. expression))
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
        
        // Debug mode: if file ends with .txt, parse as pattern
        if file.EndsWith(".txt") then
            match run (literalOrRangePattern .>> eof) text with
            | Success(pat, _, _) ->
                printfn "Pattern parsed successfully:"
                printfn "  %A" pat
                0
            | Failure(errorMsg, _, _) ->
                printfn "Pattern parse error: %s" errorMsg
                1
        else
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
