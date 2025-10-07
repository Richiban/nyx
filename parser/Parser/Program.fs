open System
open System.IO
open FParsec

// AST Types
type ModuleName = string

type Identifier = string

type Pattern = 
    | IdentifierPattern of Identifier

type Expression =
    | Literal of string
    | Variable of Identifier
    | FunctionCall of Identifier * Expression list
    | Lambda of Pattern list * Body
    | IdentifierExpression of Identifier

and BodyStatement =
    | ValueDef of Identifier * Expression
    | Expression of Expression

and Body = BodyStatement list

type ModuleContent =
    | ModuleDecl of ModuleName
    | TopLevelStatement of BodyStatement

// Parser for identifiers
let identifierPattern: Parser<Pattern, unit> =
    let isIdentStart c = System.Char.IsLetter(c) || c = '_'
    let isIdentContinue c = System.Char.IsLetterOrDigit(c) || c = '_'
    pipe2 (satisfy isIdentStart) (manyChars (satisfy isIdentContinue)) (fun c s -> string c + s |> IdentifierPattern)
    .>> spaces
    <?> "Identifier pattern"

let pattern: Parser<Pattern, unit> =
    identifierPattern .>> spaces <?> "Pattern"

// Forward declaration for recursive expressions
let expression, expressionRef = createParserForwardedToRef<Expression, unit>()
let bodyStatement, bodyStatementRef = createParserForwardedToRef<BodyStatement, unit>()
let body, bodyStatementsRef = createParserForwardedToRef<Body, unit>()

// Parser for identifiers
let identifier': Parser<Identifier, unit> =
    let isIdentStart c = System.Char.IsLetter(c) || c = '_'
    let isIdentContinue c = System.Char.IsLetterOrDigit(c) || c = '_'
    pipe2 (satisfy isIdentStart) (manyChars (satisfy isIdentContinue)) (fun c s -> string c + s |> Identifier)
    .>> spaces
    <?> "Identifier"

let moduleName: Parser<ModuleName, unit> =
    let isModuleNameStart c = System.Char.IsLetter(c) || c = '_'
    let isModuleNameContinue c = System.Char.IsLetterOrDigit(c) || c = '_'
    pipe2 (satisfy isModuleNameStart) (manyChars (satisfy isModuleNameContinue)) (fun c s -> string c + s)
    .>> spaces
    <?> "Module name"

// Parser for string literals (very simple)
let stringLiteral: Parser<Expression, unit> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))
    |>> Literal
    .>> spaces
    <?> "String literal"

    // Update lambda to use the forward reference
let lambda: Parser<Expression, unit> =
    between (pstring "{") (pstring "}") (
        pipe2
            (newline |>> (fun _ -> []) <|> (sepBy pattern (pstring "," .>> spaces) .>> pstring "->" .>> spaces) )
            body
            (fun parameters body -> Lambda(parameters, body))
    )
    .>> spaces
    <?> "Lambda expression"

// Update functionCall to use the forward reference
let functionCall: Parser<Expression, unit> =
    pipe2
        identifier'
        (between (pstring "(") (pstring ")") (sepBy (lambda <|> stringLiteral <|> expression) (pstring "," .>> spaces)))
        (fun name args -> FunctionCall(name, args))
    .>> spaces
    <?> "Function call"

let identifierExpression: Parser<Expression, unit> =
    identifier' |>> IdentifierExpression .>> spaces
    <?> "Identifier expression"

let moduleDecl: Parser<ModuleContent, unit> =
    pstring "module" >>. spaces >>. moduleName |>> ModuleDecl .>> spaces
    <?> "Module declaration"

let valueDefinition: Parser<BodyStatement, unit> =
    pipe2
        (pstring "def" >>. spaces >>. identifier' .>> spaces .>> pstring "=" .>> spaces)
        (lambda <|> stringLiteral <|> functionCall <|> (identifier' |>> IdentifierExpression))
        (fun name expr -> ValueDef(name, expr))
    <?> "Value definition"

let moduleContent: Parser<ModuleContent list, unit> =
    many (moduleDecl <|> (bodyStatement |>> TopLevelStatement)) .>> eof <?> "Module content"

// Assign the actual implementation to the forward reference
do 
    expressionRef :=
        choice [
            attempt lambda
            attempt functionCall
            attempt stringLiteral
            attempt identifierExpression
        ]
        <?> "Expression"

    bodyStatementRef := 
        (valueDefinition <|> (expression |>> Expression))
        <?> "Body statement"

    bodyStatementsRef :=
        many (bodyStatement .>> spaces)
        <?> "Body statements"

// Parse a file and print the AST or error
[<EntryPoint>]
let main argv =
    let file = @"C:\Users\RichardGibson\Source\Richiban\nyx\parser\sample.nyx"
    let text = File.ReadAllText(file)
    match run moduleContent text with
    | Success(result, _, _) ->
        printfn "Parsed successfully: %A" result
        0
    | Failure(_, err, _) ->
        printfn "Error: %A" err
        1
