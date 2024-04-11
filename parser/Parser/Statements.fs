module Nyx.Parser.Statements

open FParsec
open Nyx.Parser.Common
open Nyx.Parser.TypeExpressions
open Nyx.Parser.AST
open Nyx.Parser.ParserTypes


let pstringLiteral = 
    between 
        (pchar '"') 
        (pchar '"') 
        (manyChars (noneOf ['"'])) 
    |>> StringLiteral

let pintLiteral =
    many1Satisfy isDigit
    |>> int
    |>> IntLiteral


let pexpression =
    choice [attempt pstringLiteral; pintLiteral]

let blockParser, blockParserRef = createParserForwardedToRef()

let functionArgument: Parser<_, ParserState> = 
    commonIdentifier .>> spaces .>>.
        (opt (pchar ':' .>> spaces >>. typeParser))
        |>> FunctionArgument.mk

let argumentListParser: Parser<_, ParserState> =
    parens (sepByCommas functionArgument)

let functionDefinitionParser: Parser<_, ParserState> =
    pipe3
        (keyword "def" .>> spaces >>. commonIdentifier .>> spaces)
        (argumentListParser .>> spaces .>> pstring "->" .>> spaces)
        (blockParser)
        (fun x y z -> FunctionDefinition.mk(x, y, z))

let valueBindingParser =
    (keyword "def" .>> spaces >>. commonIdentifier .>> spaces)
    .>> (pchar '=' .>> spaces .>> opt newline)
    .>>. blockParser
    |>> ValueDefinition.mk

let typeDefinitionParser =
    (keyword "type" .>> spaces >>. typeIdentifier .>> spaces)
    .>>. (pchar '=' .>> spaces >>. typeParser)
    |>> TypeDefinition.mk

let definitionParser: Parser<_, ParserState> =
    choice [
        attempt functionDefinitionParser |>> Func
        attempt typeDefinitionParser |>> Type
        valueBindingParser |>> Val
    ]

let pstatement = choice [attempt definitionParser |>> Def; pexpression |>> Expr]

do blockParserRef.Value <- indentedMany1 pstatement "block" |>> Block .>> wsBeforeEOL



// let stringValueParser = between (pchar '"') (pchar '"') (manyChars (noneOf ['"'])) |>> StringLiteral

// let intValueParser =
//     many1Satisfy isDigit
//     |>> int
//     |>> IntLiteral


// let pexpression =
//     choice [attempt stringValueParser; intValueParser]

// let blockParser, blockParserRef = createParserForwardedToRef()

// do blockParserRef := indentedMany1 pexpression "block" .>> wsBeforeEOL

// let expressionOrBlockParser =
//     choice [attempt pexpression; blockParser |>> Block]