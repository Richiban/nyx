module Statements

open FParsec
open Common
open TypeExpressions
open Types


let stringValueParser = between (pchar '"') (pchar '"') (manyChars (noneOf ['"'])) |>> StringLiteral

let intValueParser =
    many1Satisfy isDigit
    |>> int
    |>> IntLiteral


let pexpression =
    choice [attempt stringValueParser; intValueParser]

let blockParser, blockParserRef = createParserForwardedToRef()

do blockParserRef := indentedMany1 pexpression "block" .>> wsBeforeEOL

let expressionOrBlockParser =
    choice [attempt pexpression; blockParser |>> Block]

let functionArgument: Parser<_, UserState> = 
    pipe2
        (commonIdentifier)
        (opt (pchar ':' .>> spaces >>. typeExpression))
        (fun identifier argumentType -> {name = identifier; argumentType = argumentType})

let argumentListParser: Parser<_, UserState> =
    betweenBrackets (separatedByCommas functionArgument)

let functionDefinitionParser: Parser<_, UserState> =
    pipe3
        (keyword "def" .>> spaces >>. commonIdentifier .>> spaces)
        (argumentListParser .>> spaces .>> pstring "->" .>> spaces)
        (expressionOrBlockParser)
        (FunctionDefinition.mk)

let valueBindingParser =
   // keyword "def" >>. (ws1 >>. identifier .>> wsBeforeEOL |>> Print)
    pipe3
        (pstring "def" .>> spaces)
        (manyChars (anyOf ['a' .. 'z']) .>> spaces)
        (pchar '=' .>> spaces .>> opt newline >>. expressionOrBlockParser)
        (fun _ varName value -> { name = varName; value = value })

let definitionParser: Parser<_, UserState> =
    choice [
        attempt functionDefinitionParser |>> F
        valueBindingParser |>> V
    ]