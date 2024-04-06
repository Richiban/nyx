module NyxParser.Tests.Utils
open FParsec
open NyxParser.Types

let runParser p s = 
    match runParserOnString (p .>> spaces .>> eof) (ParserState.Create()) "" s with
    | Success (a, _, _) -> a
    | Failure (a, _, _) -> failwith a

let runFailingParser p s =
    match runParserOnString (p .>> spaces .>> eof) (ParserState.Create()) "" s with
    | Success (_, _, _) -> failwith "Expected parser to fail"
    | Failure (a, _, _) -> a.Split '\n'