module Main 

open FParsec
open Types
open Common
open TypeExpressions
open Modules 
open Statements

let runParser p s = 
    runParserOnString (p .>> spaces .>> eof) (ParserState.Create()) "" s
    |> printfn "%A"

