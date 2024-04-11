module Nyx.Parser.Common

open FParsec
open Nyx.Parser.AST
open Nyx.Parser.ParserTypes

let tabStopDistance = 8 // must be a power of 2

let builtInTypes = ["int"; "float"; "string"; "list"]

let parens p = between (pchar '(' .>> spaces) (spaces >>. pchar ')') p

let spaced p = spaces >>. p .>> spaces

let sepByOp op p p' = p .>> spaces .>> pstring op .>> spaces .>>. p'

let sepByCommas p = sepBy p (spaces >>. pchar ',' .>> spaces)

let sepByCommas1 p = sepBy1 p (spaces >>. pchar ',' .>> spaces)

let isBlank = function
    | ' ' | '\t' -> true
    | _ -> false

let comment: Parser<_, ParserState> = pstring "--" >>. skipRestOfLine false

let wsBeforeEOL = skipManySatisfy isBlank >>. optional comment

type CharStream = CharStream<ParserState>
type Parser<'t> = Parser<'t, ParserState>

// If this function is called at the same index in the stream
// where the function previously stopped, then the previously
// returned indentation will be returned again. 
// This way we can avoid backtracking at the end of indented blocks.
let skipIndentation (stream: CharStream) =    
    let lastParsedIndentation = stream.UserState.LastParsedIndentation
    if lastParsedIndentation.EndIndex = stream.Index then
        lastParsedIndentation.Value
    else
        let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
        while stream.Peek() = '#' do
            stream.SkipRestOfLine(false) // skip comment
            indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)
        lastParsedIndentation.EndIndex <- stream.Index
        lastParsedIndentation.Value <- indentation
        indentation

let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
    fun stream ->
        let oldIndentation = stream.UserState.Indentation
        let indentation = skipIndentation stream
        if indentation <= oldIndentation then 
            Reply(Error, expected (if indentation < 0 then "newline" else $"indented {label}"))
        else
            stream.UserState <- {stream.UserState with Indentation = indentation}            
            let results = ResizeArray()
            let mutable stateTag = stream.StateTag
            let mutable reply = p stream // parse the first element
            let mutable newIndentation = 0
            while reply.Status = Ok 
                    && (results.Add(reply.Result)
                        newIndentation <- skipIndentation stream
                        newIndentation = indentation)
                do
                    stateTag <- stream.StateTag
                    reply <- p stream
            if reply.Status = Ok 
                || (stream.IsEndOfStream && results.Count > 0 && stream.StateTag = stateTag) 
            then
                if newIndentation < indentation || stream.IsEndOfStream then
                    stream.UserState <- {stream.UserState with Indentation = oldIndentation}
                    Reply(List.ofSeq results)
                else
                    Reply(Error, messageError "wrong indentation")
            else // p failed
                Reply(reply.Status, reply.Error) 


let commonIdentifier: Parser<_, ParserState> =
    let isAsciiIdStart    = fun c -> isAsciiLower c 
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    identifier (IdentifierOptions(
                    isAsciiIdStart = isAsciiIdStart,
                    isAsciiIdContinue = isAsciiIdContinue,
                    normalization = System.Text.NormalizationForm.FormKC,
                    normalizeBeforeValidation = true,
                    allowAllNonAsciiCharsInPreCheck = true)) |>> CommonIdentifier

let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str

let str s = pstring s >>. spaces

let builtInType = choice (builtInTypes |> List.map pstring) |>> TypeIdentifier

let customType =
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    identifier (IdentifierOptions(
                    isAsciiIdStart = isAsciiUpper,
                    isAsciiIdContinue = isAsciiIdContinue,
                    normalization = System.Text.NormalizationForm.FormKC,
                    normalizeBeforeValidation = true,
                    allowAllNonAsciiCharsInPreCheck = true)) |>> TypeIdentifier

let typeIdentifier: Parser<_, ParserState> = spaces >>. choice [customType; builtInType] .>> spaces

let moduleIdentifier: Parser<_, ParserState> =
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    identifier (IdentifierOptions(
                isAsciiIdStart = isAsciiUpper,
                isAsciiIdContinue = isAsciiIdContinue,
                normalization = System.Text.NormalizationForm.FormKC,
                normalizeBeforeValidation = true,
                allowAllNonAsciiCharsInPreCheck = true)) |>> ModuleIdentifier

let punit: Parser<_, ParserState> = parens spaces |>> fun _ -> ()