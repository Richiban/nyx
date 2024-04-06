module NyxParser.Modules

open FParsec
open NyxParser.Statements
open NyxParser.Common
open NyxParser.Types

let isBlank = fun c -> c = ' ' || c = '\t'
//let ws1 = skipMany1SatisfyL isBlank "whitespace"

let stringLiteralParser = between (pchar '"') (pchar '"') (manyChars (noneOf ['"']))
let importLiteralParser = (manyChars (noneOf ['"'; ' ']))

// let importTargetParser = ws1 >>. stringLiteralParser .>> wsBeforeEOL |>> ImportTarget

// let importSectionParser =
//     keyword "import" .>> wsBeforeEOL >>. indentedMany1 importTargetParser "Import section"

let importTargetParser = 
    stringLiteralParser <|> importLiteralParser |>> ImportTarget

let importSectionParser = 
    spaces >>. keyword "import" >>. wsBeforeEOL >>. (indentedMany1 importTargetParser "import target") |>> ImportSection
    


// Should parse:
// import
//     "test1"
//     "test2"

// as:
// ImportSection [
//     ImportTarget "test1"
//     ImportTarget "test2"
// ]

let moduleDefinition =
    pipe3
        (keyword "module" .>> spaces >>. moduleIdentifier .>> spaces)
        (opt importSectionParser)
        (many definitionParser)
        ModuleDefinition.mk

let moduleParser: Parser<_, ParserState> = spaces >>. moduleDefinition .>> eof

