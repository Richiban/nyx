module Nyx.Parser.TypeExpressions

open FParsec
open Nyx.Parser.AST
open Nyx.Parser.Common

module private TypeExpressions =
  let typeParser, typeExpressionRef = createParserForwardedToRef()

  let namedFieldParser =
      sepByOp ":" commonIdentifier typeParser 
      |>> NamedField

  let tupleParser =
      (choice [
        attempt <| sepByCommas1 namedFieldParser 
        |>> Tuple
        
        typeParser .>> spaces .>> pchar ',' .>> spaces .>>. sepByCommas1 typeParser
        |>> (fun (x, xs) -> Tuple (x::xs))
      ])
  
  /// Requires the given parser to be surrounded by parentheses only if the parser state
  /// is not already in parentheses
  let parensNotDoubled (p: Parser<'t>) : Parser<_> =
    fun stream ->
        let inParens = stream.UserState.CurrentlyInParens

        if inParens then
            stream.UserState <- {stream.UserState with CurrentlyInParens = false}
            let reply = p stream
            reply
        else
            stream.UserState <- {stream.UserState with CurrentlyInParens = true}
            let reply = (parens p) stream
            reply

  let tag =
    pstring "#" >>. commonIdentifier |>> Tag

  let typeOperators = choice [
      pstring "|" >>% (fun x y -> Union (x, y))
      pstring "&" >>% (fun x y -> Intersection (x, y))
      pstring "->" >>% (fun x y -> FunctionType (x, y))
  ]

  let typeCallParser = 
    typeIdentifier .>> spaces .>>. parensNotDoubled typeParser 
    |>> fun (x, y) -> TypeCall(Identifier x, y)

  let unitTypeParser = 
    pchar '(' .>> spaces .>> pchar ')' 
    |>> (fun _ -> Unit)

  let identifierTypeParser =
    attempt typeIdentifier |>> Identifier

  let term = spaced <| choice [
      tag
      attempt typeCallParser
      attempt unitTypeParser
      attempt <| parensNotDoubled tupleParser
      attempt <| parens (spaced typeParser)      
      attempt identifierTypeParser
      attempt namedFieldParser
  ]

  do typeExpressionRef.Value <- chainl1 term typeOperators

let typeParser = TypeExpressions.typeParser