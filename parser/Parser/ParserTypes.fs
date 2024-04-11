module Nyx.Parser.ParserTypes

type LastParsedIndentation() =
    [<DefaultValue>]
    val mutable Value: int32
    [<DefaultValue>]
    val mutable EndIndex: int64

type ParserState = 
    {   Indentation: int
        // We put LastParsedIndentation into the UserState so that we 
        // can conveniently use a separate instance for each stream.
        // The members of the LastParsedIndentation instance will be mutated
        // directly and hence won't be affected by any stream backtracking. 
        LastParsedIndentation: LastParsedIndentation
        CurrentlyInParens: bool
    }
    with
        static member Create() = 
            { Indentation = -1
              LastParsedIndentation = LastParsedIndentation(EndIndex = -1L)
              CurrentlyInParens = false }