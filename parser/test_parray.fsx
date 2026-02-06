open FParsec

let testParser = parray 4 (pchar ' ' <|> pchar '\t')

let test1 = run testParser "    hello"
printfn "Test 1: %A" test1

let test2 = run testParser "  hello"  
printfn "Test 2: %A" test2
