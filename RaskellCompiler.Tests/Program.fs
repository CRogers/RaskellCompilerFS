open FParsec
open FParsec.Primitives
open FParsec.CharParsers

open Parser
open Testing

[<EntryPoint>]
let main argv = 
    tester {
        do! blockName "simple"
        //do! showSuccessResult
        do! testMany [
            @"foo = f"
            @"foo = a b   "
            @"foo a = f x 3"
            @"foo a = f x (3 b)"
            @"    
foo x y = a b   
   bar = quux c   
   "
            @"foo a b c = a (b (c (c c) b)) c"
        ]

        do! blockName "let expressions"
        //do! showSuccessResult
        do! testMany [
            @"a = let x = b in x"
            @"a=let x=b in x"
            @"a = let x = let y = b in y in x"
        ]

        do! blockName "lambda expressions"
        //do! showSuccessResult
        do! testMany [
            @"a = \x -> x"
            @"a=\x->x"
            @"a= \x y -> y"
        ]

        do! blockName "if then else"
        do! showSuccessResult
        do! testMany [
            @"a = if b then c else d"
            @"a = if if b then t else f then c else d"
        ]
    }
    |> runTester
    |> printTestData

    0
