open FParsec
open Microsoft.FSharp.Text.Lexing
open System
open System.IO
open System.Text

open Parser

let test p str =
    match parse p str with
        | Success (result, us, _) -> printfn "Success: %A" result
        | Failure (msg, _, us)   ->
            printfn "Failure: %A\n" msg
            printfn "Debug:\n\n%s" us.Debug.Message

[<EntryPoint>]
let main argv =

    test params_ "a"
    test topLevelDefn "foo = f"
    test topLevelDefn "foo a = f x"
    test topLevelDefn "foo a b c = f z (x y)"

    0 