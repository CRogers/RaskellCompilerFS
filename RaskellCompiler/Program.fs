open FParsec
open Microsoft.FSharp.Text.Lexing
open System
open System.IO
open System.Text

open Parser

let test p str =
    let str = str + "\n"
    match parse p str with
        | Success (result, us, _) ->
            printfn "Success: %A" result
        | Failure (msg, _, us)   ->
            printfn "Failure: %A\n" msg
            printfn "Debug:\n\n%s" us.Debug.Message

[<EntryPoint>]
let main argv =

    test topLevelDefn "foo = f"
    test topLevelDefn "foo a = f x 3"
    test topLevelDefn "s x y z = x z (y ((z) z z))"

    0 