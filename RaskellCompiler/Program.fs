open FParsec
open Lexer
open Microsoft.FSharp.Text.Lexing
open Parser
open System
open System.IO
open System.Text

open FailParser
(*
let fileToLexbuf file =
    let stream = File.OpenText(file)
    LexBuffer<_>.FromTextReader stream

let stringToLexbuf str = 
    LexBuffer<_>.FromString str

let parse (text: array<string>) =
    let lexbuf = stringToLexbuf <| String.Join ("\n", text)
    try
        Parser.program Lexer.token lexbuf
    with
        | ex ->
            failwithf "Parse error at line %d char %d" lexbuf.StartPos.Line lexbuf.StartPos.Column

let parseFile file = parse (File.ReadAllLines file)
let parseText (text: string) = parse <| text.Split('\n')*)

let test p str =
    match parse p str with
        | Success (result, us, _) ->
            printfn "Success: %A" result
            printfn "Debug:\n\n%s" us.Debug.Message
        | Failure (msg, _, us)   ->
            printfn "Failure: %A\n" msg
            printfn "Debug:\n\n%s" us.Debug.Message

[<EntryPoint>]
let main argv =
    //let cu = parseText "foo a b c = f x"
    //printfn "%A" cu

    test params_ "a"
    test topLevelDefn "foo = f"
    test topLevelDefn "foo a = f x"
    test topLevelDefn "foo a b c = f z (x y)"

    0 