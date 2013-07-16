module FParsecProblem

open FParsec
open FParsec.Primitives
open FParsec.CharParsers


type UserState = { Debug: string }
type P<'t> = Parser<'t, UserState>

type DebugType = Enter | Leave of ReplyStatus

let addToDebug (stream:CharStream<UserState>) label dtype =
    let str = match dtype with
        | Enter    -> sprintf "Entering %s" label
        | Leave rs -> sprintf "Leaving  %s (%A)" label rs

    let posStr = sprintf "%A: \t%s\n" stream.Position str 

    // Append the message to Debug
    stream.UserState <- {
        Debug = stream.UserState.Debug + posStr
    }

let (<!>) (p: P<_>) label :P<_> =
    fun stream ->
        addToDebug stream label Enter
        let reply = p stream
        addToDebug stream label (Leave reply.Status)
        reply


// Top one doesn't have tracing, the bottom does. Matches (a (ba)*)?
let sepByTest:P<_> = sepBy (pchar 'a') (pchar 'b')
let sepByTestTrace = sepBy (pchar 'a' <!> "a") (pchar 'b' <!> "b") <!> "sepByTestDebug"


// Simple function to test a parser
let test p str =
    match runParserOnString p ({ Debug = "" }) "" str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (msg, _, us)   ->
            printfn "Failure: %A\n" msg
            printfn "Debug:\n\n%s" us.Debug


let main argv =
    // All of the four below should succeed
    printfn "Without tracing:\n"
    test sepByTest ""
    test sepByTest "a"

    // But these don't! (tracing is enabled below)
    printfn "\n\nWith tracing:\n"
    test sepByTestTrace ""
    test sepByTestTrace "a"

    0
