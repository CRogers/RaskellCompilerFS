open FParsec.CharParsers
open Parser

let test p str =
    match run p str with
        | Success (result, _, _)  -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test topLevelDefn "abc b c = (d e) f"

    printfn "%A" argv
    0 // return an integer exit code