﻿module Parser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

open System.Text

open Tree

type DebugInfo = { Message: string; Indent: int }
type UserState = { mutable Debug: DebugInfo }
type P<'t> = Parser<'t, UserState>

type DebugType<'a> = Enter | Leave of Reply<'a>

let addToDebug (stream:CharStream<UserState>) label dtype =
    let msgPadLen = 50

    let startIndent = stream.UserState.Debug.Indent
    let (str, curIndent, nextIndent) = match dtype with
        | Enter    -> sprintf "Entering %s" label, startIndent, startIndent+1
        | Leave res ->
            let str = sprintf "Leaving  %s (%A)" label res.Status
            let resStr = sprintf "%s %A" (str.PadRight(msgPadLen-startIndent-1)) res.Result
            resStr, startIndent-1, startIndent-1

    let indentStr =
        if curIndent = 0 then ""
        else "\u251C".PadRight(curIndent, '\u251C')

    let posStr = (sprintf "%A: " stream.Position).PadRight(20)
    let posIdentStr = posStr + indentStr

    // The %A for res.Result makes it go onto multiple lines - pad them out correctly
    let replaceStr = "\n" + "".PadRight(posStr.Length) + "".PadRight(curIndent, '\u2502').PadRight(msgPadLen)
    let correctedStr = str.Replace("\n", replaceStr)

    let fullStr = sprintf "%s %s\n" posIdentStr correctedStr

    stream.UserState.Debug <- {
        Message = stream.UserState.Debug.Message + fullStr
        Indent = nextIndent
    }

let (<!>) (p: P<_>) label :P<_> =
    fun stream ->
        addToDebug stream label Enter
        let reply = p stream
        addToDebug stream label (Leave reply)
        reply

let (<?!>) (p: P<_>) label :P<_> =
    p <?> label <!> label

let wschar = anyOf " \t"

let ws =
    many wschar
    |>> ignore
    <?!> "whitespace"

let ws1 =
    many1 wschar
    |>> ignore
    <?!> "whitespace1"

let w p = p .>> ws1

let nl = many skipNewline |>> ignore <?!> "newlines"

let wsn:P<_> =
    spaces

let asciiMixedString: P<_> = many asciiLetter
let word: P<_> = many (asciiLetter <|> pchar '_')

let ident =
    parse {
        let! fc = asciiLower
        let! w = word
        return string <| fc :: w
    } <?!> "identifier"

let type_ =
    asciiUpper .>>. word
    |>> fun (fc, w) -> string <| fc :: w
    <?!> "type identifier"

let param =
    ident
    <?!> "parameter"

let params_ =
    parse {
        let! ps = many (attempt (ws >>. param))
        do! ws
        return ps
    } <!> "params"

let expr, exprRef = createParserForwardedToRef ()

let var = ident |>> Var <!> "var"

let constInt =
    pint32
    |>> ConstInt
    <?!> "int32"

let exprBasic = 
    choice [
        constInt
        var
        pchar '(' >>. expr .>> pchar ')'
    ]
    <!> "exprBasic"

let app =
    parse {
        let! f = w exprBasic
        let! args = sepBy1 exprBasic ws1
        return App (f, args)
    } <!> "app"

do exprRef :=
    choice [
        attempt app
        exprBasic
    ]
    <?!> "expression"

// foo x y z = <expr>
let funcDef =
    parse {
        let! id = ident
        let! ps = params_
        let! _  = pchar '=' <!> "eq"
        do!       ws
        let! e  = expr
        let! _  = ws >>. skipNewline
        return FuncDef { Name = id; Params = ps; Expr = e; }        
    }
    //pipe3 (w ident) (params_ .>> w (pchar '=' <!> "eq")) (expr .>> ws .>> skipNewline)
    //<| fun id ps e -> FuncDef { Name = id; Params = ps; Expr = e; }
    <?!> "top level function definition"

let topLevelDefn = funcDef .>> wsn


let parse p str = runParserOnString p ({ Debug = { Message = ""; Indent = 0 } }) "" str
let parseProgram str = parse topLevelDefn str