module Parser

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
        | Enter    -> sprintf "\u250CEntering %s" label, startIndent, startIndent+1
        | Leave res ->
            let str = sprintf "\u2514Leaving  %s (%A)" label res.Status
            let resStr = sprintf "%s %A" (str.PadRight(msgPadLen-startIndent-1)) res.Result
            resStr, startIndent-1, startIndent-1

    let indentStr =
        //if curIndent = 0 then ""
        "".PadRight(curIndent, '\u2502')

    let posStr = (sprintf "%A: " stream.Position).PadRight(20)
    let posIdentStr = posStr + indentStr

    // The %A for res.Result makes it go onto multiple lines - pad them out correctly
    let replaceStr = "\n" + "".PadRight(posStr.Length) + "".PadRight(curIndent, '\u2502').PadRight(msgPadLen)
    let correctedStr = str.Replace("\n", replaceStr)

    let fullStr = sprintf "%s%s\n" posIdentStr correctedStr

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

let wschar = anyOf " \t" <?> "whitespace"

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

let keyword:P<_> = choice (Seq.map pstring ["let"; "in"; "if"; "then"; "else"])

let ident =
    notFollowedBy keyword
    >>. parse {
        let! fc = asciiLower
        let! w = word
        return string <| fc :: w
    }
    
    <?!> "identifier"

let type_ =
    asciiUpper .>>. word
    |>> fun (fc, w) -> string <| fc :: w
    <?!> "type identifier"

let param =
    ident
    <?!> "parameter"

let params_ =
    parse {
        let! ps = many (attempt (ws1 >>. param))
        do! ws
        return ps
    } <!> "params"

let params1 =
    parse {
        let! p = param
        let! ps = many (attempt (ws1 >>. param))
        return p :: ps
    }

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

let letExpr =
    parse {
        do! skipString "let"
        do! ws1
        let! n = ident
        do! ws >>. (skipChar '=' <!> "eq") >>. ws
        let! e1 = expr
        do! ws1 >>. skipString "in" >>. ws1
        let! e2 = expr
        return Let (n, e1, e2)
    } <?!> "let expression"

let lambda =
    parse {
        do! skipChar '\\'
        let! ps = params1
        do! ws
        do! skipString "->" <!> "->"
        do! ws
        let! e = expr
        return Lambda (ps, e)
    } <?!> "lambda"

let ifThenElse =
    parse {
        do! skipString "if"
        do! ws1
        let! e1 = expr
        do! ws1 >>. skipString "then" >>. ws1
        let! e2 = expr
        do! ws1 >>. skipString "else" >>. ws1
        let! e3 = expr
        return If (e1, e2, e3)
    } <?!> "if expression"

let app =
    parse {
        let! f = exprBasic
        let! args = many1 (attempt (ws1 >>. exprBasic))
        return App (f, args)
    } <!> "app"

do exprRef :=
    choice [
        letExpr
        lambda
        ifThenElse
        attempt app
        exprBasic
    ]
    <?!> "expression"

let funcDef =
    parse {
        let! id = ident
        let! ps = params_
        let! _  = pchar '=' <!> "eq"
        do!       ws
        let! e  = expr
        let! _  = ws >>. skipNewline
        return FuncDef { Name = id; Expr = if ps.Length = 0 then e else Lambda (ps, e) }        
    } <?!> "top level function definition"

let topLevelDefn = funcDef

let compilationUnit = wsn >>. many (topLevelDefn .>> (wsn <!> "endspace"))


let parse p str = runParserOnString p ({ Debug = { Message = ""; Indent = 0 } }) "" str
let parseProgram str = parse compilationUnit str