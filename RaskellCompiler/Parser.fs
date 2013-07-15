module Parser

open FParsec.Primitives
open FParsec.CharParsers

open Tree

type P<'t> = Parser<'t, unit>

let ws =
    anyOf " \t"
    |> many
    <?> "whitespace"

let w p = p .>> ws 

let asciiMixedString: P<_> = many asciiLetter
let word: P<_> = many (asciiLetter <|> pchar '_')

let ident =
    parse {
        let! fc = asciiLower
        let! w = word
        return string <| fc :: w
    } <?> "identifier"

let type_ =
    asciiUpper .>>. word
    |>> fun (fc, w) -> string <| fc :: w
    <?> "type identifier"

let param =
    ident
    <?> "parameter"

let params_ =
    many (w param)

let expr, exprRef = createParserForwardedToRef ()

(*
let app =
    chainl1 (expr |>> fun x -> [x]) (ws >>% List.append)
    |>> fun xs -> App { F = List.head xs; Args = List.tail xs }
*)

let app f = parse {
    let! args = many1 (w expr)
    return App { F = f; Args = args }
}

let var = parse {
    let! id = ident
    return Ident { Name = id }
}

let appVar = parse {
    let! first = w var
    return! choice [
        app first
        preturn first
    ]
}

do exprRef :=
    choice [
        appVar
        pchar '(' >>. expr .>> pchar ')'
    ]
    <?> "expression"

// foo x y z = <expr>
let funcDef = 
    pipe3 (w ident) (params_ .>> w (pchar '=')) expr
    <| fun id ps e -> FuncDef { Name = id; Params = ps; Expr = e; }
    <?> "top level function definition"

let topLevelDefn = funcDef