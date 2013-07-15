module Tree

type NameRec = { Name: string }
type AppRec = { F: Expr; Args: Expr list }

and Expr = 
    | Ident of NameRec
    | Type of NameRec
    | App of AppRec

type FuncDefRec = { Name: string; Params: string list; Expr: Expr }

type TopDecl =
    | FuncDef of FuncDefRec