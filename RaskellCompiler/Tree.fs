module Tree

type Name = string

and Expr = 
    | ConstInt of int
    | Var of Name
    | Type of Name
    | App of Expr * Expr list

type FuncDefRec = { Name: string; Params: string list; Expr: Expr }

type TopDecl =
    | FuncDef of FuncDefRec

type CompilationUnit = TopDecl list