open! Parser

type precedence = 
    | Or (* || *)
    | And (* && *)
    | Compare (* < <= > >= == != *)
    | BitOr (* | *)
    | BitXor (* ^ *)
    | BitAnd (* & *)
    | Shift (* << >> *)
    | Sum (* + - *)
    | Product (* * / % *)

let precedence (binop: Ast.binop): precedence = 
    match binop with
    | Add | Sub -> Sum
    | Mul | Div | Mod -> Product
    | Lst | Leq | Grt | Geq | Eqs | Neq -> Compare
    | Shl | Shr -> Shift
    | BitAnd -> BitAnd
    | BitOr -> BitOr
    | BitXor -> BitXor
    | And -> And
    | Or -> Or

let parse (ps: pstate): Ast.binop option = 
    match peek ps with
    | Plus      -> Some Ast.Add
    | Minus     -> Some Ast.Sub
    | Star      -> Some Ast.Mul
    | Slash     -> Some Ast.Div
    | Percent   -> Some Ast.Mod
    | EqEq      -> Some Ast.Eqs
    | NotEq     -> Some Ast.Neq
    | Langle    -> Some Ast.Lst
    | LangleEq  -> Some Ast.Leq
    | Rangle    -> Some Ast.Grt
    | RangleEq  -> Some Ast.Geq
    | AndAnd    -> Some Ast.And
    | OrOr      -> Some Ast.Or
    | And       -> Some Ast.BitAnd
    | Caret     -> Some Ast.BitXor
    | Or        -> Some Ast.BitOr
    | Langle2   -> Some Ast.Shl
    | Rangle2   -> Some Ast.Shr
    | _         -> None 

