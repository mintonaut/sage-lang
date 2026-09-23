open! Loc

type expr = expr' located
and expr' = 
    | EXPR_lit of lit
    | EXPR_par of expr
    | EXPR_tup of expr array
    | EXPR_infix of expr_infix
    | EXPR_prefix of expr_prefix

and expr_infix = {
    infix_lhs: expr;
    infix_op: binop;
    infix_rhs: expr;
}

and expr_prefix = {
    prefix_op: unop;
    prefix_expr: expr;
}

and lit = 
    | LIT_int of Int64.t

and binop = 
    | Add | Sub | Mul | Div | Mod
    | Lst | Leq | Grt | Geq | Eqs | Neq
    | Shl | Shr
    | BitAnd | BitOr | BitXor
    | And | Or

and unop = 
    | Neg | Not

type precedence = 
    | None
    | Or (* || *)
    | And (* && *)
    | Compare (* < <= > >= == != *)
    | BitOr (* | *)
    | BitXor (* ^ *)
    | BitAnd (* & *)
    | Shift (* << >> *)
    | Sum (* + - *)
    | Product (* * / % *)

let precedence (binop: binop): precedence = 
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

