open! Loc

type ty = ty' located
and ty' = 
    | TY_var of string
    | TY_par of ty
    | TY_tup of ty array

type stmt = stmt' located
and stmt' = 
    | STMT_noop
    | STMT_let of stmt_let
    | STMT_expr of expr

and stmt_let = {
    let_var: string;
    let_expr: expr option;
}

and block = stmt array located

and expr = expr' located
and expr' = 
    | EXPR_lit of lit
    | EXPR_var of string
    | EXPR_par of expr
    | EXPR_tup of expr array
    | EXPR_infix of expr_infix
    | EXPR_prefix of expr_prefix
    | EXPR_block of block
    | EXPR_call of expr_call

and expr_infix = {
    infix_lhs: expr;
    infix_op: binop;
    infix_rhs: expr;
}

and expr_prefix = {
    prefix_op: unop;
    prefix_expr: expr;
}

and expr_call = {
    call_fn: expr;
    call_args: expr array;
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

type defn = defn' located
and defn' = 
    | DEFN_fn of defn_fn

and defn_fn = {
    fn_name: string;
    fn_input: (string * ty) array;
    fn_output: ty;
    fn_body: block option;
}

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

