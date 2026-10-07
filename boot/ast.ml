open! Loc

type stmt = stmt' located
and stmt' = 
    | STMT_empty
    | STMT_expr of expr
    | STMT_semi of expr
    | STMT_let of {
        let_var: string;
        let_rhs: expr option;
    }

and block = stmt array located

and expr = expr' located
and expr' = 
    | EXPR_par of expr
    | EXPR_tup of expr array
    | EXPR_var of string
    | EXPR_lit of lit
    | EXPR_infix of {
        infix_lhs: expr;
        infix_op:  binop;
        infix_rhs: expr;
    }
    | EXPR_prefix of {
        prefix_op:   unop;
        prefix_expr: expr;
    }
    | EXPR_assign of {
        assign_lhs: expr;
        assign_rhs: expr;
    }
    | EXPR_assign_op of {
        assign_lhs: expr;
        assign_op:  assignop;
        assign_rhs: expr;
    }
    | EXPR_call of {
        call_fn: expr;
        call_args: expr array;
    }
    | EXPR_block of block
    | EXPR_if of {
        if_cond: expr;
        if_then: block;
        if_else: expr option; (* Either EXPR_block or EXPR_if *)
    }
    | EXPR_while of {
        while_cond: expr;
        while_body: block;
    }

and lit = {
    lit_kind: lit_kind;
    lit_value: string;
}

and lit_kind = 
    | LIT_int
    | LIT_float

and binop = 
    | Add | Sub | Mul | Div | Mod
    | Eqs | Neq | Lst | Leq | Grt | Geq
    | And | Or
    | BitAnd | BitOr | BitXor
    | Shl | Shr

and unop = 
    | Neg | Not

and assignop = 
    | Add | Sub | Mul | Div | Mod
    | BitAnd | BitOr | BitXor
    | Shl | Shr

let is_statement (expr: expr) = 
    match expr.node with
    | EXPR_block _ 
    | EXPR_if _ 
    | EXPR_while _ -> true
    | _ -> false

let requires_semi_to_be_statement (expr: expr) = 
    (* All expressions that are NOT statements must have a trailing semicolon 
       (or closing brace) to be raised to statements. *)
    not (is_statement expr)

