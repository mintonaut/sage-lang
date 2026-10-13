open Loc

type id = string
type symbol = string

type lit = 
    | LIT_int
    | LIT_float
    | LIT_string

type binop = 
    | Add | Sub | Mul | Div | Mod
    | Eqs | Neq | Lst | Leq | Grt | Geq
    | And | Or
    | BitAnd | BitXor | BitOr
    | Shl | Shr

type unop = Neg | Not

type asgop = 
    | Add | Sub | Mul | Div | Mod
    | BitAnd | BitXor | BitOr
    | Shl | Shr

type expr = expr' located
and expr' = 
    | EXPR_var of id
    | EXPR_par of expr
    | EXPR_tup of expr array
    | EXPR_lit of {
        lit_kind: lit;
        lit_symb: symbol;
    }
    | EXPR_prefix of {
        prefix_op: unop;
        prefix_expr: expr;
    }
    | EXPR_infix of {
        infix_lhs: expr;
        infix_op:  binop;
        infix_rhs: expr;
    }
    | EXPR_assign of {
        assign_lhs: expr;
        assign_rhs: expr;
    }
    | EXPR_assign_op of {
        assign_lhs: expr;
        assign_op:  asgop;
        assign_rhs: expr;
    }
    | EXPR_call of {
        call_fn: expr;
        call_args: expr array;
    }

