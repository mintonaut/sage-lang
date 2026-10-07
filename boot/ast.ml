open! Loc

type expr = expr' located
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

