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
    (* True expressions *)
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
    (* Statements *)
    | EXPR_block of block
    | EXPR_if of {
        if_cond: expr;
        if_then: block;
        if_else: expr option;
    }
    | EXPR_while of {
        while_cond: expr;
        while_body: block;
    }
    | EXPR_loop of block
    | EXPR_break of expr option

and stmt  = stmt' located
and stmt' = 
    | STMT_noop
    | STMT_expr of expr
    | STMT_semi of expr
    | STMT_let of {
        let_lhs: id;
        let_rhs: expr;
    }

and block = stmt array located

(* Returns true if the expression e has a block-y form, i.e. they define sequences of statements *)
let is_block_like (e: expr): bool = 
    match e.node with
    | EXPR_if _ -> true
    | EXPR_while _ -> true
    | EXPR_block _ -> true
    | EXPR_loop _ -> true
    | _ -> false

(* Returns true if the expression e can only be interpreted as a statement *)
let is_statement (e: expr): bool = 
    match e.node with
    | EXPR_break _ -> true
    | _ -> false

(* Mark a statement as having an explicit ';'. Only affects expression-like statements *)
let mark_trailing_semi (s: stmt): stmt = 
    let stmt = match s.node with
    | STMT_expr e -> STMT_semi e
    | stmt -> stmt
    in Loc.locate s.span stmt

