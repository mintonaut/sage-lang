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

type unop = Pos | Neg | Not

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

(* [Note: Motivation and ambiguity of an expression-oriented syntax]
   Block-like expressions are semantically equivalent to expressions, meaning they produce some 
   kind of value. For statement blocks such as "{ 1 }", their value is the value of the last 
   expression; for 'loop'-loops its the value of the expression given to a 'break' statement; and
   so on. Importantly, these expressions can also be used at a statement context without trailing
   semicolons.

   The duality of such expressions comes with two major flaws. On one hand, it enables especially
   dumb syntax, such as "{ 1 } + 2" and "2 + loop { break 1; }". While semantically valid, using 
   block-like expressions as operands offers little benefit to internalizing the binary operations:
   "{ 1 + 2 }" and "loop { break 1 + 2; }". 

   On the other hand, it introduces ambiguity to the grammar. Since the grammar is whitespace-
   agnostic, "{ 1 } - 2" can be interpreted as a statement block followed by an unrelated expression,
   or as the binary negation of a block and a literal. Distinguishing between the two would either
   entail making the parser to be aware of newline characters, or forcing semicolons at the end of
   all block-like expressions.

   Notice both issues are only specific to infix and prefix operations. Other composing expressions
   (such as function calls) can benefit from having blocky expression as sub-expressions. Furthermore,
   let-bindings can make use of blocky expressions to encode complex assignment logic and hidden
   variables. So we certainly don't want to segregate pure expressions from statements.

   The simplest solution is to just not allow non-composable expressions in such operations. An
   infix/prefix expression can thus only use expressions that are not externally statement-like.
   Only literal and data values, accesses, function calls and other infix/prefix expressions are
   allowed as operands. A blocky expression can be used as an operand only when a part of a nested
   composable expression (such as inside parentheses). While at first glace this solution seems
   limiting, going against functional principles, it follows the syntax restrictions exhibited by
   functional languages: "1 + if e then a else b + 2" usually goes against expected behavior and
   requires additional parentheses to align them with the observed behavior - 
   "1 + (if e then a else b) + 2"
*)

(* Returns true if the expression e can be composable with other expressions. A composable 
   expression is one that can appear as an operand in a binary or unary operation. Only 'pure'
   expressions (those that are not statements nor are block-like) are composable.
   See [Note: Motivation and ambiguity of an expression-oriented syntax] *)
let is_composable (e: expr): bool = 
    not (is_block_like e || is_statement e)

(* Mark a statement as having an explicit ';'. Only affects expression-like statements *)
let mark_trailing_semi (s: stmt): stmt = 
    let stmt = match s.node with
    | STMT_expr e -> STMT_semi e
    | stmt -> stmt
    in Loc.locate s.span stmt

