open! Util
open! Parser
open! Pbinop

let mk_expr_binary (lhs: Ast.expr) (op: Ast.binop) (rhs: Ast.expr): Ast.expr = 
    let span = Loc.combine lhs.span rhs.span in
    Loc.locate span (Ast.EXPR_infix {
        infix_lhs = lhs;
        infix_op  = op;
        infix_rhs = rhs;
    })

let mk_expr_assign (lhs: Ast.expr) (op: Ast.asgop option) (rhs: Ast.expr): Ast.expr = 
    let span = Loc.combine lhs.span rhs.span in
    Loc.locate span begin match op with
    | None -> Ast.EXPR_assign { assign_lhs = lhs; assign_rhs = rhs }
    | Some op -> Ast.EXPR_assign_op { assign_lhs = lhs; assign_op = op; assign_rhs = rhs }
    end

let parse_lit (ps: pstate): (string * Ast.lit) option = 
    match ps.peek.kind with
    | Lit x -> 
        let kind: Ast.lit = match x.kind with
        | Lit_int -> LIT_int
        | Lit_float -> LIT_float
        in Some (x.symb, kind)
    | _ -> None

let rec parse_expr (ps: pstate): Ast.expr = 
    parse_expr_infix Unbound ps

and parse_expr_infix (min_prec: precedence bound) (ps: pstate): Ast.expr = 
    let lhs = parse_expr_prefix ps in
    parse_expr_infix_rest lhs min_prec ps

and parse_expr_infix_rest 
    (lhs: Ast.expr)
    (min_prec: precedence bound)
    (ps: pstate)
    : Ast.expr = 
        let rec go lhs = 
            match check_binop min_prec ps with
            | None -> lhs
            | Some binop -> 
                bump ps;
                let prec: precedence bound = match fixity binop with
                | Right -> Include (precedence binop)
                | Left | NoFixity -> Exclude (precedence binop)
                in 
                let rhs = parse_expr_infix prec ps in
                let expr = match binop with
                | Binary op -> mk_expr_binary lhs op rhs
                | Assign maybe_op -> mk_expr_assign lhs maybe_op rhs
            in go expr
        in go lhs

and parse_expr_prefix (ps: pstate): Ast.expr = 
    match ps.peek.kind with
    | Plus -> bump ps; parse_expr_prefix ps
    | Minus -> make_expr_prefix Ast.Neg ps
    | Bang -> make_expr_prefix Ast.Not ps
    | _ -> parse_expr_call ps

and make_expr_prefix (op: Ast.unop) (ps: pstate): Ast.expr = locate ps ps.peek.span begin
    bump ps;
    let expr = parse_expr_prefix ps in
    Ast.EXPR_prefix {
        prefix_op = op;
        prefix_expr = expr;
    }
end

and parse_expr_call (ps: pstate): Ast.expr = 
    let lhs = parse_expr_bottom ps in
    match ps.peek.kind with
    | Lpar -> 
        let args = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        let span = Loc.combine lhs.span ps.last.span in
        Loc.locate span (Ast.EXPR_call {
            call_fn   = lhs;
            call_args = args;
        })
    | _ -> lhs

and parse_expr_bottom (ps: pstate): Ast.expr = 
    let lo = ps.peek.span in
    match ps.peek.kind with
    | Ident id -> bump ps; Loc.locate ps.last.span (Ast.EXPR_var id)
    | Lpar -> 
        let fields = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        let expr = match fields with
        | [| expr |] -> Ast.EXPR_par expr
        | fields -> Ast.EXPR_tup fields
        in locate ps lo expr
    | _ -> match parse_lit ps with
    | Some (symb, kind) -> 
        bump ps; 
        Loc.locate lo (Ast.EXPR_lit { lit_symb = symb; lit_kind = kind })
    | None -> unexpected ~expected:"an expression" ps

