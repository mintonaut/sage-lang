open! Util
open! Parser

let mk_expr_binary (lhs: Ast.expr) (op: Ast.binop) (rhs: Ast.expr): Ast.expr = 
    let span = Loc.combine lhs.span rhs.span in
    Loc.locate span (Ast.EXPR_infix {
        infix_lhs = lhs;
        infix_op = op;
        infix_rhs = rhs;
    })

let mk_expr_assign (lhs: Ast.expr) (op: Ast.assignop option) (rhs: Ast.expr): Ast.expr = 
    let span = Loc.combine lhs.span rhs.span in
    Loc.locate span begin match op with
    | None -> Ast.EXPR_assign { assign_lhs = lhs; assign_rhs = rhs }
    | Some op -> Ast.EXPR_assign_op { assign_lhs = lhs; assign_op = op; assign_rhs = rhs }
    end

let parse_lit (ps: pstate): Ast.lit option = 
    match ps.peek.kind with
    | Lit x -> 
        let kind: Ast.lit_kind = match x.kind with
        | Int -> LIT_int
        | Float -> LIT_float
        in Some {
            Ast.lit_value = x.symbol;
            Ast.lit_kind = kind;
        }
    | _ -> None

let rec parse_expr (ps: pstate): Ast.expr = 
    parse_expr_infix Unbound ps

and parse_expr_infix (min_prec: Pbinop.precedence bound) (ps: pstate): Ast.expr = 
    let lhs = parse_expr_prefix ps in
    parse_expr_infix_rest min_prec lhs ps

and parse_expr_infix_rest
    (min_prec: Pbinop.precedence bound) 
    (lhs: Ast.expr) 
    (ps: pstate)
    : Ast.expr = 
        let rec go lhs = 
            match Pbinop.accept_binop ps min_prec with
            | None -> lhs
            | Some op -> 
                bump ps;
                let prec: Pbinop.precedence bound = match Pbinop.fixity op with
                | Right -> Include (Pbinop.precedence op)
                | Left | NoFixity -> Exclude (Pbinop.precedence op)
                in 
                let rhs = parse_expr_infix prec ps in
                let expr = match op with
                | Binary op -> mk_expr_binary lhs op rhs
                | Assign maybe_op -> mk_expr_assign lhs maybe_op rhs
                in go expr
        in go lhs

and parse_expr_prefix (ps: pstate): Ast.expr = 
    match ps.peek.kind with
    | Plus -> (bump ps; parse_expr_prefix ps)
    | Minus -> make_expr_prefix Ast.Neg ps
    | Bang -> make_expr_prefix Ast.Not ps
    | _ -> parse_expr_call ps

and make_expr_prefix (op: Ast.unop) (ps: pstate): Ast.expr = 
    let lo = ps.peek.span in
    bump ps;
    let expr = parse_expr_prefix ps in
    Loc.respan (Loc.combine lo expr.span) expr

and parse_expr_call (ps: pstate): Ast.expr = 
    let lo = ps.peek.span in
    let lhs = parse_expr_bottom ps in
    match ps.peek.kind with
    | Lpar -> 
        let args = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        let span = Loc.combine lo ps.last.span in
        Loc.locate span (Ast.EXPR_call {
            call_fn   = lhs;
            call_args = args;
        })
    | _ -> lhs

and parse_expr_bottom (ps: pstate): Ast.expr = locate ps ps.peek.span begin
    match ps.peek.kind with
    | Ident id -> bump ps; Ast.EXPR_var id
    | Lpar -> 
        let fields = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        begin match fields with
        | [| expr |] -> Ast.EXPR_par expr
        | fields -> Ast.EXPR_tup fields
        end
    | _ -> match parse_lit ps with
    | Some lit -> bump ps; Ast.EXPR_lit lit
    | None -> unexpected ~expected:"an expression" ps
end

