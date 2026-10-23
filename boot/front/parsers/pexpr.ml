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

let ensure_composable (prule: pstate -> Ast.expr) (ps: pstate): Ast.expr = 
    let expr = prule ps in
    if Ast.is_composable expr
    then expr
    else error ps "expected a composable expression"

let rec parse_expr (ps: pstate): Ast.expr = 
    parse_expr_infix Unbound ps

and parse_expr_infix (min_prec: precedence bound) (ps: pstate): Ast.expr = 
    let lhs = parse_expr_prefix ps in
    if not (Ast.is_composable lhs) then lhs else
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
                let rhs = ensure_composable (parse_expr_infix prec) ps in
                let expr = match binop with
                | Binary op -> mk_expr_binary lhs op rhs
                | Assign maybe_op -> mk_expr_assign lhs maybe_op rhs
            in go expr
        in go lhs

and parse_expr_prefix (ps: pstate): Ast.expr = 
    match ps.peek.kind with
    | Plus -> make_expr_prefix Ast.Pos ps
    | Minus -> make_expr_prefix Ast.Neg ps
    | Bang -> make_expr_prefix Ast.Not ps
    | _ -> parse_expr_call ps

and make_expr_prefix (op: Ast.unop) (ps: pstate): Ast.expr = 
    let lo = ps.peek.span in
    locate ps lo begin
    bump ps;
    let expr = ensure_composable parse_expr_prefix ps in
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
    | Lbrace -> parse_expr_block ps
    | _ when eat ps While -> parse_expr_while lo ps
    | _ when eat ps If -> parse_expr_if lo ps
    | _ when eat ps Match -> parse_expr_match lo ps
    | _ when eat ps Loop -> parse_expr_loop lo ps
    | _ when eat ps Break -> parse_expr_break lo ps
    | _ when eat ps Return -> parse_expr_return lo ps
    | _ -> match parse_lit ps with
    | Some lit -> 
        bump ps; 
        Loc.locate lo (Ast.EXPR_lit lit)
    | None -> unexpected ~expected:"an expression" ps

and parse_expr_block (ps: pstate): Ast.expr = 
    let block = parse_block ps in
    Loc.locate block.span (Ast.EXPR_block block)

and parse_expr_while (lo: Loc.span) (ps: pstate): Ast.expr = 
    let cond = parse_expr ps in
    let body = parse_block ps in
    locate ps lo (Ast.EXPR_while {
        while_cond = cond;
        while_body = body;
    })

and parse_expr_if (lo: Loc.span) (ps: pstate): Ast.expr = 
    let cond = parse_expr ps in
    let bthen = parse_block ps in
    let belse = match eat ps Else with
    | false -> None
    | true -> match ps.peek.kind with
    | Lbrace -> Some (parse_expr_block ps)
    | _ when eat ps If -> Some (parse_expr_if ps.last.span ps)
    | _ -> unexpected ~expected:"an if expression or a block" ps
    in locate ps lo (Ast.EXPR_if {
        if_cond = cond;
        if_then = bthen;
        if_else = belse;
    })

and parse_expr_match (lo: Loc.span) (ps: pstate): Ast.expr = 
    let value = parse_expr ps in
    let alts = parse_match_alts ps in
    locate ps lo (Ast.EXPR_match {
        match_value = value;
        match_alts  = alts;
    })

and parse_match_alts (ps: pstate): Ast.match_alt Loc.located array = 
    expect ps Lbrace;
    let alts = ref [parse_match_alt ps] in
    while not (eat ps Rbrace) do
        alts := parse_match_alt ps :: !alts;
    done;
    Array.of_list (List.rev !alts)

and parse_match_alt (ps: pstate): Ast.match_alt Loc.located = 
    let lo = ps.peek.span in
    let pat = parse_pattern ps in
    let guard = if eat ps If then Some (parse_expr ps) else None in
    expect ps EqRangle;
    let expr = parse_expr ps in
    let alt = locate ps lo ({
        Ast.alt_pat   = pat;
        Ast.alt_guard = guard;
        Ast.alt_rhs   = expr;
    }) in
    match ps.peek.kind with
    | Rbrace -> alt
    | _ when Ast.is_block_like expr -> ignore ps Comma; alt
    | _ -> expect ps Comma; alt

and parse_expr_loop (lo: Loc.span) (ps: pstate): Ast.expr = 
    let body = parse_block ps in
    locate ps lo (Ast.EXPR_loop body)

and parse_expr_break (lo: Loc.span) (ps: pstate): Ast.expr = 
    let expr = match ps.peek.kind with
    | Semi -> None
    | _ -> Some (parse_expr ps)
    in locate ps lo (Ast.EXPR_break expr)

and parse_expr_return (lo: Loc.span) (ps: pstate): Ast.expr = 
    let expr = match ps.peek.kind with
    | Semi -> None
    | _ -> Some (parse_expr ps)
    in locate ps lo (Ast.EXPR_return expr)

