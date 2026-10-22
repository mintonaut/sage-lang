open! Parser

type semi_behavior = 
    | Ignore (* Must have either a trailing ';' or '}' or '<eof>' *)
    | Absent (* Do not expect trailing tokens *)
    | Present (* Must have a trailing ';' only *)


let parse_stmt_core (ps: pstate): Ast.stmt =
    match ps.peek.kind with
    | Semi -> Loc.locate ps.peek.span Ast.STMT_noop
    | Fn | Const -> 
        let spell = parse_spell ps in
        Loc.locate spell.span (Ast.STMT_spell spell)
    | _ when eat ps Let -> 
        let lo = ps.last.span in
        let pat = parse_pattern ps in
        expect ps Eq;
        let expr = parse_expr ps in
        let span = Loc.combine lo expr.span in
        Loc.locate span (Ast.STMT_let {
            let_lhs = pat;
            let_rhs = expr;
        })
    | _ -> 
        let expr = parse_expr ps in
        Loc.locate expr.span (Ast.STMT_expr expr)

let parse_stmt (ps: pstate): Ast.stmt = 
    let stmt = parse_stmt_core ps in
    let needs_trail: semi_behavior = match stmt.node with
    | STMT_noop   -> Present (* { ; } is fine, { } is not captured here *)
    | STMT_let  _ -> Present (* Let-binds must end with a semi *)
    | STMT_expr e when Ast.is_block_like e -> Absent (* Block expressions acting as statements *)
    | STMT_expr e when Ast.is_statement e -> Present (* 'Pure' statements must end with a semi *)
    | STMT_semi _ | STMT_expr _| STMT_spell _ -> Ignore (* Actual expressions, or spells, where semi is handled externally *)
    in 
    
    let semi = match needs_trail with
    | Present -> true
    | Absent  -> false
    | Ignore  -> not (matches ps [| Eof; Rbrace |])
    in 

    if semi then begin
        expect ~expected:"a trailing semicolon" ps Semi; Ast.mark_trailing_semi stmt
    end else stmt

let parse_block (ps: pstate): Ast.block = locate ps ps.peek.span begin
    expect ps Lbrace;
    let stmts = ref [] in
    while not (ps.peek.kind == Rbrace || eat ps Eof) do
        stmts := parse_stmt ps :: !stmts;
    done;
    expect ps Rbrace;
    Array.of_list (List.rev !stmts)
end

