open! Parser

let parse_spell_const (ps: pstate): Ast.spell = locate ps ps.last.span begin
    let ident = parse_ident ps in
    expect ~expected:"a type signature" ps Colon;
    let ty = parse_type ps in
    expect ps Eq;
    let expr = parse_expr ps in
    expect ps Semi;
    Ast.SPELL_const {
        const_name = ident;
        const_ty = ty;
        const_expr = expr;
    }
end

let parse_fn_body (ps: pstate): Ast.fn_type = 
    match ps.peek.kind with
    | _ when eat ps Eq -> 
        let expr = parse_expr ps in
        (if not (Ast.is_functional expr)
        then error ps "Pure expression function bodies cannot be statements or blocks");
        (if not (Ast.is_block_like expr) then expect ps Semi);
        Ast.Fn_expr expr
    | Lbrace -> Ast.Fn_block (parse_block ps)
    | _ -> unexpected ~expected:"a function definition" ps

let parse_spell_fn (ps: pstate): Ast.spell = locate ps ps.last.span begin
    let ident = parse_ident ps in
    let args = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_slot ps in
    let ret = if eat ps Colon then Some (parse_type ps) else None in
    let body = parse_fn_body ps in
    Ast.SPELL_fn {
        fn_name = ident;
        fn_ins = args;
        fn_out = ret;
        fn_body = body;
    }
end

let parse_spell (ps: pstate): Ast.spell = 
    if eat ps Const then
        parse_spell_const ps
    else if eat ps Fn then
        parse_spell_fn ps
    else unexpected ~expected:"a spell" ps

