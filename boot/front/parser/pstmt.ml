open! Parser

let parse_ident (ps: pstate): string = 
    match ps.peek.kind with
    | Ident id -> bump ps; id
    | _ -> unexpected ~expected:"an identifier" ps

(* Note: does NOT consume trailing semicolons, except for empty statements *)
let parse_stmt (ps: pstate): Ast.stmt = 
    let lo = ps.peek.span in
    if eat ps Let then (* Assignment *)
        let var = parse_ident ps in
        expect ps (Eq None);
        let expr = match ps.peek.kind with
        | Semi -> None
        | _ -> Some (parse_expr ps)
        in locate ps lo (Ast.STMT_let {
            let_var = var;
            let_rhs = expr;
        })
    else if eat ps Semi then (* Empty statement *)
        locate ps lo Ast.STMT_empty
    else (* Expression *)
        let expr = parse_expr ps in
        locate ps lo (Ast.STMT_expr expr)

let parse_full_stmt (ps: pstate): Ast.stmt = 
    let stmt = parse_stmt ps in
    (* Check if we need to consume a trailing semicolon *)
    match stmt.node with
    | STMT_empty | STMT_semi _ -> stmt
    | STMT_let _ -> expect ps Semi; stmt
    | STMT_expr expr when Ast.requires_semi_to_be_statement expr ->
        (* Require a semicolon only if this is not the last statement in a block *)
        if ps.peek.kind == Rbrace
        then stmt (* Last statement - do not consume trailing braces *)
        else (expect ~expected:"a trailiing semicolon" ps Semi; stmt)
    | STMT_expr _ -> stmt

let parse_block (ps: pstate): Ast.block = 
    let lo = ps.peek.span in
    expect ps Lbrace;
    let res = ref [] in
    while not (ps.peek.kind == Eof || eat ps Rbrace) do
        let stmt = parse_full_stmt ps in
        res := stmt :: !res;
    done;
    let stmts = Array.of_list (List.rev !res) in
    locate ps lo stmts

