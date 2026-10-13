open! Parser

let parse_mod (term: Token.token_kind) (ps: pstate): Ast.expr = 
    let expr = Pexpr.parse_expr ps in
    expect ps term;
    expr

let parse_file (sess: Session.sess) = 
    handle_errors ~default:() sess begin fun ps -> 
        ignore (parse_mod Eof ps) 
    end

