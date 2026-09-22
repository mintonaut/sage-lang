open! Parser

let parse_lit (ps: pstate): Ast.lit option = 
    match peek ps with
    | Lit_int i -> Some (LIT_int i)
    | _ -> None

let rec parse_expr (ps: pstate): Ast.expr = 
    match peek ps with
    | Lpar -> 
        bump ps;
        let expr = parse_expr ps in
        expect Rpar ps;
        expr
    | _ -> match parse_lit ps with
    | Some lit -> bump ps; EXPR_lit lit
    | None -> unexpected ps

