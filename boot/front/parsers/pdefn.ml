open! Parser
open! Putil

let parse_fn_inputs (ps: pstate): (string * Ast.ty) = 
    let arg = parse_ident ps in 
    let ty = Ptype.parse_ty_slot ps in
    arg, ty

let parse_fn_body (ps: pstate): Ast.block option = 
    match peek ps with
    | Semi -> (bump ps; None)
    | _ -> Some (Pexp.parse_stmt_block ps)

let parse_fn_defn (ps: pstate): Ast.defn = located ps @@ fun ps -> 
    expect ~expected:"a function declaration" ps Fn;
    let ident = parse_ident ps in
    let input = tuple ~min:0 ~bra:Lpar ~sep:Comma ~ket:Rpar parse_fn_inputs ps in
    let output = match peek ps with
    | Colon -> Ptype.parse_ty_slot ps
    | _ -> nospan ps (Ast.TY_tup [||])
    in
    let body = parse_fn_body ps in
    Ast.DEFN_fn {
        Ast.fn_name = ident;
        Ast.fn_input = input;
        Ast.fn_output = output;
        Ast.fn_body = body;
    }

