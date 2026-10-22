open! Parser

let rec parse_type (ps: pstate): Ast.ty = 
    let lhs = parse_type_bottom ps in
    let rec go (lhs: Ast.ty): Ast.ty = 
        match ps.peek.kind with
        | Ident _ | Lpar | Fn -> 
            let arg = parse_type_bottom ps in
            let lhs = locate ps lhs.span (Ast.TY_app {
                ty_fn  = lhs;
                ty_arg = arg;
            }) in
            go lhs
        | _ -> lhs
    in go lhs

and parse_type_bottom (ps: pstate): Ast.ty = locate ps ps.peek.span begin
    match ps.peek.kind with
    | Ident id -> bump ps; Ast.TY_var id
    | Lpar -> 
        let fields = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_type ps in
        let ty = match fields with
        | [| ty |] -> Ast.TY_par ty
        | fields -> Ast.TY_tup fields
        in ty
    | _ when eat ps Fn -> 
        let args = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_type ps in
        let ret = if eat ps Colon
        then parse_type ps
        else Loc.locate ps.last.span (Ast.TY_tup [||]) in
        Ast.TY_fn {
            fn_ins = args;
            fn_out = ret;
        }
    | _ -> unexpected ~expected:"a type" ps
end
