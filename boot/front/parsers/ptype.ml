open! Parser

let rec parse_ty (ps: pstate): Ast.ty = located ps @@ fun ps -> 
    match peek ps with
    | Ident str -> (bump ps; Ast.TY_var str)
    | Lpar -> 
        let tup = tuple ~min:0 ~bra:Lpar ~sep:Comma ~ket:Rpar parse_ty ps in
        begin match tup with
        | [| ty |] -> Ast.TY_par ty
        | fields -> Ast.TY_tup fields
        end
    | _ -> unexpected ~expected:"a type" ps

let parse_ty_slot (ps: pstate): Ast.ty = 
    expect ps Colon;
    parse_ty ps

