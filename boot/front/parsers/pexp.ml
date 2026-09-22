open! Parser

let parse_lit (ps: pstate): unit option = 
    match peek ps with
    | Lit_int _ -> Some ()
    | _ -> None

let rec parse_expr (ps: pstate): unit = 
    match peek ps with
    | Lpar -> 
        bump ps;
        parse_expr ps;
        expect Rpar ps;
    | _ -> match parse_lit ps with
    | Some _ -> bump ps; ()
    | None -> unexpected ps

