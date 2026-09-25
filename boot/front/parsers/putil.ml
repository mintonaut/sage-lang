open! Parser

let parse_ident (ps: pstate): string = 
    match peek ps with
    | Ident str -> (bump ps; str)
    | _ -> unexpected ~expected:"an identifier" ps

