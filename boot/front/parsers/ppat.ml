open! Parser

let rec parse_pattern (ps: pstate): Ast.pat = 
    let lhs = parse_pat_or ps in
    if eat ps Colon then
        let ty = parse_type ps in
        locate ps lhs.span (Ast.PAT_sig {
            psig_pat = lhs;
            psig_ty = ty;
        })
    else lhs

and parse_pat_or (ps: pstate): Ast.pat = 
    let lo = ps.peek.span in
    match repeat ~sep:Pipe parse_pat_bottom ps with
    | [| pat |] -> pat
    | pats -> locate ps lo (Ast.PAT_or pats)

and parse_pat_bottom (ps: pstate): Ast.pat = 
    let lo = ps.peek.span in
    match ps.peek.kind with
    | Ident id -> bump ps; Loc.locate lo (Ast.PAT_var id)
    | Underscore -> bump ps; Loc.locate lo Ast.PAT_wild
    | Lpar -> 
        let fields = many ~bra:Lpar ~sep:Comma ~ket:Rpar parse_pattern ps in
        locate ps lo begin match fields with
        | [| pat |] -> Ast.PAT_par pat
        | fields -> Ast.PAT_tup fields
        end
    | Plus -> make_pat_prefix Ast.Pos ps
    | Minus -> make_pat_prefix Ast.Neg ps
    | _ -> match parse_lit ps with
    | Some lit -> bump ps; locate ps lo (Ast.PAT_lit lit)
    | None -> unexpected ~expected:"a pattern" ps

and make_pat_prefix (op: Ast.unpat) (ps: pstate): Ast.pat = 
    let lo = ps.peek.span in
    locate ps lo begin
    bump ps;
    let pat = parse_pat_bottom ps in
    Ast.PAT_prefix {
        prefix_op = op;
        prefix_pat = pat;
    }
end

let parse_slot (ps: pstate): Ast.slot = 
    let lhs = parse_pat_or ps in
    expect ps Colon;
    let ty = parse_type ps in
    locate ps lhs.span ({
        Ast.slot_pat = lhs;
        Ast.slot_ty  = ty;
    })

