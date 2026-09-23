open! Parser


let parse_lit (ps: pstate): Ast.lit option = 
    match peek ps with
    | Lit_int i -> Some (Ast.LIT_int i)
    | _ -> None

let parse_binop (ps: pstate): Ast.binop option = 
    match peek ps with
    | Plus      -> Some Ast.Add
    | Minus     -> Some Ast.Sub
    | Star      -> Some Ast.Mul
    | Slash     -> Some Ast.Div
    | Percent   -> Some Ast.Mod
    | EqEq      -> Some Ast.Eqs
    | NotEq     -> Some Ast.Neq
    | Langle    -> Some Ast.Lst
    | LangleEq  -> Some Ast.Leq
    | Rangle    -> Some Ast.Grt
    | RangleEq  -> Some Ast.Geq
    | AndAnd    -> Some Ast.And
    | OrOr      -> Some Ast.Or
    | And       -> Some Ast.BitAnd
    | Caret     -> Some Ast.BitXor
    | Or        -> Some Ast.BitOr
    | Langle2   -> Some Ast.Shl
    | Rangle2   -> Some Ast.Shr
    | _         -> None 

let rec parse_expr (ps: pstate): Ast.expr = 
    parse_expr_infix None ps

and parse_expr_infix (prec: Ast.precedence) (ps: pstate): Ast.expr = 
    let lhs = parse_expr_prefix ps in
    parse_expr_infix_rest lhs prec ps

and parse_expr_infix_rest 
    (lhs: Ast.expr)
    (m_prec: Ast.precedence)
    (ps: pstate)
    : Ast.expr = 
        let rec go (lhs: Ast.expr): Ast.expr = 
            match parse_binop ps with
            | None -> lhs
            | Some op -> 
                let op_prec = Ast.precedence op in
                if m_prec < op_prec then begin
                    bump ps;
                    let rhs = parse_expr_infix op_prec ps in
                    let lhs = Ast.EXPR_infix {
                        Ast.infix_lhs = lhs;
                        Ast.infix_op  = op;
                        Ast.infix_rhs = rhs;
                    } in go lhs
                end 
                else lhs
        in go lhs

and parse_expr_prefix (ps: pstate): Ast.expr = 
    match peek ps with
    | Plus -> parse_expr_prefix ps (* +expr is identical to expr *)
    | Minus -> make_expr_unary Neg ps (* -expr *)
    | Bang -> make_expr_unary Not ps (* !expr *)
    | _ -> parse_expr_bottom ps

and make_expr_unary (op: Ast.unop) (ps: pstate): Ast.expr = 
    bump ps;
    let expr = parse_expr_prefix ps in
    Ast.EXPR_prefix {
        Ast.prefix_op = op;
        Ast.prefix_expr = expr;
    }

and parse_expr_bottom (ps: pstate): Ast.expr = 
    match peek ps with
    | Lpar -> 
        let tup = tuple ~min:0 ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        begin match tup with
        | [| expr |] -> EXPR_par expr
        | fields -> EXPR_tup fields
        end
    | _ -> match parse_lit ps with
    | Some lit -> (bump ps; Ast.EXPR_lit lit)
    | None -> unexpected ps

