open! Parser
open! Putil

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
                    let span = Loc.combine lhs.span rhs.span in
                    let lhs = Loc.locate (Ast.EXPR_infix {
                        Ast.infix_lhs = lhs;
                        Ast.infix_op  = op;
                        Ast.infix_rhs = rhs;
                    }) span 
                    in go lhs
                end 
                else lhs
        in go lhs

and parse_expr_prefix (ps: pstate): Ast.expr = 
    let apos = lexpos ps in
    match peek ps with
    | Plus -> (bump ps; parse_expr_prefix ps) (* +expr is identical to expr *)
    | Minus -> make_expr_unary apos Ast.Neg ps (* -expr *)
    | Bang -> make_expr_unary apos Ast.Not ps (* !expr *)
    | _ -> parse_expr_call ps

and make_expr_unary (apos: Loc.position) (op: Ast.unop) (ps: pstate): Ast.expr = 
    bump ps;
    let expr = parse_expr_prefix ps in
    span_from ps apos (Ast.EXPR_prefix {
        Ast.prefix_op = op;
        Ast.prefix_expr = expr;
    })

and parse_expr_call (ps: pstate): Ast.expr = 
    let apos = lexpos ps in
    let lhs = parse_expr_bottom ps in
    match peek ps with
    | Lpar -> 
        let args = tuple ~min:0 ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        span_from ps apos (Ast.EXPR_call {
            Ast.call_fn = lhs;
            Ast.call_args = args;
        })
    | _ -> lhs

and parse_expr_bottom (ps: pstate): Ast.expr = located ps @@ fun ps -> 
    match peek ps with
    | Lpar -> 
        let tup = tuple ~min:0 ~bra:Lpar ~sep:Comma ~ket:Rpar parse_expr ps in
        begin match tup with
        | [| expr |] -> Ast.EXPR_par expr
        | fields -> Ast.EXPR_tup fields
        end
    | Ident str -> (bump ps; Ast.EXPR_var str)
    | _ -> match parse_lit ps with
    | Some lit -> (bump ps; Ast.EXPR_lit lit)
    | None -> unexpected ps

and parse_stmt_block (ps: pstate): Ast.block = located ps @@ fun ps -> 
    let stmts = ref [] in
    expect ps Lbrace;
    while not (peek ps == Rbrace) do
        stmts := parse_stmt ps :: !stmts
    done;
    expect ps Rbrace;
    Array.of_list (List.rev !stmts)

and parse_stmt (ps: pstate): Ast.stmt = located ps @@ fun ps -> 
    match peek ps with
    | Let -> 
        bump ps;
        let var = parse_ident ps in
        let rhs = match peek ps with
        | Eq -> (bump ps; Some (parse_expr ps))
        | Semi -> None
        | _ -> unexpected ps
        in
        expect ps Semi;
        Ast.STMT_let {
            Ast.let_var = var;
            Ast.let_expr = rhs;
        }
    | Semi -> (bump ps; Ast.STMT_noop)
    | _ -> 
        let expr = parse_expr ps in
        expect ps Semi;
        Ast.STMT_expr expr

