open! Parser

let parse_mod (term: Token.token_kind) (ps: pstate): unit = 
    let stmts = ref [] in
    while not (eat ps term) do
        stmts := Pstmt.parse_stmt ps :: !stmts
    done;
    log ps "Parsed %d statements" (List.length !stmts)

let parse_file (sess: Session.sess) = 
    let open! Effect in
    handle_errors ~default:() sess begin fun ps -> 
        let rec handler: 'a. 'a Deep.effect_handler = { effc = fun (type b) (effc: b t) -> 
            match effc with
            | Parse Expr -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Pexpr.parse_expr ps handler)
            end
            | Parse Block -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Pstmt.parse_block ps handler)
            end
            | _ -> None
        } in
        Deep.try_with (parse_mod Eof) ps handler
    end

