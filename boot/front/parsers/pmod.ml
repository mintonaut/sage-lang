open! Parser

let parse_mod (term: Token.token_kind) (ps: pstate): unit = 
    let spells = ref [] in
    while not (eat ps term) do
        spells := Pspell.parse_spell ps :: !spells
    done;
    log ps "Parsed %d spells" (List.length !spells)

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
            | Parse Pattern -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Ppat.parse_pattern ps handler)
            end
            | Parse Slot -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Ppat.parse_slot ps handler)
            end
            | Parse Type -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Ptype.parse_type ps handler)
            end
            | Parse Spell -> Some begin fun (k: (b, _) continuation) -> 
                Deep.(continue k @@ try_with Pspell.parse_spell ps handler)
            end
            | _ -> None
        } in
        Deep.try_with (parse_mod Eof) ps handler
    end

