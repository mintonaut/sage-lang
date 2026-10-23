open! Parser

let infer_mod_name (file: File.description): Ast.symbol = 
    let file_name = File.name file in
    String.capitalize_ascii file_name

let parse_scroll (ps: pstate): Ast.spell array = 
    let spells = ref [] in
    while not (eat ps Eof) do
        spells := Pspell.parse_spell ps :: !spells;
    done;
    Array.of_list (List.rev !spells)

let handle_errors
    (sess: Session.sess)
    (prule: pstate -> Ast.mod_scroll)
    (input: File.input File.handle)
    : Ast.mod_scroll = 
        match prule (make_state sess input) with
        | value -> value
        | exception Parser_err { location; reason } 
        | exception Lexer.Lexer_err { location; reason } -> 
            Session.error sess "%s at %s" reason (Loc.string_of_location location);
            let span = Loc.(span input.file.name (position 0 0) (position 0 0)) in
            Loc.locate span ({
                Ast.scroll_name   = infer_mod_name input.file;
                Ast.scroll_spells = [||];
            })

let parse_file (sess: Session.sess) = 
    let open! Effect in
    handle_errors sess begin fun ps -> 
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
        let modname = infer_mod_name ps.pstate_file in
        let lo = ps.peek.span in
        let spells = Deep.try_with parse_scroll ps handler in
        locate ps lo ({
            Ast.scroll_name = modname;
            Ast.scroll_spells = spells;
        })
    end

