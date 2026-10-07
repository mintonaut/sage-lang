open! Parser
open! Effect.Deep

let parse
    (sess: Session.sess)
    (input: File.input File.handle)
    (fn: pstate -> 'a)
    : 'a = 
        let pstate = make_state sess input in
        let rec handler: 'a. 'a effect_handler = { effc = fun (type b) (eff: b Effect.t) -> 
            match eff with
            | Parser_expr -> Some begin fun (k: (b, _) continuation) -> 
                continue k @@ try_with Pexpr.parse_expr pstate handler
            end
            | Parser_stmt Stmt -> Some begin fun (k: (b, _) continuation) -> 
                continue k @@ try_with Pstmt.parse_stmt pstate handler
            end
            | Parser_stmt Block -> Some begin fun (k: (b, _) continuation) -> 
                continue k @@ try_with Pstmt.parse_block pstate handler
            end
            | _ -> None
        } in
        try_with Pexpr.parse_expr pstate handler

let parse_src_file
    (sess: Session.sess)
    (input: File.input File.handle)
    : unit = parse_safe sess @@ fun _ -> 
        ignore (parse sess input Pexpr.parse_expr)

