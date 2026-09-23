open! Token

type pstate = {
    mutable pstate_peek: token;
    pstate_sess:         Session.sess;
    pstate_lexbuf:       Lexing.lexbuf;
    pstate_file:         string;
}

let log (ps: pstate) = Session.log
    ~name:"PARSER"
    ~flag:ps.pstate_sess.Session.sess_log.log_parser
    ps.pstate_sess.Session.sess_log_out

let make_state
    (sess: Session.sess)
    (file: File.input File.handle)
    : pstate = 
        let Input channel = file.file_channel in
        let lexbuf = Lexing.from_channel channel in
        Lexing.set_filename lexbuf file.file_name;
        let token = Lexer.token lexbuf in 
        let pstate = {
            pstate_peek = token;
            pstate_sess = sess;
            pstate_lexbuf = lexbuf;
            pstate_file = file.file_name;
        } in
        log pstate "Built parser state for: %s" file.file_name;
        pstate

exception Parser_err of {
    file: string;
    reason: string;
}

let error (ps: pstate) = 
    Printf.ksprintf (fun reason -> raise (Parser_err { 
        file = ps.pstate_file; 
        reason;
    }))

let with_handle (sess: Session.sess) (thunk: unit -> unit): unit = 
    match thunk () with
    | value -> value
    | exception Parser_err { file; reason } -> 
        Session.error sess "%s: %s" file reason

    | exception Lexer.Lexer_err { file; reason } -> 
        Session.error sess "%s: %s" file reason

let peek (ps: pstate) = 
    ps.pstate_peek

let bump (ps: pstate) = 
    log ps "%s: accepted token %S" 
        ps.pstate_file (string_of_token ps.pstate_peek);
    ps.pstate_peek <- Lexer.token ps.pstate_lexbuf

let unexpected (ps: pstate) = 
    error ps "Unexpected token: %S" (string_of_token ps.pstate_peek)

let expect (ps: pstate) (tk: token) = 
    let pk = peek ps in
    if tk == pk 
    then bump ps
    else error ps "Expected token %S, found %S" 
        (string_of_token tk) (string_of_token pk)

let bracketed
    ~(bra: token)
    ~(ket: token)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a = 
        expect ps bra;
        let res = prule ps in
        expect ps ket;
        res

let tuple
    ?(min=1)
    ~(bra: token)
    ?(sep: token option)
    ~(ket: token)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a array = 
        expect ps bra;
        let separator ps = Option.iter (expect ps) sep in
        let res = ref (if min > 0 then [prule ps] else []) in
        while List.compare_length_with !res min < 0 do
            separator ps;
            res := prule ps :: !res;
        done;
        if not (List.is_empty !res) then separator ps;
        while not (peek ps == ket) do
            res := prule ps :: !res;
            if not (peek ps == ket) then separator ps;
        done;
        expect ps ket;
        Array.of_list (List.rev !res)

