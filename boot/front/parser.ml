open! Token

type pstate = {
    mutable peek:   token;
    mutable last:   token;
    pstate_lexbuf:  Lexing.lexbuf;
    pstate_sess:    Session.sess;
    pstate_file:    File.name;
}

let log (ps: pstate) = Session.log
    ~name:"PARSER"
    ~flag:ps.pstate_sess.Session.sess_log_flags.parser
    ps.pstate_sess.Session.sess_log_out.parser

let make_state
    (sess: Session.sess)
    (input: File.input File.handle)
    : pstate = 
        let Input chan = input.handle in
        let lexbuf = Lexing.from_channel chan in
        Lexing.set_filename lexbuf input.file.name;
        let curr = Lexer.token lexbuf in
        let ps: pstate = {
            last = curr;
            peek = curr;
            pstate_lexbuf = lexbuf;
            pstate_sess = sess;
            pstate_file = input.file.name;
        } in
        log ps "Built parser for %s" input.file.name;
        ps

let bump (ps: pstate): unit = 
    log ps "%s: Accepted token %S" ps.pstate_file (string_of_token ps.peek);
    ps.last <- ps.peek;
    ps.peek <- Lexer.token ps.pstate_lexbuf

exception Parser_err of {
    reason: string;
    location: Loc.location;
}

let error (ps: pstate) = Printf.ksprintf begin fun reason -> 
    let location = Loc.of_position ps.pstate_file ps.peek.span.lo in
    raise (Parser_err { reason; location })
end

let unexpected ?(expected:string option) (ps: pstate) = 
    match expected with
    | None | Some "" -> error ps "Unexpected token %S" (string_of_token ps.peek)
    | Some exp -> error ps "Expected %s, but found %S" exp (string_of_token ps.peek)

let expect ?(expected:string option) (ps: pstate) (tk:token_kind) = 
    if ps.peek.kind == tk
    then bump ps
    else match expected with
    | None | Some "" -> error ps "Expected token %S, but found %S"
        (string_of_token_kind tk) (string_of_token ps.peek)
    | Some exp -> error ps "Expected %s, but found %S" exp (string_of_token ps.peek)

let eat (ps: pstate) (tk: token_kind): bool = 
    if ps.peek.kind == tk
    then (bump ps; true)
    else false

let locate
    (ps: pstate)
    (lo: Loc.span)
    ?(hi: Loc.span option)
    (node: 'a)
    : 'a Loc.located = 
        let hi = Option.value ~default:ps.last.span hi  in
        let span = Loc.combine lo hi in
        Loc.locate span node

let bracketed
    ~(bra: token_kind)
    ~(ket: token_kind)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a = 
        expect ps bra;
        let res = prule ps in
        expect ps ket;
        res

let many
    ?(min=0)
    ~(bra: token_kind)
    ?(sep: token_kind option)
    ~(ket: token_kind)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a array = 
        expect ps bra;
        let sep ps = Option.iter (expect ps) sep
        and res = ref (if min > 0 then [prule ps] else []) in
        while List.compare_length_with !res min < 0 do
            sep ps;
            res := prule ps :: !res;
        done;
        while not (ps.peek.kind == ket || eat ps Eof) do
            if not (List.is_empty !res) then sep ps;
            res := prule ps :: !res;
        done;
        expect ps ket;
        Array.of_list (List.rev !res)

let handle_errors
    ~(default:'a)
    (sess: Session.sess)
    (prule: pstate -> 'a)
    (input: File.input File.handle)
    : 'a = 
        match prule (make_state sess input) with
        | value -> value
        | exception Parser_err { location; reason } 
        | exception Lexer.Lexer_err { location; reason } -> 
            Session.error sess "%s at %s" reason (Loc.string_of_location location);
            default

