open! Token

type pstate = {
    mutable peek: token;
    mutable last: token;
    pstate_lexbuf: Lexing.lexbuf;
    pstate_sess: Session.sess;
    pstate_file: File.path;
}

let log (ps: pstate) = Session.log 
    ~name:"PARSER"
    ~flag:ps.pstate_sess.Session.sess_log.log_parser
    ps.pstate_sess.Session.sess_log_out

let make_state
    (sess: Session.sess)
    (file: File.input File.handle)
    : pstate = 
        let Input ch = file.handle in
        let lexbuf = Lexing.from_channel ch in
        Lexing.set_filename lexbuf file.meta.name;
        let token = Lexer.token lexbuf in
        let ps: pstate = {
            peek = token;
            last = token;
            pstate_lexbuf = lexbuf;
            pstate_sess = sess;
            pstate_file = file.meta.name;
        } in 
        log ps "Built parser for %s" file.meta.name;
        ps

let bump (ps: pstate): unit = 
    log ps "Accepted token %S" (string_of_token ps.peek);
    ps.last <- ps.peek;
    ps.peek <- Lexer.token ps.pstate_lexbuf

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
        let hi = Option.value ~default:ps.last.span hi in
        let span = Loc.combine lo hi in
        Loc.locate span node

exception Parser_err of {
    reason: string;
    location: Loc.location;
}

let error (ps: pstate) = Printf.ksprintf begin fun reason -> 
    let pos = ps.peek.span.lo in
    let location = Loc.location ps.pstate_file pos.line pos.column in
    raise (Parser_err { location; reason })
end

let unexpected ?(expected: string option) (ps: pstate) = 
    match expected with
    | None | Some "" -> error ps "Unexpected token %S" (string_of_token ps.peek)
    | Some exp -> error ps "Expected %s, but found %S" exp (string_of_token ps.peek)

let expect ?(expected: string option) (ps: pstate) (tk: token_kind) = 
    if ps.peek.kind == tk
    then bump ps
    else match expected with
    | None | Some "" -> error ps "Expected token %S, but found %S" 
        (string_of_token_kind tk) (string_of_token ps.peek)
    | Some exp -> error ps "Expected %s, but found %S" exp (string_of_token ps.peek)

let many
    ?(min=0)
    ~(bra: token_kind)
    ?(sep: token_kind option)
    ~(ket: token_kind)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a array = 
        expect ps bra;
        let sep ps = Option.iter (expect ps) sep in
        let res = ref (if min > 0 then [prule ps] else []) in
        while List.compare_length_with !res min < 0 do
            sep ps;
            res := prule ps :: !res;
        done;
        if not (List.is_empty !res) then sep ps;
        while not (ps.peek.kind == ket || eat ps Eof) do
            res := prule ps :: !res;
            if not (ps.peek.kind == ket || eat ps Eof) then sep ps;
        done;
        expect ps ket;
        Array.of_list (List.rev !res)

(* Top-level parsing functions *)

let parse_safe
    (sess: Session.sess)
    (fn: unit -> 'a)
    : unit = 
        match fn () with
        | _ -> ()
        | exception Parser_err { reason; location } -> 
            Session.error sess "%s at %s" reason (Loc.string_of_location location)
        | exception Lexer.Lexer_err { reason; location } -> 
            Session.error sess "%s at %s" reason (Loc.string_of_location location)

(* Segregation of mutually recursive parser combinators via effects *)
open! Effect
open! Effect.Deep

type _ stmt_parser = 
    | Stmt: Ast.stmt stmt_parser
    | Block: Ast.block stmt_parser

type _ Effect.t += 
    | Parser_expr: Ast.expr t
    | Parser_stmt: 'a stmt_parser -> 'a t

let parse_expr (_: pstate): Ast.expr = perform (Parser_expr)
let parse_stmt (_: pstate): Ast.stmt = perform (Parser_stmt Stmt)
let parse_block (_: pstate): Ast.block = perform (Parser_stmt Block)

