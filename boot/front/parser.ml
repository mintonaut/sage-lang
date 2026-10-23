open! Token

type pstate = {
    mutable peek:   token;
    mutable last:   token;
    pstate_lexbuf:  Lexing.lexbuf;
    pstate_sess:    Session.sess;
    pstate_file:    File.description;
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
            pstate_file = input.file;
        } in
        log ps "Built parser for %s" input.file.name;
        ps

let bump (ps: pstate): unit = 
    log ps "%s: Accepted token %S" ps.pstate_file.name (string_of_token ps.peek);
    ps.last <- ps.peek;
    ps.peek <- Lexer.token ps.pstate_lexbuf

exception Parser_err of {
    reason: string;
    location: Loc.location;
}

let error (ps: pstate) = Printf.ksprintf begin fun reason -> 
    let location = Loc.of_position ps.pstate_file.name ps.peek.span.lo in
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

let ignore (ps: pstate) (tk: token_kind): unit = 
    if ps.peek.kind == tk
    then bump ps

let matches (ps: pstate) (tks: token_kind array): bool = 
    Array.mem ps.peek.kind tks

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

let repeat
    ~(sep: token_kind)
    (prule: pstate -> 'a)
    (ps: pstate)
    : 'a array = 
        let res = ref [prule ps] in
        while eat ps sep do
            res := prule ps :: !res
        done;
        Array.of_list (List.rev !res)

let parse_lit (ps: pstate): Ast.lit option = 
    match ps.peek.kind with
    | Lit x -> Some { 
        lit_symb = x.symb;
        lit_kind = match x.kind with
        | Lit_int -> LIT_int
        | Lit_float -> LIT_float
    }
    | _ -> None

let parse_ident (ps: pstate): Ast.id = 
    match ps.peek.kind with
    | Ident id -> bump ps; id
    | _ -> unexpected ~expected:"an identifier" ps

(* Setup for mutually recursive definitions split amongst modules without recursive modules *)

open! Effect
open! Effect.Deep

type _ ast_parser = 
    | Expr: Ast.expr ast_parser
    | Block: Ast.block ast_parser
    | Pattern: Ast.pat ast_parser
    | Type: Ast.ty ast_parser
    | Slot: Ast.slot ast_parser
    | Spell: Ast.spell ast_parser

type _ Effect.t += Parse: 'a ast_parser -> 'a t

let parse_expr (_: pstate): Ast.expr = perform (Parse Expr)
let parse_block (_: pstate): Ast.block = perform (Parse Block)
let parse_type (_: pstate): Ast.ty = perform (Parse Type)
let parse_slot (_: pstate): Ast.slot = perform (Parse Slot)
let parse_spell (_: pstate): Ast.spell = perform (Parse Spell)
let parse_pattern (_: pstate): Ast.pat = perform (Parse Pattern)

