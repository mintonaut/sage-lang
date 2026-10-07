{
    open! Token

    exception Lexer_err of {
        location: Loc.location;
        reason: string;
    }

    let error (lexbuf: Lexing.lexbuf) = Printf.ksprintf begin fun reason -> 
        let location = Loc.lexloc lexbuf.lex_start_p in
        raise (Lexer_err { reason; location })
    end

    let keywords = Table.alloc 8;;

    List.iter (fun (kwd, tok) -> Table.put keywords kwd tok) [
        ("let", Let);
        ("while", While);
        ("if", If);
        ("else", Else);
    ]
}

let dec = ['0'-'9']['0'-'9' '_']*
let bin = '0' 'b' ['0' '1']['0' '1' '_']*
let oct = '0' 'o' ['0'-'7']['0'-'7' '_']*
let hex = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let int = bin|dec|oct|hex

let exp = ['e' 'E']['+' '-']? dec
let flo = (dec '.' dec exp?)|(dec exp)

let symbol = ['+' '-' '*' '/' '%' '<' '>' '=' '&' '|' '^' '!']
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let ws = [' ' '\t' '\r']

rule token = parse
    | ws+           { token lexbuf }
    | '\n'          { Lexing.new_line lexbuf;
                     token lexbuf }
    | '('           { Lpar }
    | ')'           { Rpar }
    | '{'           { Lbrace }
    | '}'           { Rbrace }
    | ';'           { Semi }
    | ','           { Comma }

    | "+"           { Plus }
    | "-"           { Minus }
    | "*"           { Star }
    | "/"           { Slash }
    | "%"           { Percent }
    | "=="          { EqEq }
    | "!="          { NotEq }
    | "<"           { Langle }
    | ">"           { Rangle }
    | "<="          { LangleEq }
    | ">="          { RangleEq }
    | "|"           { Or }
    | "&"           { And }
    | "||"          { OrOr }
    | "&&"          { AndAnd }
    | "^"           { Caret }
    | "<<"          { Langle2 }
    | ">>"          { Rangle2 }
    | "!"           { Bang }

    | "="           { Eq None }
    | "+="          { Eq (Some Plus) }
    | "-="          { Eq (Some Minus) }
    | "*="          { Eq (Some Star) }
    | "/="          { Eq (Some Slash) }
    | "%="          { Eq (Some Percent) }
    | "|="          { Eq (Some Or) }
    | "&="          { Eq (Some And) }
    | "^="          { Eq (Some Caret) }
    | "<<="         { Eq (Some Langle2) }
    | ">>="         { Eq (Some Rangle2) }

    | ident as id   { match Table.search keywords id with
                      | Some tok -> tok
                      | None -> Ident (id) }

    | int as symbol { Lit { symbol; kind=Int } }
    | flo as symbol { Lit { symbol; kind=Float } }

    | eof           { Eof }
    | _ as ch       { error lexbuf "Unexpected character %C" ch }

{
    let token (lexbuf: Lexing.lexbuf): token = 
        let kind = token lexbuf in
        let apos = Loc.lexpos lexbuf.lex_start_p
        and bpos = Loc.lexpos lexbuf.lex_curr_p 
        and file = lexbuf.lex_start_p.pos_fname in
        let span = Loc.span file apos bpos in
        { kind; span }
}
