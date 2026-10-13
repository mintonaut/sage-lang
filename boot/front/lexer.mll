{
    open! Token

    exception Lexer_err of {
        reason: string;
        location: Loc.location;
    }

    let error (lexbuf: Lexing.lexbuf) = Printf.ksprintf begin fun reason -> 
        let location = Loc.lexloc lexbuf.lex_start_p in
        raise (Lexer_err { location; reason })
    end
}

let bin = '0' 'b' ['0' '1']['0' '1' '_']*
let dec = ['0'-'9']['0'-'9' '_']*
let oct = '0' 'o' ['0'-'7']['0'-'7' '_']*
let hex = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let int = (bin|dec|oct|hex)

let exp = ['e' 'E']['+' '-']? dec
let flo = (dec '.' dec exp?) | (dec exp)

let symbol = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

let ws = [' ' '\t' '\r']

rule token = parse
    | '\n'          { Lexing.new_line lexbuf;
                      token lexbuf }
    | ws+           { token lexbuf }

    | "("           { Lpar }
    | ")"           { Rpar }
    | ","           { Comma }

    | "+"           { Plus }
    | "-"           { Minus }
    | "*"           { Star }
    | "/"           { Slash }
    | "%"           { Percent }
    | "&"           { And }
    | "|"           { Or }
    | "^"           { Caret }
    | "!"           { Bang }
    | "<"           { Langle }
    | ">"           { Rangle }
    | "=="          { EqEq }
    | "!="          { BangEq }
    | "<="          { LangleEq }
    | ">="          { RangleEq }
    | "<<"          { Langle2 }
    | ">>"          { Rangle2 }
    | "&&"          { AndAnd }
    | "||"          { OrOr }

    | "="           { Assign None }
    | "+="          { Assign (Some Plus) }
    | "-="          { Assign (Some Minus) }
    | "*="          { Assign (Some Star) }
    | "/="          { Assign (Some Slash) }
    | "%="          { Assign (Some Percent) }
    | "&="          { Assign (Some And) }
    | "|="          { Assign (Some Or) }
    | "^="          { Assign (Some Caret) }
    | "<<="         { Assign (Some Langle2) }
    | ">>="         { Assign (Some Rangle2) }

    | symbol as id  { Ident id }

    | int as symb   { Lit { symb; kind=Lit_int } }
    | flo as symb   { Lit { symb; kind=Lit_float } }

    | eof           { Eof }
    | _ as ch       { error lexbuf "Unexpected character %C" ch }

{
    let token (lexbuf: Lexing.lexbuf): token = 
        let kind = token lexbuf in
        let span = Loc.lexspan lexbuf in
        { kind; span }
}
