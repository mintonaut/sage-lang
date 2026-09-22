{
    open! Token

    exception Lexer_err of {
        file: string;
        reason: string;
    }

    let error (lexbuf: Lexing.lexbuf) = 
        Printf.ksprintf (fun reason -> raise (Lexer_err { 
            file = lexbuf.Lexing.lex_start_p.pos_fname;
            reason;
        }))
}

let dec = ['0'-'9']['0'-'9' '_']*
let bin = '0' 'b' ['0' '1']['0' '1' '_']*
let oct = '0' 'o' ['0'-'7']['0'-'7' '_']*
let hex = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let int = (dec | bin | oct | hex)

let ws = [' ' '\t' '\r']

rule token = parse
    | '\n'          { Lexing.new_line lexbuf;
                      token lexbuf }
    | ws+           { token lexbuf }

    | '('           { Lpar }
    | ')'           { Rpar }
    | ','           { Comma }

    | int as num    { Lit_int (Int64.of_string num) }

    | eof           { Eof }
    | _ as ch       { error lexbuf "Unexpected character: %C" ch }

