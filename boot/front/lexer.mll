{
    open! Token

    exception Lexer_err of {
        location: Loc.location;
        reason: string;
    }

    let error (lexbuf: Lexing.lexbuf) = 
        let location = Loc.lexloc lexbuf.Lexing.lex_start_p in
        Printf.ksprintf (fun reason -> raise (Lexer_err { location; reason }))

    let operators = Table.alloc 32

    let _ = List.iter (fun (kwd, tok) -> Table.put operators kwd tok) [
        ("+",  Plus);
        ("-",  Minus);
        ("*",  Star);
        ("/",  Slash);
        ("%",  Percent);
        ("<",  Langle);
        ("<=", LangleEq);
        (">",  Rangle);
        (">=", RangleEq);
        ("==", EqEq);
        ("!=", NotEq);
        ("<<", Langle2);
        (">>", Rangle2);
        ("&&", AndAnd);
        ("||", OrOr);
        ("!",  Bang);
        ("&",  And);
        ("|",  Or);
        ("^",  Caret);
    ]
}

let dec = ['0'-'9']['0'-'9' '_']*
let bin = '0' 'b' ['0' '1']['0' '1' '_']*
let oct = '0' 'o' ['0'-'7']['0'-'7' '_']*
let hex = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let int = (dec | bin | oct | hex)

let ws = [' ' '\t' '\r']

let symbol = ['+' '-' '*' '/' '%' '<' '>' '=' '!' '^' '|' '&']

rule token = parse
    | '\n'          { Lexing.new_line lexbuf;
                      token lexbuf }
    | ws+           { token lexbuf }

    | '('           { Lpar }
    | ')'           { Rpar }
    | ','           { Comma }

    | symbol+ as op { match Table.search operators op with
                      | Some op -> op
                      | None -> error lexbuf "%S is not a valid operator" op }

    | int as num    { Lit_int (Int64.of_string num) }

    | eof           { Eof }
    | _ as ch       { error lexbuf "Unexpected character: %C" ch }

