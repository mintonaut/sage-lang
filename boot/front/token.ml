type token_kind = 
    | Lpar | Rpar
    | Comma

    | Assign of token_kind option
    | Plus 
    | Minus 
    | Star 
    | Slash 
    | Percent
    | EqEq 
    | BangEq 
    | Langle
    | LangleEq
    | Langle2
    | Rangle
    | RangleEq
    | Rangle2
    | And
    | Or
    | Caret
    | AndAnd
    | OrOr
    | Bang

    | Lit of { kind: lit_kind; symb: string }
    | Ident of string

    | Eof

and lit_kind = 
    | Lit_int
    | Lit_float

let rec string_of_token_kind = function
    | Lpar          -> "("
    | Rpar          -> ")"
    | Comma         -> ","

    | Plus          -> "+"
    | Minus         -> "-"
    | Star          -> "*"
    | Slash         -> "/"
    | Percent       -> "%"
    | EqEq          -> "=="
    | BangEq        -> "!="
    | Langle        -> "<"
    | Rangle        -> ">"
    | LangleEq      -> "<="
    | RangleEq      -> ">="
    | Langle2       -> "<<"
    | Rangle2       -> ">>"
    | And           -> "&"
    | Or            -> "|"
    | Caret         -> "^"
    | AndAnd        -> "&&"
    | OrOr          -> "||"
    | Bang          -> "!"
    | Assign None   -> "="
    | Assign Some p -> string_of_token_kind p ^ "="
    | Lit lit       -> lit.symb
    | Ident id      -> id

    | Eof           -> "<eof>"

type token = { kind: token_kind; span: Loc.span }

let string_of_token (tk: token): string = 
    string_of_token_kind tk.kind

