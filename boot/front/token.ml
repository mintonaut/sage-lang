type token_kind = 
    | Lpar | Rpar
    | Lbrace | Rbrace
    | Comma | Colon | Semi
    | Underscore
    | Pipe

    | Eq 
    | EqOp of token_kind
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

    | Const
    | Fn | Return
    | Let
    | While
    | If | Else
    | Loop | Break

    | Lit of { kind: lit_kind; symb: string }
    | Ident of string

    | Eof

and lit_kind = 
    | Lit_int
    | Lit_float

let rec string_of_token_kind = function
    | Lpar          -> "("
    | Rpar          -> ")"
    | Lbrace        -> "{"
    | Rbrace        -> "}"
    | Comma         -> ","
    | Semi          -> ";"
    | Colon         -> ":"
    | Underscore    -> "_"
    | Pipe          -> "|"

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
    | Eq            -> "="
    | EqOp p        -> string_of_token_kind p ^ "="

    | Const         -> "const"
    | Fn            -> "fn"
    | Return        -> "return"
    | Let           -> "let"
    | While         -> "while"
    | If            -> "if"
    | Else          -> "else"
    | Loop          -> "loop"
    | Break         -> "break"

    | Lit lit       -> lit.symb
    | Ident id      -> id

    | Eof           -> "<eof>"

type token = { kind: token_kind; span: Loc.span }

let string_of_token (tk: token): string = 
    string_of_token_kind tk.kind

