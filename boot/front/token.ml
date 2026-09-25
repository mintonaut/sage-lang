type token = 
    (* Separator and miscellaneous tokens *)
    | Lpar | Rpar
    | Lbrace | Rbrace
    | Comma | Semi 
    | Eq

    (* Operators *)
    | Plus | Minus | Star | Slash | Percent
    | Langle | LangleEq | Rangle | RangleEq | EqEq | NotEq
    | Langle2 | Rangle2 | And | Or | Caret
    | AndAnd | OrOr | Bang

    (* Reversed keywords + variables *)
    | Let
    | Ident of string

    (* Literal tokens *)
    | Lit_int of Int64.t

    | Eof

let string_of_token = function
    | Lpar      -> "("
    | Rpar      -> ")"
    | Lbrace    -> "{"
    | Rbrace    -> "}"
    | Semi      -> ";"
    | Comma     -> ","

    | Eq        -> "="
    | Plus      -> "+"
    | Minus     -> "-"
    | Star      -> "*"
    | Slash     -> "/"
    | Percent   -> "%"
    | Langle    -> "<"
    | LangleEq  -> "<="
    | Rangle    -> ">"
    | RangleEq  -> ">="
    | EqEq      -> "=="
    | NotEq     -> "!="
    | AndAnd    -> "&&"
    | OrOr      -> "||"
    | And       -> "&"
    | Or        -> "|"
    | Caret     -> "^"
    | Langle2   -> "<<"
    | Rangle2   -> ">>"
    | Bang      -> "!"

    | Let       -> "let"

    | Lit_int i -> Int64.to_string i

    | Ident str -> str

    | Eof       -> "<eof>"
