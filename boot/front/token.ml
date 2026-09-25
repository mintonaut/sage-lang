type token = 
    (* Separator and miscellaneous tokens *)
    | Lpar | Rpar
    | Lbrace | Rbrace
    | Comma | Semi

    (* Operators *)
    | Plus | Minus | Star | Slash | Percent
    | Langle | LangleEq | Rangle | RangleEq | EqEq | NotEq
    | Langle2 | Rangle2 | And | Or | Caret
    | AndAnd | OrOr | Bang

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

    | Lit_int i -> Int64.to_string i

    | Eof       -> "<eof>"
