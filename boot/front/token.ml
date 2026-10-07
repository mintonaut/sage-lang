type token_kind = 
    | Lpar | Rpar
    | Lbrace | Rbrace
    | Semi | Comma

    | Eq of token_kind option
    | Plus | Minus | Star | Slash | Percent
    | EqEq | NotEq | Langle | Rangle | LangleEq | RangleEq
    | AndAnd | OrOr
    | And | Or | Caret
    | Langle2 | Rangle2
    | Bang

    | Let
    | While
    | If | Else

    | Ident of string
    | Lit of {
        symbol: string;
        kind: lit_kind
    }

    | Eof

and lit_kind = 
    | Int
    | Float

type token = { kind: token_kind; span: Loc.span }

let rec string_of_token_kind (tk: token_kind) = 
    match tk with
    | Lpar      -> "("
    | Rpar      -> ")"
    | Lbrace    -> "{"
    | Rbrace    -> "}"
    | Semi      -> ";"
    | Comma     -> ","
    
    | Eq None   -> "="
    | Eq Some o -> string_of_token_kind o ^ "="
    | Plus      -> "+"
    | Minus     -> "-"
    | Star      -> "*"
    | Slash     -> "/"
    | Percent   -> "%"
    | EqEq      -> "=="
    | NotEq     -> "!="
    | Langle    -> "<"
    | LangleEq  -> "<="
    | Rangle    -> ">"
    | RangleEq  -> ">="
    | And       -> "&"
    | Or        -> "|"
    | Caret     -> "^"
    | AndAnd    -> "&&"
    | OrOr      -> "||"
    | Langle2   -> "<<"
    | Rangle2   -> ">>"
    | Bang      -> "!"

    | Let       -> "let"
    | While     -> "while"
    | If        -> "if"
    | Else      -> "else"

    | Ident id  -> id
    | Lit x     -> x.symbol

    | Eof       -> "<eof>"

let string_of_token (tk: token) = 
    string_of_token_kind tk.kind

