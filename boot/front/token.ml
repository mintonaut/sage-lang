type token = 
    (* Separator and miscellaneous tokens *)
    | Lpar | Rpar
    | Comma

    (* Literal tokens *)
    | Lit_int of Int64.t

    | Eof

let string_of_token = function
    | Lpar      -> "("
    | Rpar      -> ")"
    | Comma     -> ","

    | Lit_int i -> Int64.to_string i

    | Eof       -> "<eof>"
