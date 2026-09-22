type expr = 
    | EXPR_lit of lit
    | EXPR_par of expr

and lit = 
    | LIT_int of Int64.t
