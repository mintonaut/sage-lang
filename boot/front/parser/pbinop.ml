open! Util
open! Parser

type precedence = 
    | Assign (* = += -= *= /= %= &= |= ^= <<= >>= *)
    | Or (* || *)
    | And (* && *)
    | Compare (* == != < <= > >= *)
    | BitOr (* | *)
    | BitXor (* ^ *)
    | BitAnd (* & *)
    | Shift (* >> << *)
    | Sum (* + - *)
    | Product (* * / % *)

type binop = 
    | Binary of Ast.binop
    | Assign of Ast.assignop option

type fixity = 
    | NoFixity
    | Left
    | Right

let precedence (binop: binop): precedence = 
    match binop with
    | Binary (Mul | Div | Mod) -> Product
    | Binary (Add | Sub) -> Sum
    | Binary (Shl | Shr) -> Shift
    | Binary BitAnd -> BitAnd
    | Binary BitXor -> BitXor
    | Binary BitOr  -> BitOr
    | Binary (Eqs | Neq | Lst | Leq | Grt | Geq) -> Compare
    | Binary And -> And
    | Binary Or  -> Or
    | Assign _ -> Assign

let fixity (binop: binop): fixity = 
    match binop with
    | Assign _ -> Right
    | Binary (Eqs | Neq | Lst | Leq | Grt | Geq) -> NoFixity
    | Binary (Add | Sub | Mul | Div | Mod | And | Or | BitOr | BitXor | BitAnd | Shl | Shr ) -> Left

let parse (ps: pstate): binop option = 
    match ps.peek.kind with
    | Plus -> Some (Binary Add)
    | Minus -> Some (Binary Sub)
    | Star -> Some (Binary Mul)
    | Slash -> Some (Binary Div)
    | Percent -> Some (Binary Mod)
    | EqEq -> Some (Binary Eqs)
    | NotEq -> Some (Binary Neq)
    | Langle -> Some (Binary Lst)
    | LangleEq -> Some (Binary Leq)
    | Rangle -> Some (Binary Grt)
    | RangleEq -> Some (Binary Geq)
    | And -> Some (Binary BitAnd)
    | Or -> Some (Binary BitOr)
    | Caret -> Some (Binary BitXor)
    | AndAnd -> Some (Binary And)
    | OrOr -> Some (Binary Or)
    | Langle2 -> Some (Binary Shl)
    | Rangle2 -> Some (Binary Shr)
    | Eq None -> Some (Assign None)
    | Eq Some Plus -> Some (Assign (Some Add))
    | Eq Some Minus -> Some (Assign (Some Sub))
    | Eq Some Star -> Some (Assign (Some Mul))
    | Eq Some Slash -> Some (Assign (Some Div))
    | Eq Some Percent -> Some (Assign (Some Mod))
    | Eq Some And -> Some (Assign (Some BitAnd))
    | Eq Some Or -> Some (Assign (Some BitOr))
    | Eq Some Caret -> Some (Assign (Some BitXor))
    | Eq Some Langle2 -> Some (Assign (Some Shl))
    | Eq Some Rangle2 -> Some (Assign (Some Shr))
    | _ -> None

let accept_binop (ps: pstate) (min_prec: precedence bound): binop option = 
    match parse ps with
    | None -> None
    | Some op -> 
        let prec = precedence op in
        match min_prec with
        | Unbound -> Some op
        | Include min_prec when prec < min_prec -> None
        | Exclude min_prec when prec <= min_prec -> None
        | Include _ | Exclude _ -> Some op

