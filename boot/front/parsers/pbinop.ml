open! Util
open! Parser

type precedence = 
    | Assign (* = += -= *= /= %= &= |= ^= <<= >>= *)
    | Or (* || *)
    | And (* && *)
    | Compare (* == != < > <= >= *)
    | BitOr (* | *)
    | BitXor (* ^ *)
    | BitAnd (* & *)
    | Shift (* << >> *)
    | Sum (* + - *)
    | Product (* * / % *)

type binop = 
    | Binary of Ast.binop
    | Assign of Ast.asgop option

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
    | Binary BitOr -> BitOr
    | Binary (Eqs | Neq | Lst | Leq | Grt | Geq) -> Compare
    | Binary And -> And
    | Binary Or -> Or
    | Assign _ -> Assign

let fixity (binop: binop): fixity = 
    match binop with
    | Assign _ -> Right
    | Binary (Eqs | Neq | Lst | Leq | Grt | Geq) -> NoFixity
    | Binary (Add | Sub | Mul | Div | Mod | And | Or | BitOr | BitXor | BitAnd | Shl | Shr) -> Left

let parse_binop (ps: pstate): binop option = 
    match ps.peek.kind with
    | Plus      -> Some (Binary Add)
    | Minus     -> Some (Binary Sub)
    | Star      -> Some (Binary Mul)
    | Slash     -> Some (Binary Div)
    | Percent   -> Some (Binary Mod)
    | EqEq      -> Some (Binary Eqs)
    | BangEq    -> Some (Binary Neq)
    | Langle    -> Some (Binary Lst)
    | Rangle    -> Some (Binary Grt)
    | LangleEq  -> Some (Binary Leq)
    | RangleEq  -> Some (Binary Geq)
    | AndAnd    -> Some (Binary And)
    | OrOr      -> Some (Binary Or)
    | And       -> Some (Binary BitAnd)
    | Caret     -> Some (Binary BitXor)
    | Or        -> Some (Binary BitOr)
    | Langle2   -> Some (Binary Shl)
    | Rangle2   -> Some (Binary Shr)

    | Eq            -> Some (Assign None)
    | EqOp Plus     -> Some (Assign (Some Add))
    | EqOp Minus    -> Some (Assign (Some Sub))
    | EqOp Star     -> Some (Assign (Some Mul))
    | EqOp Slash    -> Some (Assign (Some Div))
    | EqOp Percent  -> Some (Assign (Some Mod))
    | EqOp Langle2  -> Some (Assign (Some Shl))
    | EqOp Rangle2  -> Some (Assign (Some Shr))
    | EqOp And      -> Some (Assign (Some BitAnd))
    | EqOp Caret    -> Some (Assign (Some BitXor))
    | EqOp Or       -> Some (Assign (Some BitOr))

    | _ -> None

let check_binop (min_prec: precedence bound) (ps: pstate): binop option = 
    let open Option.Syntax in
    let* op = parse_binop ps in
    let prec = precedence op in
    match min_prec with
    | Unbound -> Some op
    | Include min_prec when prec < min_prec -> None
    | Exclude min_prec when prec <= min_prec -> None
    | Include _ | Exclude _ -> Some op

