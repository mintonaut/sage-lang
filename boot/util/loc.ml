type position = { line: int; column: int }
type location = { file: File.path; line: int; column: int }
type span     = { file: File.path; lo: position; hi: position }
type 'a located = { node: 'a; span: span }

let position (line: int) (column: int): position = { line; column }

let location (file: File.path) (line: int) (column: int): location = { file; line; column }

let of_position (file: File.path) (pos: position): location = 
    location file pos.line pos.column

let span (file: File.path) (lo: position) (hi: position): span = { file; lo; hi }

let locate (span: span) (node: 'a): 'a located = { span; node }

let respan (span: span) (loc: 'a located): 'a located = { loc with span }

let lexpos (pos: Lexing.position): position = 
    position pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let lexloc (pos: Lexing.position): location = 
    location pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let lexspan (lexbuf: Lexing.lexbuf): span = 
    let file = lexbuf.lex_start_p.pos_fname
    and lo = lexpos lexbuf.lex_start_p
    and hi = lexpos lexbuf.lex_curr_p in
    { file; lo; hi }

let combine (a: span) (b: span): span = 
    if a.file == b.file then
        let lo = 
            let p1 = a.lo and p2 = b.lo in
            if p1.line < p2.line then p1
            else if p2.line < p1.line then p2
            else if p1.column < p2.column then p1 else p2
        and hi = 
            let p1 = a.hi and p2 = b.hi in
            if p1.line < p2.line then p2
            else if p2.line < p1.line then p1
            else if p1.column < p2.column then p2 else p1
        in span a.file lo hi
    else a

let string_of_position (pos: position): string = 
    Printf.sprintf "%d:%d" pos.line pos.column

let string_of_location (loc: location): string = 
    Printf.sprintf "%s:%d:%d" loc.file loc.line loc.column

let string_of_span (span: span): string = 
    Printf.sprintf "%s from %s to %s" span.file
        (string_of_position span.lo)
        (string_of_position span.hi)

