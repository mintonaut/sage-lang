type position = { line: int; column: int }
type location = { file: File.path; line: int; column: int }
type span = { file: File.path; pstart: position; pend: position }

let position (line: int) (column: int): position = 
    { line; column }

let location (file: File.path) (line: int) (column: int): location = 
    { file; line; column }

let span (file: File.path) (pstart: position) (pend: position): span = 
    { file; pstart; pend }

let lexpos (pos: Lexing.position): position = 
    position pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let lexloc (pos: Lexing.position): location = 
    location pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let combine (a: span) (b: span): span = 
    if a.file == b.file then
        let pstart = 
            let p1 = a.pstart and p2 = b.pstart in
            if p1.column < p2.column then p1
            else if p2.column < p1.column then p2
            else if p1.line < p2.line then p1 else p2
        and pend = 
            let p1 = a.pend and p2 = b.pend in
            if p1.column > p2.column then p1
            else if p2.column > p1.column then p2
            else if p1.line > p2.line then p1 else p2
        in span a.file pstart pend
    else a

type 'a located = { node: 'a; span: span }

let locate (node: 'a) (span: span): 'a located = 
    { node; span }

let respan (loc: 'a located) (span: span): 'a located = 
    { loc with span }

let string_of_position (pos: position): string = 
    Printf.sprintf "%d:%d" pos.line pos.column

let string_of_location (loc: location): string = 
    Printf.sprintf "%s:%d:%d" loc.file loc.line loc.column

let string_of_span (span: span): string = 
    Printf.sprintf "%s:[%s::%s]" span.file
        (string_of_position span.pstart)
        (string_of_position span.pend)

