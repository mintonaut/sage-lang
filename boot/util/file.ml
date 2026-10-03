type path = string

type input = private Input
type output = private Output

type _ channel = 
    | Input: in_channel -> input channel
    | Output: out_channel -> output channel

type meta = {
    dir: path;
    name: string;
    ext: string;
}

type 'a handle = {
    path: path;
    file: meta;
    channel: 'a channel;
}

type format = Txt | Bin

type _ mode = 
    | Input: format -> input mode
    | Output: format -> output mode

let describe (path: path): meta = 
    let dir = Filename.dirname path
    and name = Filename.basename path in
    let ext = String.drop_first 1 (Filename.extension name) in
    { dir; name; ext }

let is_relative (file: meta): bool = 
    Filename.is_implicit file.dir

let exists (path: path): bool = 
    Sys.file_exists path && not (Sys.is_directory path)

let load (type a) (mode: a mode) (path: path): a handle = 
    let file = describe path in
    let channel: a channel = match mode with
    | Input fmt -> Input (match fmt with Txt -> open_in path | Bin -> open_in_bin path)
    | Output fmt -> Output (match fmt with Txt -> open_out path | Bin -> open_out_bin path)
    in { file; path; channel }

let close (type a) (handle: a handle): unit = 
    match handle.channel with
    | Input ch -> close_in_noerr ch
    | Output ch -> close_out_noerr ch

let use (type a) (mode: a mode) (path: path) (fn: a handle -> 'b): 'b = 
    let file = load mode path in
    match fn file with
    | value -> close file; value
    | exception exn -> close file; raise exn

let use_safe (type a) (mode: a mode) (path: path) (fn: a handle -> 'b): 'b option = 
    match load mode path with
    | file -> begin match fn file with
        | value -> close file; Some value
        | exception exn -> close file; raise exn
    end
    | exception _ -> None

