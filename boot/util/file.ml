type path = string
type name = string

type input = private Input
type output = private Output

type _ channel = 
    | Input: in_channel -> input channel
    | Output: out_channel -> output channel

type meta = {
    dir: path;
    name: name;
    ext: string;
}

let describe (path: path): meta = 
    let dir = Filename.dirname path
    and name = Filename.basename path in
    let ext = String.drop_first 1 (Filename.extension name) in
    { dir; name; ext }

type 'a handle = {
    path: path;
    meta: meta;
    handle: 'a channel;
}

type _ mode = 
    | Input: input mode
    | Output: output mode

type file_type = Txt | Bin

let exists (path: path): bool = 
    Sys.file_exists path && not (Sys.is_directory path)

let is_relative (meta: meta): bool = 
    Filename.is_implicit meta.dir

let load (type a) (mode: a mode) (fty: file_type) (path: path): a handle = 
    let meta = describe path in
    let handle: a channel = match mode, fty with
    | Input, Txt  -> Input (open_in path)
    | Input, Bin  -> Input (open_in_bin path)
    | Output, Txt -> Output (open_out path)
    | Output, Bin -> Output (open_out_bin path)
    in { path; meta; handle }

let close (type a) (file: a handle): unit = 
    match file.handle with
    | Input ch -> close_in_noerr ch
    | Output ch -> close_out_noerr ch


