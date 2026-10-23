type path = string
type name = string

type input = private Input
type output = private Output

type _ channel = 
    | Input: in_channel -> input channel
    | Output: out_channel -> output channel

type description = {
    dir:  path;
    name: name;
    ext:  string;
}

let describe (path: path): description = 
    let dir = Filename.dirname path
    and name = Filename.basename path in
    let ext = String.drop_first 1 (Filename.extension name) in
    { dir; name; ext }

let name (file: description): name = Filename.chop_extension file.name

type 'a handle = {
    path: path;
    file: description;
    handle: 'a channel;
}

type _ mode = 
    | Input: input mode
    | Output: output mode

type file_type = Txt | Bin

let exists (path: path): bool = 
    Sys.file_exists path && not (Sys.is_directory path)

let is_relative str = not (String.includes ~affix:Filename.dir_sep str)

let is_relative (meta: description): bool = 
    Filename.is_implicit meta.dir && is_relative meta.dir

let load (type a) (mode: a mode) (ty: file_type) (path: path): a handle = 
    let file = describe path in
    let handle: a channel = match mode with
    | Input -> Input (match ty with Txt -> open_in path | Bin -> open_in_bin path)
    | Output -> Output (match ty with Txt -> open_out path | Bin -> open_out_bin path)
    in { path; file; handle }

let close (type a) (file: a handle): unit = 
    match file.handle with
    | Input ch -> close_in_noerr ch
    | Output ch -> close_out_noerr ch

