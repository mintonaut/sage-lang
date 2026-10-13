type path = string
type name = string

type input = private Input
type output = private Output

type _ channel = 
    | Input: in_channel -> input channel
    | Output: out_channel -> output channel

type description = {
    dir:  path;
    file: name;
    ext:  string;
}

let describe (path: path): description = 
    let dir = Filename.dirname path
    and file = Filename.basename path in
    let ext = String.drop_first 1 (Filename.extension file) in
    { dir; file; ext }

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

