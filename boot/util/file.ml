type path = string

type input = private Input
type output = private Output

type _ channel = 
    | Input: in_channel -> input channel
    | Output: out_channel -> output channel

type _ mode = 
    | Input: input mode
    | Output: output mode

type 'a handle = {
    file_path: path;
    file_dir:  path;
    file_name: string;
    file_ext:  string;
    file_channel: 'a channel;
}

let can_open (type a) (mode: a mode) (path: path): bool = 
    match mode with
    | Input -> Sys.file_exists path && not (Sys.is_directory path)
    | Output -> not (Sys.file_exists path && Sys.is_directory path)

let split_path (path: path): path * string * string = 
    let dir = Filename.dirname path 
    and name = Filename.basename path in
    let ext = Filename.extension name in
    dir, name, String.drop_first 1 ext

let load (type a) (mode: a mode) (path: path): a handle = 
    let dir, name, ext = split_path path in
    let chan: a channel = match mode with
    | Input -> Input (open_in path)
    | Output -> Output (open_out path)
    in { file_path = path;
         file_dir  = dir;
         file_name = name;
         file_ext  = ext;
         file_channel = chan; }

let close (type a) (file: a handle): unit = 
    match file.file_channel with
    | Input chan -> close_in chan
    | Output chan -> close_out chan

let load_safe (type a) (mode: a mode) (path: path) (fn: a handle -> 'b): 'b = 
    let file = load mode path in
    match fn file with
    | value -> close file; value
    | exception e -> close file; raise e

