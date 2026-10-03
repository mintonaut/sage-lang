let sess: Session.sess = {
    Session.sess_file_in = None;
    Session.sess_failed = false;
    Session.sess_log_out = stdout;
    Session.sess_log_err = stderr;
    Session.sess_log = {
        Session.log_lexer = false;
        Session.log_parser = false;
    };
    Session.sess_timings = Table.alloc 2;
}

let error fmt = Printf.ksprintf (Printf.fprintf sess.sess_log_err "Error: %s\n%!") fmt

let checkpoint _ = 
    if sess.sess_failed then exit 1

let args: (string * Arg.spec * string) list = 
    let flag ~desc name fn = name, Arg.Unit fn, desc in 
    let print_version _ = print_endline "sageboot version 0.1a"; exit 0 in [
        flag "-version" ~desc:"Print version and exit" print_version;
        flag "-llexer" ~desc:"Print lexer logs"  
            begin fun _ -> sess.sess_log.log_lexer <- true end;
        flag "-lparser" ~desc:"Print parser logs" @@
            begin fun _ -> sess.sess_log.log_parser <- true end;
    ]

;;

Arg.parse args 
    (fun input -> sess.sess_file_in <- Some input)
    "usage: sageboot [OPTIONS] SOURCE_FILE.sg"

;;

if Option.is_none sess.sess_file_in
then (error "no input file specified"; exit 1)

;;

let _ = 
    let [@warning "-8"] Some input = sess.sess_file_in in
    let file = File.describe input in
    if not (File.exists input) then begin
        let dir = if File.is_relative file
            then "the current directory"
            else file.dir
        in (error "%s is not a valid file within %s" file.name dir; exit 1)
    end;
    let res = File.use_safe (Input Txt) input @@ fun input -> 
        match input.file.ext with
        | "sg" -> ()
        | _ -> (sess.sess_failed <- true; error "unrecognized file extension: %s" input.file.name)
    in
    match res with
    | Some res -> res
    | None -> (error "failed to open file: %s" file.name; exit 1)

;;

checkpoint ()

;; 

print_endline "Accepted!"
