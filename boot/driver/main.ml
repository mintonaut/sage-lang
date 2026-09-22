open! Pexp
open! Table

let sess: Session.sess = {
    sess_file_in = None;
    sess_log_out = stdout;
    sess_log_err = stderr;
    sess_failed  = false;
    sess_log = {
        log_parser = false;
    };
}

let checkpoint (sess: Session.sess) = 
    if sess.sess_failed then exit 1

let flag ~doc flg fn = (flg, Arg.Unit fn, doc)

let print_version _ = 
    Printf.fprintf stdout "sageboot version 0.1a"

let args: (string * Arg.spec * string) list = [
    flag "-version" print_version ~doc:"print version and exit";
    flag "-lparse" (fun _ -> sess.Session.sess_log.log_parser <- true)
        ~doc:"enable debugging information for parsing"
]

;;

let _ = Arg.parse args
    (fun input -> sess.Session.sess_file_in <- Some input)
    (Printf.sprintf "usage: %s [options] SOURCE_FILE.sg" (Filename.basename Sys.argv.(0)))

;;

if Option.is_none sess.Session.sess_file_in 
then (Session.error sess "no input file specified"; exit 1)

;;

Option.iter (fun path -> if not (File.can_open Input path) 
    then (Session.error sess "%s: cannot open file" path; exit 1)
) sess.Session.sess_file_in

;;

let _ = 
    let [@warning "-8"] Some filepath = sess.Session.sess_file_in in
    File.load_safe Input filepath @@ fun file -> 
        match file.file_ext with
        | "sg" -> Parser.with_handle sess @@ fun _ ->
            let pstate = Parser.make_state sess file in
            ignore (Pexp.parse_expr pstate)
        | ext -> Session.error sess "unrecognized input file type: %s" ext

;;

checkpoint sess

;;

print_endline "Accepted!"

;;
