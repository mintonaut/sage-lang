let sess: Session.sess = {
    sess_file_in = None;
}

let error fmt = Printf.ksprintf begin fun str -> 
    Printf.fprintf stderr "Error: %s\n%!" str;
    exit 1
end fmt

let print_version _ = 
    Printf.fprintf stdout "sageboot version %s\n" Version.version;
    exit 0

let argset: (string * Arg.spec * string) list = 
    let flag name ~desc fn = name, Arg.Unit fn, desc in [
        flag "-version" ~desc:"Print version and exit" print_version;
    ]

;;

Arg.parse argset
    (fun input -> sess.sess_file_in <- Some input)
    "usage: sageboot [OPTIONS] SOURCE_FILE.sg"

;;

if Option.is_none sess.sess_file_in
then error "no input file specified"

;;

let _ = 
    let [@warning "-8"] Some input = sess.sess_file_in in
    match File.load Input Txt input with
    | exception _ -> error "could not open file %s" input
    | input -> match input.meta.ext with
    | "sg" -> File.close input
    | _ -> File.close input; error "unrecognized file extension: %s" input.meta.name

;;

print_endline "Accepted!"

;;
