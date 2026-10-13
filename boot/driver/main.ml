let sess: Session.sess = {
    sess_file_in = None;
    sess_failed  = false;
}

let checkpoint _ = if sess.sess_failed then exit 1

let error fmt = Printf.ksprintf begin fun str -> 
    Printf.eprintf "Error: %s\n%!" str;
    exit 1
end fmt

let argset: Arg.(key * spec * doc) list = 
    let print_version _ = 
        Printf.printf "sageboot version %s\n" Version.version_full;
        exit 0
    in
    let flag key ~desc fn = key, Arg.Unit fn, desc in [
        flag "-version" ~desc:"Print sageboot version and exit" print_version;
        flag "--v"      ~desc:"Print sageboot version and exit" print_version;
    ]

;;

Arg.parse argset
    (fun input -> sess.sess_file_in <- Some input)
   "usage: sageboot [OPTIONS] SOURCE_FILE"

;;

if Option.is_none sess.sess_file_in
then error "no input file specified"

;;

let _ = 
    let input = Option.get sess.sess_file_in in
    let file = File.describe input in
    match File.exists input, file.ext with
    | true, "sg" -> ()
    | true, _    -> error "%s: unrecognized file type" file.file
    | false, _   -> if File.is_relative file 
        then error "file %s does not exist within the %s directory" file.file 
            (if file.dir = Filename.parent_dir_name then "parent" else "current")
        else error "file %s does not exist inside %s" file.file file.dir

;;

print_endline "Accepted"
