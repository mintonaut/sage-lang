open! Pexpr

let sess: Session.sess = {
    sess_file_in    = None;
    sess_failed     = false;
    sess_log_flags  = {
        parser = false;
    };
    (* TODO: make flags for these session fields - requires different Arg parser *)
    sess_log_out    = {
        parser = stdout;
    }
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
        flag "-lparser" ~desc:"Report all parser logs" begin fun _ -> 
            sess.sess_log_flags.parser <- true end;
    ]

;;

Arg.parse argset
    (fun input -> sess.sess_file_in <- Some input)
   "usage: sageboot [OPTIONS] SOURCE_FILE"

;;

if Option.is_none sess.sess_file_in
then error "no input file specified"

;;

let scroll: Ast.mod_scroll = 
    let input = Option.get sess.sess_file_in in
    let file = File.describe input in
    match File.exists input, file.ext with
    | true, "sg" -> 
        let file = match File.load Input Txt input with
        | file -> file
        | exception _ -> error "could not open file %S" file.name
        in
        let res = Pmod.parse_file sess file in
        File.close file;
        res
    | true, _    -> error "%s: unrecognized file type" file.name
    | false, _   -> if File.is_relative file 
        then error "file %s does not exist within the %s directory" file.name
            (if file.dir = Filename.parent_dir_name then "parent" else "current")
        else error "file %s does not exist inside %s" file.name file.dir

;;

checkpoint () 

;;

Printf.printf "Parsed scroll %s with %d spells\n" scroll.node.scroll_name 
    (Array.length scroll.node.scroll_spells)

