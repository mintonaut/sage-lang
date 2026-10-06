let print_version _ = 
    Printf.fprintf stdout "sageboot version %s\n" Version.version;
    exit 0

let argset: (string * Arg.spec * string) list = 
    let flag name ~desc fn = name, Arg.Unit fn, desc in [
        flag "-version" ~desc:"Print version and exit" print_version;
    ]

;;

Arg.parse argset
    (fun input -> print_endline input)
    "usage: sageboot [OPTIONS]"

;;


