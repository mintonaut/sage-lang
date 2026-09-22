type sess = {
    mutable sess_file_in: string option;
    mutable sess_log_out: out_channel;
    mutable sess_log_err: out_channel;
    mutable sess_failed: bool;
    sess_log: sess_log;
}

and sess_log = {
    mutable log_parser: bool;
}

let log ?(flag = true) ~(name: string) chan = 
    Printf.ksprintf @@ if flag 
        then Printf.fprintf chan "[%s] %s\n%!" name 
        else Fun.const ()

let error (sess: sess) = 
    sess.sess_failed <- true;
    Printf.ksprintf (Printf.fprintf sess.sess_log_err 
        "[\027[0;31mERROR\027[0m] %s\n%!")

