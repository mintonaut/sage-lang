type sess = {
    mutable sess_file_in:   string option;
    mutable sess_failed:    bool;
    mutable sess_log_out:   sess_log_out;
    mutable sess_log_flags: sess_log_flags;
}

and sess_log_out = {
    mutable parser: out_channel;
}

and sess_log_flags = {
    mutable parser: bool;
}

let log ~(name:string) ?(flag=true) channel = Printf.ksprintf begin
    if flag 
    then Printf.fprintf channel "[%s] %s\n%!" name 
    else Fun.const ()
end

let error sess = Printf.ksprintf begin fun fmt -> 
    sess.sess_failed <- true;
    Printf.eprintf "[\027[0;31mERROR\027[0m] %s\n%!" fmt
end

