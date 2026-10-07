type sess = {
    mutable sess_file_in: File.path option;
    mutable sess_failed: bool;
    mutable sess_log_out: out_channel;
    mutable sess_log_err: out_channel;
    mutable sess_log: sess_log;
}

and sess_log = {
    mutable log_parser: bool;
}

let log ~(name:string) ~(flag:bool) channel = Printf.ksprintf begin
    if flag 
    then Printf.fprintf channel "[%s] %s\n%!" name
    else Fun.const ()
end

let error (sess: sess) = Printf.ksprintf begin fun str -> 
    sess.sess_failed <- true;
    Printf.fprintf sess.sess_log_err "[\027[0;31mERROR\027[0m] %s\n%!" str
end
