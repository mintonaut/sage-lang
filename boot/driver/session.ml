type sess = {
    mutable sess_file_in: File.path option;
    mutable sess_failed: bool;
    mutable sess_log_out: out_channel;
    mutable sess_log_err: out_channel;
    mutable sess_log: sess_log;

    sess_timings: (string, float) Table.t;
}

and sess_log = {
    mutable log_lexer: bool;
    mutable log_parser: bool;
}

let log ~(name: string) ~(flag: bool) (channel: out_channel) = 
    Printf.ksprintf (if flag then Printf.fprintf channel "%s %s\n%!" name else Fun.const ())

let error (sess: sess) = 
    sess.sess_failed <- true;
    Printf.fprintf sess.sess_log_err

let report_error (sess: sess) = 
    sess.sess_failed <- true;
    Printf.ksprintf (Printf.fprintf sess.sess_log_err "[\027[0;31mERROR\027[0m] %s\n%!")

let log_time (sess: sess) (phase: string) (time: float): unit = 
    Table.update ~default:0.0 sess.sess_timings phase 
        begin fun existing -> time +. existing end

let timed ~(phase: string) (sess: sess) (thunk: unit -> 'a): 'a = 
    let t1 = Unix.gettimeofday () in
    let res = thunk () in
    let t2 = Unix.gettimeofday () in
    log_time sess phase (t2 -. t1); res

