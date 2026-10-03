type ('a, 'b) t = ('a, 'b) Hashtbl.t

let alloc (size: int): ('a, 'b) t = Hashtbl.create size

let put (tbl: ('a, 'b) t) (key: 'a) (value: 'b): unit = 
    assert (not (Hashtbl.mem tbl key));
    Hashtbl.add tbl key value

let search (tbl: ('a, 'b) t) (key: 'a): 'b option = 
    Hashtbl.find_opt tbl key

let find ~(default:'b) (tbl: ('a, 'b) t) (key: 'a): 'b = 
    match Hashtbl.find_opt tbl key with
    | Some value -> value
    | None -> default

let ensure ~(default:unit -> 'b) (tbl: ('a, 'b) t) (key: 'a): 'b = 
    match Hashtbl.find_opt tbl key with
    | Some value -> value
    | None -> 
        let value = default () in
        Hashtbl.add tbl key value;
        value

let update ~(default:'b) (tbl: ('a, 'b) t) (key: 'a) (fn: 'b -> 'b): unit = 
    let value = match Hashtbl.find_opt tbl key with
        | Some value -> value
        | None -> default
    in Hashtbl.replace tbl key value

