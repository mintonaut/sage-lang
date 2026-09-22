type ('a, 'b) table = ('a, 'b) Hashtbl.t

let alloc (size: int): ('a, 'b) table = Hashtbl.create size

let put (tbl: ('a, 'b) table) (key: 'a) (value: 'b): unit = 
    assert (not (Hashtbl.mem tbl key));
    Hashtbl.add tbl key value

let search (tbl: ('a, 'b) table) (key: 'a): 'b option = 
    Hashtbl.find_opt tbl key

let find ~(default: unit -> 'b) (tbl: ('a, 'b) table) (key: 'a): 'b = 
    match Hashtbl.find_opt tbl key with
    | Some value -> value
    | None -> default ()

let ensure ~(default: unit -> 'b) (tbl: ('a, 'b) table) (key: 'a): 'b = 
    match Hashtbl.find_opt tbl key with
    | Some value -> value
    | None -> 
        let value = default () in
        Hashtbl.add tbl key value;
        value

