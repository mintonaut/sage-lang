type ('a, 'b) t = ('a, 'b) Hashtbl.t

let alloc (size: int): ('a, 'b) t = Hashtbl.create size

let put (tbl: ('a, 'b) t) (key: 'a) (value: 'b): unit = 
    assert (not (Hashtbl.mem tbl key));
    Hashtbl.add tbl key value

let search (tbl: ('a, 'b) t) (key: 'a): 'b option = 
    Hashtbl.find_opt tbl key

