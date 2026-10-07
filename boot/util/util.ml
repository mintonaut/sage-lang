type 'a bound = 
    | Unbound
    | Include of 'a
    | Exclude of 'a

