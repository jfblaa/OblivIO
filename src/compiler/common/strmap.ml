module ST = Map.Make(String)
    
type 'a strmap = 'a ST.t

let empty = ST.empty
let enter (t, k, v) = ST.add k v t
let look (t, k ) = ST.find_opt k t
let numItems = ST.cardinal
