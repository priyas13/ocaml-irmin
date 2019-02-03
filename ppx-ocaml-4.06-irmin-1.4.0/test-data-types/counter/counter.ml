module Make = struct 
type t = int [@@derive versioned]

let inc t x = t + x
let dec t x = t - x

let merge ~ancestor v1 v2 = ancestor + (v1-ancestor) + (v2-ancestor)
end [@@derive_versioned]
