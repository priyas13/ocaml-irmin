module Make = struct 
type t = int64

let inc t x = Int64.to_int t + Int64.to_int x
let dec t x = Int64.to_int t - Int64.to_int x

let merge ~ancestor v1 v2 = Int64.to_int ancestor + (Int64.to_int v1- Int64.to_int ancestor) + (Int64.to_int v2- Int64.to_int ancestor)
end 
