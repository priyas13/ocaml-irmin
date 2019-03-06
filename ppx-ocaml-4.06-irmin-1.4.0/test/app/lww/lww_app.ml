open Lwt.Infix
open Irmin_unix
open Core

module type ATOM =
sig
  type t
  val t: t Irmin.Type.t
  val compare: t -> t -> int
  val to_string : t -> string   
  val of_string: string -> t
end

module Make (Atom:ATOM) = struct

type atom = Atom.t

type t = {time: int; value: atom} 

let create v curr_time = {time=curr_time; value =v}

let read {time=t;value=v} = v

let update {time=t;value=v} v' curr_time = {time=curr_time; value=v'}

let merge3 old v1 v2 = let t1 = Pervasives.compare old.time v1.time in 
                      let t2 = Pervasives.compare old.time v2.time in 
                      let t3 = Pervasives.compare v1.time v2.time in 
if (t1 > 0 && t2 > 0) then old
     else if (t1 > 0 && t2 < 0) then v2 
     else if (t1 < 0 && t2 > 0) then v1
     else if (t1 < 0 && t2 < 0 && t3 < 0) then v2 
     else if (t1 < 0 && t2 < 0 && t3 > 0) then v1
       else let c1 = Pervasives.compare old.value v1.value in 
            let c2 = Pervasives.compare old.value v2.value in 
            let c3 = Pervasives.compare v1.value v2.value in 
              if (c1 > 0 && c2 > 0) then old
              else if (c1 > 0 && c2 < 0) then v2 
              else if (c1 < 0 && c2 > 0) then v1
              else if (c1 < 0 && c2 < 0 && c3 < 0) then v2 
              else if (c1 < 0 && c2 < 0 && c3 > 0) then v1
              else old 


end 
