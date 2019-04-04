module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Queue *)
let _ =
  U.print_header "2 merge set1";
  let module Atom = struct
  type t = int64
  let t = Irmin.Type.int64
  let compare x y = Int64.to_int @@ Int64.sub x y
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end  in 

  let module H = Set_imp.Make(Atom) in

  let q1 = H.empty  in 
    let original = H.empty |> H.add (Int64.of_int 1) |> H.add (Int64.of_int 2) in
  let q2 =  original |> H.remove (Int64.of_int 1) in  
  let m = H.merge3 original q1 q2 in 
  H.print_set H.print_int64 m;
