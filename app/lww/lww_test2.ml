open Core

module U = struct
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

let _ =
  U.print_header "LWW1";
  let module IntAtom = struct
    type t = int
    let compare = Pervasives.compare
    let to_string = string_of_int
  end in

  let module M = Lww_app.Make(IntAtom) in

  let original = M.{time = 2; value = 0}  in 
  let q1 =  M.{time = 3; value = 1} in 
  let q2 = M.{time = 4; value = 2} in 
  let m = M.merge3 original q1 q2 in 
  print_int m.value


