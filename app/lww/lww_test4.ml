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

  let original = M.{time = 2; value = 10}  in 
  let q1 =  M.create 4 3 ; M.update original 5 4 in 
  let q2 = M.update original 6 5 in 
  let m = M.merge3 original q1 q2 in 
  print_int m.value


