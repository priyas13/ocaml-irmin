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

  let l = M.create 4 5 in 
  print_int l.value


