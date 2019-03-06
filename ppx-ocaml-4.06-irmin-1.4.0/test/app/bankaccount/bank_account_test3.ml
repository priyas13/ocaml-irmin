
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
  let to_string = string_of_int
end

(* Queue *)
let _ =
  U.print_header "BANKAPP";

  let module M = Bank_app.Make in

  let original = M.{acct_id = 1; value = 20} in 
  let q1 =  M.withdraw original 10  in 
  let q2 = M.withdraw original 5 in 
  (* Edit seq generation demonstration *)
  let edit_seq_printer = U.string_of_list (M.edit_to_string U.to_string) in 
  (* edit seq generation with diff *)
  let p = M.op_diff original q1 in
  let q = M.op_diff original q2 in
  let _ = Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q) in 
  let p', q' = M.op_transform original  p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q') in 
    let m = M.merge3 ~ancestor:original q1 q2 in 
    print_int (M.get_balance m) 



