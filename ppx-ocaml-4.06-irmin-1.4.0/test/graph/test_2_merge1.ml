(* Graph *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Graph *)
let _ =
  U.print_header "Graph";
let _ = 
  U.print_header "Testing two-way merge" in 

  let module G = Graph_imp.Make in

  let original = G.G(([], Int64.of_int 3, "c", []), E_G) in 
  let g = original |> G.insert_node (Int64.of_int 2) "b" [] [("bc",(Int64.of_int 3))] in 
  let g' = g |> G.insert_node (Int64.of_int 1) "a" [("ba", (Int64.of_int 2))] [("ab", (Int64.of_int 2))]
             |> G.insert_edge (Int64.of_int 1) (Int64.of_int 3) "ac" in
  let m = G.merge2 g g' in 
  G.print_graph m

