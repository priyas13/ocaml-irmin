(* Graph *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Graph *)
let _ =
  U.print_header "Graph";
let _ = 
  U.print_header "Testing delete node" in 

  let module Graph = Graph_imp.Make in

  let original =   Graph.G((Graph.OS.Empty, Int64.of_int 1, "a", Graph.OS.Empty), 
                           Graph.G(((Graph.OS.add ("ab", Int64.of_int 1) Graph.OS.Empty), Int64.of_int 2, "b", Graph.OS.Empty), 
                           (Graph.G(((Graph.OS.add ("ac", Int64.of_int 1) Graph.OS.Empty), Int64.of_int 3, "c", Graph.OS.Empty), E_G))))  in 
  let g = original |> Graph.delete_node (Int64.of_int 2) in 
  let g' = g |> Graph.insert_node 
                       (Int64.of_int 4) 
                       "d" 
                       (Graph.OS.add ("ad", Int64.of_int 1) Graph.OS.Empty)
                       (Graph.OS.Empty) in 
  let g2 = g' |> Graph.delete_node (Int64.of_int 1) |> Graph.delete_node (Int64.of_int 3) in 
  Graph.print_graph g2

