(* Graph *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Graph *)
let _ =
  U.print_header "Graph";
let _ = 
  U.print_header "Merge13" in 

  let module Graph = Graph_imp.Make in

  let original = Graph.G((Graph.OS.Empty, Int64.of_int 1, "a", Graph.OS.Empty), 
                           Graph.G(((Graph.OS.add ("ab", Int64.of_int 1) Graph.OS.Empty), Int64.of_int 2, "b", Graph.OS.Empty), 
                           (Graph.G(((Graph.OS.add ("bc", Int64.of_int 2) Graph.OS.Empty), Int64.of_int 3, "c", 
                              (Graph.OS.add ("ca", Int64.of_int 1) Graph.OS.Empty)), E_G)))) in 
  let g1 = original |> Graph.delete_node (Int64.of_int 3) |> Graph.delete_node (Int64.of_int 2) in  
  let g2 = original |> Graph.delete_node (Int64.of_int 3) |> Graph.delete_node (Int64.of_int 2) 
                    |> Graph.insert_node (Int64.of_int 4) "d" 
                          (Graph.OS.Empty) 
                          (Graph.OS.add ("da", Int64.of_int 1) Graph.OS.Empty) in  
  let mg = Graph.merge3 original g1 g2 in  
  Graph.print_graph mg
