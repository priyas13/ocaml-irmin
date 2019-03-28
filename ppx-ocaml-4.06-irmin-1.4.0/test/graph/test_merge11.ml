(* Graph *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Graph *)
let _ =
  U.print_header "Graph";
let _ = 
  U.print_header "Merge11" in 

  let module Graph = Graph_imp.Make in

  let original = Graph.G(((Graph.OS.Empty), Int64.of_int 1, "a", 
                               (Graph.OS.add ("af", Int64.of_int 2) Graph.OS.Empty)), 
                          Graph.G(((Graph.OS.Empty), Int64.of_int 2, "f", Graph.OS.Empty), E_G)) in 
  let g1 = original |> Graph.insert_node (Int64.of_int 3) "c" 
                       (Graph.OS.add ("ac", Int64.of_int 1) Graph.OS.Empty)
                        Graph.OS.Empty  in  
  let g2 = original |> Graph.delete_edge (Int64.of_int 1) (Int64.of_int 2) "af" 
                    |> Graph.delete_node (Int64.of_int 2) in  
  let mg = Graph.merge3 original g1 g2 in  
  Graph.print_graph mg
