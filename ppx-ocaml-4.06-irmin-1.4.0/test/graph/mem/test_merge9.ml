(* Graph *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end



(* Graph *)
let _ =
  U.print_header "Graph";
let _ = 
  U.print_header "Merge9" in 

  let module Graph = Graph_imp.Make in

  let original = Graph.E_G in 
  let g1 = Graph.G((Graph.OS.Empty, Int64.of_int 1, "a", Graph.OS.Empty), 
                           Graph.G(((Graph.OS.add ("ab", Int64.of_int 1) Graph.OS.Empty), Int64.of_int 2, "b", Graph.OS.Empty), 
                           Graph.G(((Graph.OS.add ("ac", Int64.of_int 1) Graph.OS.Empty), Int64.of_int 3, "c", Graph.OS.Empty), E_G))) in  
  let g2 = Graph.G((Graph.OS.Empty, Int64.of_int 1, "a", Graph.OS.Empty), 
                    Graph.G((Graph.OS.Empty, Int64.of_int 2, "b", (Graph.OS.add ("ba", Int64.of_int 1) Graph.OS.Empty)), E_G)) in  
  let mg = Graph.merge3 original g1 g2 in 
  let test = Graph.get_common_nodes g1 original in 
  let gtest = Graph.delete_nodes test g1 in 
  Graph.print_graph mg;
  Graph.print_list Graph.print_int64 test

