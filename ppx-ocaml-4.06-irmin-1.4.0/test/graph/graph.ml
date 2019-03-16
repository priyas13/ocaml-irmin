open Printf
module type ATOM = sig
  type t
  val t: t Irmin.Type.t
  val to_string : t -> string   
  val of_string: string -> t
  val compare: t -> t -> int
  (*include Msigs.RESOLVEABLE with type t := t*)
end

module Make =
  struct
   (*module OL = Mlist.Make(Atom)*)
   type node = int64 
   type edge = char
   type to_edge = (edge * node) list
   type from_edge = (edge * node) list
   type context = {a_t : to_edge ; n : node; a_f : from_edge}
   type t = 
           | E_G 
           | G of context


   let empty = E_G

   let print_int64 i = output_string stdout (string_of_int (Int64.to_int i))

   let print_list f lst = 
    let rec print_elements = function
      | [] -> ()
      | x :: xl -> f x ; print_elements xl in 
    print_string "[";
    print_elements lst;
    print_string "]"

    let print_pair f f' p = 
     let print_elements = function
     | (x,y) -> (f x, f' y) in 
     print_string "(";
     print_elements;
     print_string ")"

   let print_adj_list l = print_list (print_pair (print_char) (print_int64)) l

   let print_c f f' c = 
     let rec print_elements = function
      | {a_t= at; n = n'; a_f = af} ->
         f' at ;
         f n' ;
         f' af in 
        print_string "}";
        print_elements;
        print_string "}" 

   let print_graph f g = 
     let rec print_elements = function 
       | E_G -> ()
       | G a -> f a in 
         print_string "Graph";
         print_elements




 end