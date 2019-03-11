exception Empty 
module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : int64 -> string
    val of_string : string -> t
  end
(* Heap is defined as variant type, Empty and T of node where node is a record type consisting of rank of node, root, left
node and right node *)

(* Properties 
(1) Easy to retrieve minimum element 
(2) Root is always minimum.
(3) Element on the left hand side is smaller than the elements on its right hand side.
(4) Deleting is just deleting the head which is O(n) times.
(5) Inserting is O(n) times
(6) Merge is like mergesort. *)
module Heap_leftlist(Atom:ATOM) =
  struct
    type atom = Atom.t
    type node = {
      ra: int64 ;
      d: Atom.t ;
      l: t ;
      r: t }
    and t =
      | E 
      | T of node [@@derive versioned]
    (* Rank of node which is length of the path between the node and the right most leaf *)
    let rank = function | E -> Int64.of_int 0 | T { ra;_} -> ra

    let makeT x a b =
      if (rank a) >= (rank b)
      then T { ra = Int64.of_int (Int64.to_int (rank b) + 1); d = x; l = a; r = b }
      else T { ra = Int64.of_int (Int64.to_int (rank a) + 1); d = x; l = b; r = a }

    let empty = E

    let is_empty h = h = E
    
    (* merge h1 h2 merges two trees h1 and h2 where suppose h1 is (r1, x, l1, r1) and h2 is (r2, x, l2, r2) 
       (1) Comapre two roots:
           if x > y : switch two trees and perform merge h2 h1. This ensures trees on the left has smaller keys
                      since h2 is the smaller key so root of h2 will become the new root.
                      since we know right branch is always shortest, we then merge h1_right h2
                      continue till one of the tree becomes leaf and return the other in this case
                      Update rank *) 
    let rec merge h1 h2 =
      match (h1, h2) with
      | (_, E) -> h1
      | (E, _) -> h2
      | (T { ra = _; d = x; l = a1; r = b1 }, T
         { ra = _; d = y; l = a2; r = b2 }) ->
          if (Atom.compare x y) <= 0
          then makeT x a1 (merge b1 h2)
          else makeT y a2 (merge h1 b2)
    (* insert x h inserts the element in the heap *)
    (* It comapres the element x with all existing element one by one and insert it in appropriate position. 
       Element on its left hand side must be smaller than it and elements on its right must be bigger *)      
    let insert x h = merge (T { ra = Int64.of_int 1; d = x; l = E; r = E }) h
    
    (* return the minimum element that will be the head : Time is O(1) *)
    let find_min =
      function | E -> raise Empty | T { ra = _; d = x; l = _; r = _ } -> x

    (* delete_min deleters the head and then merge *) 
    let delete_min =
      function
      | E -> raise Empty
      | T { ra = _; d = _; l = a; r = b } -> merge a b

    (* gives the root and then merge the rest *)
    let pop_min =
      function
      | E -> raise Empty
      | T { ra = _; d = x; l = a; r = b } -> (x, (merge a b))

      
    let rec elements h =
      if is_empty h
      then []
      else (let (min, h') = pop_min h in min :: (elements h'))

    (* Two types of edits: Insert and Delete *)  
    type edit =
      | Insert of atom 
      | Delete of atom 

    (* patch is the list of edits *)
    type patch = edit list

    let edit_to_string atom_to_string =
      function
      | Insert a -> Printf.sprintf "Insert (%s)" (atom_to_string a)
      | Delete a -> Printf.sprintf "Delete (%s)" (atom_to_string a)


    let op_diff xt yt =
      let rec heap_diff hx hy =
        match (hx, hy) with
        | (E, E) -> []
        | (E, _) ->
            let (m, hy) = pop_min hy in (Insert m) :: (heap_diff hx hy)
        | (_, E) ->
            let (m, hx) = pop_min hx in (Delete m) :: (heap_diff hx hy)
        | (_, _) ->
            let a1 = find_min hx in
            let a2 = find_min hy in
            let c = Atom.compare a1 a2 in
            if c = 0
            then
              let hy = delete_min hy in
              let hx = delete_min hx in heap_diff hx hy
            else
              if c < 0
              then
                (let hx = delete_min hx in (Delete a1) :: (heap_diff hx hy))
              else
                (let hy = delete_min hy in (Insert a2) :: (heap_diff hx hy)) in
      heap_diff xt yt

    let op_transform p q =
      let rec transform_aux xs ys =
        match (xs, ys) with
        | ([], []) -> ([], [])
        | ([], _) -> ([], ys)
        | (_, []) -> (xs, [])
        | (hx::rxs, hy::rys) ->
            let handle kx ky on_conflict =
              let c = Atom.compare kx ky in
              if c = 0
              then on_conflict ()
              else
                if c < 0
                then (let (a, b) = transform_aux rxs ys in ((hx :: a), b))
                else (let (a, b) = transform_aux xs rys in (a, (hy :: b))) in
            (match (hx, hy) with
             | (Insert x, Insert y)|(Delete x, Delete y) ->
                 let on_conflict () = transform_aux rxs rys in
                 handle x y on_conflict
             | (Insert x, Delete y) ->
                 let on_conflict () =
                   let (a, b) = transform_aux rxs rys in ((hx :: hx :: a), b) in
                 handle x y on_conflict
             | (Delete x, Insert y) ->
                 let on_conflict () =
                   let (a, b) = transform_aux rxs rys in (a, (hy :: hy :: b)) in
                 handle x y on_conflict) in
      transform_aux p q

    let resolve x y = merge x y
    let rec apply s =
      function
      | [] -> s
      | (Insert x)::r -> let s' = insert x s in apply s' r
      | (Delete x)::r ->
          let (xx, s') = pop_min s in let _ = assert (x = xx) in apply s' r
          
    let merge3 ~ancestor  l r =
      let p = op_diff ancestor l in
      let q = op_diff ancestor r in
      let (_, q') = op_transform p q in apply l q'
  end[@@derive_versioned ]