module Id = struct 
  type t = int64

  let t = Irmin.Type.int64

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare = Int64.compare

  let of_int = Int64.of_int

  let random ?x y = 
    let x = match x with 
      | Some x -> Int64.of_int x
      | None -> 0L in
    let y = Int64.of_int y in
    Int64.add (Random.int64 (Int64.sub y x)) x
end

type id = Id.t

let id = Id.t

let counter_merge lca v1 v2 = 
  let (+) a b = Int32.add a b in
  let (-) a b = Int32.sub a b in
  lca + (v1-lca) + (v2-lca)

let minf f l = 
  List.fold_left 
    (fun acc x -> match acc with
       | None -> Some x
       | Some x' -> if Int64.compare (f x) (f x') < 0 
                  then Some x else acc)
    None l

(* Information about the customer *)
module Customer = struct
type t = {c_id : id; }