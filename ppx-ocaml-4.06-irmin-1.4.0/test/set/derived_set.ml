module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t
  end
module Set(Atom:ATOM) =
  struct
    type atom = Atom.t[@@derive ezjsonm]
    type node = {
      l: t ;
      v: Atom.t ;
      r: t ;
      h: int64 }
    and t =
      | Empty 
      | Node of node [@@derive versioned]
    let height = function | Empty -> Int64.of_int 0 | Node { h;_} -> h
    let create l v r =
      let hl = match l with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      let hr = match r with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      Node
        {
          l;
          v;
          r;
          h =
            (if hl >= hr
             then Int64.of_int ((Int64.to_int hl) + 1)
             else Int64.of_int ((Int64.to_int hr) + 1))
        }
    let bal l v r =
      let hl = match l with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      let hr = match r with | Empty -> Int64.of_int 0 | Node { h;_} -> h in
      if hl > (Int64.of_int ((Int64.to_int hr) + 2))
      then
        match l with
        | Empty -> invalid_arg "Set.bal"
        | Node { l = ll; v = lv; r = lr;_} ->
            (if (height ll) >= (height lr)
             then create ll lv (create lr v r)
             else
               (match lr with
                | Empty -> invalid_arg "Set.bal"
                | Node { l = lrl; v = lrv; r = lrr;_} ->
                    create (create ll lv lrl) lrv (create lrr v r)))
      else
        if hr > (Int64.of_int ((Int64.to_int hl) + 2))
        then
          (match r with
           | Empty -> invalid_arg "Set.bal"
           | Node { l = rl; v = rv; r = rr;_} ->
               if (height rr) >= (height rl)
               then create (create l v rl) rv rr
               else
                 (match rl with
                  | Empty -> invalid_arg "Set.bal"
                  | Node { l = rll; v = rlv; r = rlr;_} ->
                      create (create l v rll) rlv (create rlr rv rr)))
        else
          Node
            {
              l;
              v;
              r;
              h =
                (if hl >= hr
                 then Int64.of_int ((Int64.to_int hl) + 1)
                 else Int64.of_int ((Int64.to_int hr) + 1))
            }
    let rec add x =
      function
      | Empty -> Node { l = Empty; v = x; r = Empty; h = (Int64.of_int 1) }
      | Node { l; v; r;_} as t ->
          let c = Atom.compare x v in
          if c = 0
          then t
          else
            if c < 0
            then (let ll = add x l in if l == ll then t else bal ll v r)
            else (let rr = add x r in if r == rr then t else bal l v rr)
    let singleton x =
      Node { l = Empty; v = x; r = Empty; h = (Int64.of_int 1) }
    let rec add_min_element x =
      function
      | Empty -> singleton x
      | Node { l; v; r;_} -> bal (add_min_element x l) v r
    let rec add_max_element x =
      function
      | Empty -> singleton x
      | Node { l; v; r;_} -> bal l v (add_max_element x r)
    let rec join l v r =
      match (l, r) with
      | (Empty, _) -> add_min_element v r
      | (_, Empty) -> add_max_element v l
      | (Node { l = ll; v = lv; r = lr; h = lh }, Node
         { l = rl; v = rv; r = rr; h = rh }) ->
          if lh > (Int64.of_int ((Int64.to_int rh) + 2))
          then bal ll lv (join lr v r)
          else
            if rh > (Int64.of_int ((Int64.to_int lh) + 2))
            then bal (join l v rl) rv rr
            else create l v r
    let rec min_elt =
      function
      | Empty -> raise Not_found
      | Node { l = Empty; v;_} -> v
      | Node { l;_} -> min_elt l
    let rec min_elt_opt =
      function
      | Empty -> None
      | Node { l = Empty; v;_} -> Some v
      | Node { l;_} -> min_elt_opt l
    let rec max_elt =
      function
      | Empty -> raise Not_found
      | Node { v; r = Empty;_} -> v
      | Node { r;_} -> max_elt r
    let rec max_elt_opt =
      function
      | Empty -> None
      | Node { v; r = Empty;_} -> Some v
      | Node { r;_} -> max_elt_opt r
    let rec remove_min_elt =
      function
      | Empty -> invalid_arg "Set.remove_min_elt"
      | Node { l = Empty; r;_} -> r
      | Node { l; v; r;_} -> bal (remove_min_elt l) v r
    let merge t1 t2 =
      match (t1, t2) with
      | (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)
    let concat t1 t2 =
      match (t1, t2) with
      | (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)
    let rec split x =
      function
      | Empty -> (Empty, false, Empty)
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0
          then (l, true, r)
          else
            if c < 0
            then
              (let (ll, pres, rl) = split x l in (ll, pres, (join rl v r)))
            else
              (let (lr, pres, rr) = split x r in ((join l v lr), pres, rr))
    let empty = Empty
    let is_empty = function | Empty -> true | _ -> false
    let rec mem x =
      function
      | Empty -> false
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          (c = 0) || (mem x (if c < 0 then l else r))
    let rec remove x =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let c = Atom.compare x v in
          if c = 0
          then merge l r
          else
            if c < 0
            then (let ll = remove x l in if l == ll then t else bal ll v r)
            else (let rr = remove x r in if r == rr then t else bal l v rr)
    let rec union s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node { l = l1; v = v1; r = r1; h = h1 }, Node
         { l = l2; v = v2; r = r2; h = h2 }) ->
          if h1 >= h2
          then
            (if h2 = (Int64.of_int 1)
             then add v2 s1
             else
               (let (l2, _, r2) = split v1 s2 in
                join (union l1 l2) v1 (union r1 r2)))
          else
            if h1 = (Int64.of_int 1)
            then add v1 s2
            else
              (let (l1, _, r1) = split v2 s1 in
               join (union l1 l2) v2 (union r1 r2))
    let rec inter s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | (Node { l = l1; v = v1; r = r1;_}, t2) ->
          (match split v1 t2 with
           | (l2, false, r2) -> concat (inter l1 l2) (inter r1 r2)
           | (l2, true, r2) -> join (inter l1 l2) v1 (inter r1 r2))
    let rec diff s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> Empty
      | (t1, Empty) -> t1
      | (Node { l = l1; v = v1; r = r1;_}, t2) ->
          (match split v1 t2 with
           | (l2, false, r2) -> join (diff l1 l2) v1 (diff r1 r2)
           | (l2, true, r2) -> concat (diff l1 l2) (diff r1 r2))
    type enumeration =
      | End 
      | More of atom * t * enumeration 
    let rec cons_enum s e =
      match s with
      | Empty -> e
      | Node { l; v; r;_} -> cons_enum l (More (v, r, e))
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _) -> (-1)
      | (_, End) -> 1
      | (More (v1, r1, e1), More (v2, r2, e2)) ->
          let c = Atom.compare v1 v2 in
          if c <> 0
          then c
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    let compare s1 s2 = compare_aux (cons_enum s1 End) (cons_enum s2 End)
    let equal s1 s2 = (compare s1 s2) = 0
    let rec subset s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> true
      | (_, Empty) -> false
      | (Node { l = l1; v = v1; r = r1;_},
         (Node { l = l2; v = v2; r = r2;_} as t2)) ->
          let c = Atom.compare v1 v2 in
          if c = 0
          then (subset l1 l2) && (subset r1 r2)
          else
            if c < 0
            then
              (subset
                 (Node { l = l1; v = v1; r = Empty; h = (Int64.of_int 0) })
                 l2)
                && (subset r1 t2)
            else
              (subset
                 (Node { l = Empty; v = v1; r = r1; h = (Int64.of_int 0) })
                 r2)
                && (subset l1 t2)
    let rec iter f =
      function | Empty -> () | Node { l; v; r;_} -> (iter f l; f v; iter f r)
    let rec fold f s accu =
      match s with
      | Empty -> accu
      | Node { l; v; r;_} -> fold f r (f v (fold f l accu))
    let rec for_all p =
      function
      | Empty -> true
      | Node { l; v; r;_} -> (p v) && ((for_all p l) && (for_all p r))
    let rec exists p =
      function
      | Empty -> false
      | Node { l; v; r;_} -> (p v) || ((exists p l) || (exists p r))
    let rec filter p =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let l' = filter p l in
          let pv = p v in
          let r' = filter p r in
          if pv
          then (if (l == l') && (r == r') then t else join l' v r')
          else concat l' r'
    let rec partition p =
      function
      | Empty -> (Empty, Empty)
      | Node { l; v; r;_} ->
          let (lt, lf) = partition p l in
          let pv = p v in
          let (rt, rf) = partition p r in
          if pv
          then ((join lt v rt), (concat lf rf))
          else ((concat lt rt), (join lf v rf))
    let rec cardinal =
      function
      | Empty -> 0
      | Node { l; r;_} -> ((cardinal l) + 1) + (cardinal r)
    let rec elements_aux accu =
      function
      | Empty -> accu
      | Node { l; v; r;_} -> elements_aux (v :: (elements_aux accu r)) l
    let elements s = elements_aux [] s
    let choose = min_elt
    let choose_opt = min_elt_opt
    let rec find x =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0 then v else find x (if c < 0 then l else r)
    let rec find_first_aux v0 f =
      function
      | Empty -> v0
      | Node { l; v; r;_} ->
          if f v then find_first_aux v f l else find_first_aux v0 f r
    let rec find_first f =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          if f v then find_first_aux v f l else find_first f r
    let rec find_first_opt_aux v0 f =
      function
      | Empty -> Some v0
      | Node { l; v; r;_} ->
          if f v then find_first_opt_aux v f l else find_first_opt_aux v0 f r
    let rec find_first_opt f =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          if f v then find_first_opt_aux v f l else find_first_opt f r
    let rec find_last_aux v0 f =
      function
      | Empty -> v0
      | Node { l; v; r;_} ->
          if f v then find_last_aux v f r else find_last_aux v0 f l
    let rec find_last f =
      function
      | Empty -> raise Not_found
      | Node { l; v; r;_} ->
          if f v then find_last_aux v f r else find_last f l
    let rec find_last_opt_aux v0 f =
      function
      | Empty -> Some v0
      | Node { l; v; r;_} ->
          if f v then find_last_opt_aux v f r else find_last_opt_aux v0 f l
    let rec find_last_opt f =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          if f v then find_last_opt_aux v f r else find_last_opt f l
    let rec find_opt x =
      function
      | Empty -> None
      | Node { l; v; r;_} ->
          let c = Atom.compare x v in
          if c = 0 then Some v else find_opt x (if c < 0 then l else r)
    let try_join l v r =
      if
        ((l = Empty) || ((Atom.compare (max_elt l) v) < 0)) &&
          ((r = Empty) || ((Atom.compare v (min_elt r)) < 0))
      then join l v r
      else union l (add v r)
    let rec map f =
      function
      | Empty -> Empty
      | Node { l; v; r;_} as t ->
          let l' = map f l in
          let v' = f v in
          let r' = map f r in
          if (l == l') && ((v == v') && (r == r'))
          then t
          else try_join l' v' r'
    let of_sorted_list l =
      let rec sub n l =
        match (n, l) with
        | (0, l) -> (Empty, l)
        | (1, x0::l) ->
            ((Node { l = Empty; v = x0; r = Empty; h = (Int64.of_int 1) }),
              l)
        | (2, x0::x1::l) ->
            ((Node
                {
                  l =
                    (Node
                       { l = Empty; v = x0; r = Empty; h = (Int64.of_int 1) });
                  v = x1;
                  r = Empty;
                  h = (Int64.of_int 2)
                }), l)
        | (3, x0::x1::x2::l) ->
            ((Node
                {
                  l =
                    (Node
                       { l = Empty; v = x0; r = Empty; h = (Int64.of_int 1) });
                  v = x1;
                  r =
                    (Node
                       { l = Empty; v = x2; r = Empty; h = (Int64.of_int 1) });
                  h = (Int64.of_int 2)
                }), l)
        | (n, l) ->
            let nl = n / 2 in
            let (left, l) = sub nl l in
            (match l with
             | [] -> assert false
             | mid::l ->
                 let (right, l) = sub ((n - nl) - 1) l in
                 ((create left mid right), l)) in
      fst (sub (List.length l) l)
    type edit =
      | Add of atom 
      | Remove of atom 
    type patch = edit list
    let edit_to_string atom_to_string =
      function
      | Add a -> Printf.sprintf "Add (%s)" (atom_to_string a)
      | Remove a -> Printf.sprintf "Remove (%s)" (atom_to_string a)
    let op_diff xt yt =
      let rec diff_avlt s1 s2 =
        match (s1, s2) with
        | (Empty, t2) -> fold (fun x -> fun y -> y @ [Add x]) t2 []
        | (t1, Empty) -> fold (fun x -> fun y -> y @ [Remove x]) t1 []
        | (Node { l = l1; v = v1; r = r1; h = h1 }, Node
           { l = l2; v = v2; r = r2; h = h2 }) ->
            if h1 >= h2
            then
              let (l2, p, r2) = split v1 s2 in
              let l = diff_avlt l1 l2 in
              let r = diff_avlt r1 r2 in
              (if p
               then List.append l r
               else List.append l ((Remove v1) :: r))
            else
              (let (l1, p, r1) = split v2 s1 in
               let l = diff_avlt l1 l2 in
               let r = diff_avlt r1 r2 in
               if p then List.append l r else List.append l ((Add v2) :: r)) in
      diff_avlt xt yt
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
             | (Add x, Add y)|(Remove x, Remove y) ->
                 let on_conflict () = transform_aux rxs rys in
                 handle x y on_conflict
             | (Add x, Remove y)|(Remove x, Add y) ->
                 let on_conflict () = assert false in handle x y on_conflict) in
      transform_aux p q
    let resolve = union
    let rec apply s =
      function
      | [] -> s
      | (Add x)::r -> let s' = add x s in apply s' r
      | (Remove x)::r -> let s' = remove x s in apply s' r
    let print_set f s =
      let rec print_elements =
        function
        | Empty -> ()
        | Node { l; v; r; h } ->
            (print_elements l;
             print_string ";";
             f v;
             print_string ";";
             print_elements r;
             print_string ";";
             print_int (Int64.to_int h)) in
      print_string "{"; print_elements s; print_string "}"
    let merge3 ~ancestor  l r =
      let p = op_diff ancestor l in
      let q = op_diff ancestor r in
      let (_, q') = op_transform p q in apply l q'
  end[@@derive_versioned ]
module ISet =
  struct
    open Lwt.Infix
    open Irmin_unix
    module K = Irmin.Hash.SHA1
    module G = Git_unix.FS
    module type Config  =
      sig val root : string val shared : string val init : unit -> unit end
    let from_just op msg =
      match op with
      | Some x -> x
      | None -> failwith @@ (msg ^ ": Expected Some. Got None.")
    module MakeVersioned(Config:Config)(Atom:ATOM) =
      struct
        module OM = Set(Atom)
        open OM
        type node = {
          l: K.t ;
          v: Atom.t ;
          r: K.t ;
          h: int64 }
        and madt =
          | Empty 
          | Node of node 
        module IrminConvert =
          struct
            let atom = let open Irmin.Type in Atom.t
            let mknode t =
              let open Irmin.Type in
                (((((record "node"
                       (fun l -> fun v -> fun r -> fun h -> { l; v; r; h }))
                      |+ (field "l" K.t (fun t -> t.l)))
                     |+ (field "v" Atom.t (fun t -> t.v)))
                    |+ (field "r" K.t (fun t -> t.r)))
                   |+ (field "h" int64 (fun t -> t.h)))
                  |> sealr
            and mkmadt node =
              let open Irmin.Type in
                (((variant "madt"
                     (fun empty ->
                        fun node ->
                          function | Empty -> empty | Node a0 -> node a0))
                    |~ (case0 "Empty" Empty))
                   |~ (case1 "Node" node (fun x -> Node x)))
                  |> sealv
          end
        module IrminConvertTie =
          struct
            
            let () = ()
            and (node, madt) =
              let open Irmin.Type in
                mu2
                  (fun node ->
                     fun madt ->
                       ((IrminConvert.mknode madt),
                         (IrminConvert.mkmadt node)))
          end
        module AO_value =
          (struct
             type t = madt
             let t = IrminConvertTie.madt
             let pp = Irmin.Type.pp_json ~minify:false t
             let of_string s =
               let decoder = Jsonm.decoder (`String s) in
               let res =
                 try Irmin.Type.decode_json t decoder
                 with
                 | Invalid_argument s ->
                     failwith @@
                       (Printf.sprintf
                          "AO_Value.of_string: Invalid_argument: %s" s) in
               res
           end : (Irmin.Contents.Conv with type  t =  madt))
        module AO_store =
          struct
            module S = ((Irmin_git.AO)(Git_unix.FS))(AO_value)
            include S
            let create config =
              let level =
                Irmin.Private.Conf.key ~doc:"The Zlib compression level."
                  "level" (let open Irmin.Private.Conf in some int) None in
              let root =
                Irmin.Private.Conf.get config Irmin.Private.Conf.root in
              let level = Irmin.Private.Conf.get config level in
              G.create ?root ?level ()
            let create () = create @@ (Irmin_git.config Config.root)
            let on_add = ref (fun k -> fun v -> Lwt.return ())
            let add t v =
              (S.add t v) >>=
                (fun k -> ((!on_add) k v) >>= (fun _ -> Lwt.return k))
            let rec add_adt t (a : OM.t) =
              ((add t) =<<
                 (match a with
                  | OM.Empty -> Lwt.return @@ Empty
                  | OM.Node a0 ->
                      (match a0 with
                       | { l; v; r; h;_} ->
                           (add_adt t l) >>=
                             ((fun l' ->
                                 (add_adt t r) >>=
                                   (fun r' ->
                                      Lwt.return @@ { l = l'; v; r = r'; h }))))
                        >>= ((fun a0' -> Lwt.return @@ (Node a0')))) : 
              K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in
                    match a with
                    | Empty -> Lwt.return @@ OM.Empty
                    | Node a0 ->
                        (match a0 with
                         | { l; v; r; h;_} ->
                             (read_adt t l) >>=
                               ((fun l' ->
                                   (read_adt t r) >>=
                                     (fun r' ->
                                        Lwt.return @@
                                          {
                                            OM.l = l';
                                            OM.v = v;
                                            OM.r = r';
                                            OM.h = h
                                          }))))
                          >>= ((fun a0' -> Lwt.return @@ (OM.Node a0')))) : 
              OM.t Lwt.t)
          end
        module type IRMIN_STORE_VALUE  =
          sig
            include Irmin.Contents.S
            val of_adt : OM.t -> t Lwt.t
            val to_adt : t -> OM.t Lwt.t
          end
        module BC_value =
          (struct
             include AO_value
             let of_adt (a : OM.t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_add adt = AO_store.add_adt ao_store adt in
                     match a with
                     | OM.Empty -> Lwt.return @@ Empty
                     | OM.Node a0 ->
                         (match a0 with
                          | { l; v; r; h;_} ->
                              (aostore_add l) >>=
                                ((fun l' ->
                                    (aostore_add r) >>=
                                      (fun r' ->
                                         Lwt.return @@
                                           { l = l'; v; r = r'; h }))))
                           >>= ((fun a0' -> Lwt.return @@ (Node a0')))) : 
               t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     match t with
                     | Empty -> Lwt.return @@ OM.Empty
                     | Node a0 ->
                         (match a0 with
                          | { l; v; r; h;_} ->
                              (aostore_read l) >>=
                                ((fun l' ->
                                    (aostore_read r) >>=
                                      (fun r' ->
                                         Lwt.return @@
                                           {
                                             OM.l = l';
                                             OM.v = v;
                                             OM.r = r';
                                             OM.h = h
                                           }))))
                           >>= ((fun a0' -> Lwt.return @@ (OM.Node a0')))) : 
               OM.t Lwt.t)
             let rec merge ~old:(old : t Irmin.Merge.promise)  (v1 : t)
               (v2 : t) =
               let open Irmin.Merge.Infix in
                 (old ()) >>=*
                   (fun old ->
                      (to_adt (from_just old "merge")) >>=
                        (fun oldv ->
                           (to_adt v1) >>=
                             (fun v1 ->
                                (to_adt v2) >>=
                                  (fun v2 ->
                                     let v = OM.merge3 oldv v1 v2 in
                                     (of_adt v) >>=
                                       (fun merged_v ->
                                          Irmin.Merge.ok merged_v)))))
             let merge = let open Irmin.Merge in option (v t merge)
           end : (IRMIN_STORE_VALUE with type  t =  madt))
        module BC_store =
          struct
            module Store = (Irmin_unix.Git.FS.KV)(BC_value)
            module Sync = (Irmin.Sync)(Store)
            type t = Store.t
            type path = string list
            let init ?root  ?bare  () =
              let config = Irmin_git.config Config.root in
              Store.Repo.v config
            let master (repo : Store.repo) = Store.master repo
            let clone t name = Store.clone t name
            let get_branch r ~branch_name  = Store.of_branch r branch_name
            let merge s ~into  = Store.merge s ~into
            let read t (p : path) = Store.find t p
            let string_of_path p = String.concat "/" p
            let info s = Irmin_unix.info "[repo %s] %s" Config.root s
            let rec update ?msg  t (p : path) (v : BC_value.t) =
              let msg =
                match msg with
                | Some s -> s
                | None -> "Setting " ^ (string_of_path p) in
              let fname_of_hash hsh =
                String.sub (Fmt.to_to_string Irmin.Hash.SHA1.pp hsh) 0 7 in
              let link_to_tree k =
                (AO_store.create ()) >>=
                  (fun ao_store ->
                     (AO_store.find ao_store k) >>=
                       (fun vop ->
                          let v_k = from_just vop "BC_store.update" in
                          let path_k = [fname_of_hash k] in
                          update t path_k v_k)) in
              (match v with
               | Empty -> Lwt.return()
               | Node a0 ->
                   (match a0 with
                    | { l; v; r; h;_} ->
                        List.fold_left
                          (fun m -> fun k -> m >>= (fun () -> link_to_tree k))
                          (Lwt.return()) [l; r])
                     >>= ((fun a0' -> Lwt.return())))
                >>= (fun () -> Store.set t p v ~info:(info msg))
          end
        module Vpst :
          sig
            type 'a t
            val return : 'a -> 'a t
            val bind : 'a t -> ('a -> 'b t) -> 'b t
            val with_init_version_do : OM.t -> 'a t -> 'a
            val with_remote_version_do : string -> 'a t -> 'a
            val fork_version : 'a t -> unit t
            val get_latest_version : unit -> OM.t t
            val sync_next_version : ?v:OM.t -> OM.t t
            val liftLwt : 'a Lwt.t -> 'a t
            val pull_remote : string -> unit t
          end =
          struct
            type store = BC_store.t
            type st =
              {
              master: store ;
              local: store ;
              name: string ;
              next_id: int }
            type 'a t = st -> ('a * st) Lwt.t
            let info s = Irmin_unix.info "[repo %s] %s" Config.root s
            let path = ["state"]
            let return (x : 'a) = (fun st -> Lwt.return (x, st) : 'a t)
            let bind (m1 : 'a t) (f : 'a -> 'b t) =
              (fun st -> (m1 st) >>= (fun (a, st') -> f a st') : 'b t)
            let with_init_version_do (v : OM.t) (m : 'a t) =
              Lwt_main.run
                ((BC_store.init ()) >>=
                   (fun repo ->
                      (BC_store.master repo) >>=
                        (fun m_br ->
                           (BC_value.of_adt v) >>=
                             (fun (v' : BC_value.t) ->
                                (BC_store.update ~msg:"initial version" m_br
                                   path v')
                                  >>=
                                  (fun () ->
                                     (BC_store.clone m_br "1_local") >>=
                                       (fun t_br ->
                                          let st =
                                            {
                                              master = m_br;
                                              local = t_br;
                                              name = "1";
                                              next_id = 1
                                            } in
                                          (m st) >>=
                                            (fun (a, _) -> Lwt.return a)))))))
            let fork_version (m : 'a t) =
              (fun (st : st) ->
                 let thread_f () =
                   let child_name =
                     st.name ^ ("_" ^ (string_of_int st.next_id)) in
                   let parent_m_br = st.master in
                   let m_br = parent_m_br in
                   (BC_store.clone m_br (child_name ^ "_local")) >>=
                     (fun t_br ->
                        let new_st =
                          {
                            master = m_br;
                            local = t_br;
                            name = child_name;
                            next_id = 1
                          } in
                        m new_st) in
                 Lwt.async thread_f;
                 Lwt.return ((), { st with next_id = (st.next_id + 1) }) : 
              unit t)
            let get_latest_version () =
              (fun (st : st) ->
                 (BC_store.read st.local path) >>=
                   (fun (vop : BC_value.t option) ->
                      let v = from_just vop "get_latest_version" in
                      (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) : 
              OM.t t)
            let pull_remote remote_uri (st : st) =
              let cinfo =
                info
                  (Printf.sprintf "Merging remote(%s) to local master"
                     remote_uri) in
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote (`Merge cinfo)) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
            let with_remote_version_do remote_uri m =
              Lwt_main.run
                ((BC_store.init ()) >>=
                   (fun repo ->
                      (BC_store.master repo) >>=
                        (fun m_br ->
                           let remote = Irmin.remote_uri remote_uri in
                           (BC_store.Sync.pull m_br remote `Set) >>=
                             (fun res ->
                                (match res with
                                 | Ok _ -> Lwt.return ()
                                 | Error _ ->
                                     failwith
                                       "Error while pulling the remote")
                                  >>=
                                  (fun _ ->
                                     (BC_store.clone m_br "1_local") >>=
                                       (fun t_br ->
                                          let st =
                                            {
                                              master = m_br;
                                              local = t_br;
                                              name = "1";
                                              next_id = 1
                                            } in
                                          (m st) >>=
                                            (fun (a, _) -> Lwt.return a)))))))
            let sync_next_version ?v  =
              (fun (st : st) ->
                 (match v with
                  | None -> Lwt.return ()
                  | Some v ->
                      (BC_value.of_adt v) >>=
                        ((fun v' ->
                            BC_store.update ~msg:"Committing local state"
                              st.local path v')))
                   >>=
                   (fun () ->
                      let cinfo = info "Merging master into local" in
                      (BC_store.merge st.master ~into:(st.local) ~info:cinfo)
                        >>=
                        (fun _ ->
                           let cinfo = info "Merging local into master" in
                           (BC_store.merge st.local ~into:(st.master)
                              ~info:cinfo)
                             >>= (fun _ -> get_latest_version () st))) : 
              OM.t t)
            let liftLwt (m : 'a Lwt.t) =
              (fun st -> m >>= (fun a -> Lwt.return (a, st)) : 'a t)
          end 
      end
  end
