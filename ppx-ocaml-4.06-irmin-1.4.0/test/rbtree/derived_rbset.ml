module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t
  end
module type S  =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> (t * t)
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> (t * bool * t)
  end
module Rbset(Atom:ATOM) =
  struct
    type elt = Atom.t[@@derive ezjsonm]
    type node = (t * Atom.t * t)
    and t =
      | Empty 
      | Black of node 
      | Red of node [@@derive versioned]
    type enum =
      | More of elt * t * enum 
      | End [@@derive ezjsonm]
    let rec enum s e =
      match s with
      | Empty -> e
      | Black (l, x, r)|Red (l, x, r) -> enum l (More (x, r, e))
    let blackify =
      function | Red (l, x, r) -> ((Black (l, x, r)), false) | s -> (s, true)
    let empty = Empty
    let is_empty = function | Empty -> true | _ -> false
    let rec mem x =
      function
      | Empty -> false
      | Red (l, y, r)|Black (l, y, r) ->
          let c = Atom.compare x y in
          if c < 0 then mem x l else if c > 0 then mem x r else true
    let balance_left l x r =
      match (l, x, r) with
      | (Red (Red (a, x, b), y, c), z, d)|(Red (a, x, Red (b, y, c)), z, d)
          -> Red ((Black (a, x, b)), y, (Black (c, z, d)))
      | (l, x, r) -> Black (l, x, r)
    let balance_right l x r =
      match (l, x, r) with
      | (a, x, Red (Red (b, y, c), z, d))|(a, x, Red (b, y, Red (c, z, d)))
          -> Red ((Black (a, x, b)), y, (Black (c, z, d)))
      | (l, x, r) -> Black (l, x, r)
    let add x s =
      let rec add_aux =
        function
        | Empty -> Red (Empty, x, Empty)
        | Red (l, y, r) as s ->
            let c = Atom.compare x y in
            if c < 0
            then Red ((add_aux l), y, r)
            else if c > 0 then Red (l, y, (add_aux r)) else s
        | Black (l, y, r) as s ->
            let c = Atom.compare x y in
            if c < 0
            then balance_left (add_aux l) y r
            else if c > 0 then balance_right l y (add_aux r) else s in
      fst (blackify (add_aux s))
    let singleton x = Black (Empty, x, Empty)
    let unbalanced_left =
      function
      | Red (Black (a, x, b), y, c) ->
          ((balance_left (Red (a, x, b)) y c), false)
      | Black (Black (a, x, b), y, c) ->
          ((balance_left (Red (a, x, b)) y c), true)
      | Black (Red (a, x, Black (b, y, c)), z, d) ->
          ((Black (a, x, (balance_left (Red (b, y, c)) z d))), false)
      | _ -> assert false
    let unbalanced_right =
      function
      | Red (a, x, Black (b, y, c)) ->
          ((balance_right a x (Red (b, y, c))), false)
      | Black (a, x, Black (b, y, c)) ->
          ((balance_right a x (Red (b, y, c))), true)
      | Black (a, x, Red (Black (b, y, c), z, d)) ->
          ((Black ((balance_right a x (Red (b, y, c))), z, d)), false)
      | _ -> assert false
    let rec remove_min =
      function
      | Empty|Black (Empty, _, Black _) -> assert false
      | Black (Empty, x, Empty) -> (Empty, x, true)
      | Black (Empty, x, Red (l, y, r)) -> ((Black (l, y, r)), x, false)
      | Red (Empty, x, r) -> (r, x, false)
      | Black (l, x, r) ->
          let (l, y, d) = remove_min l in
          let s = Black (l, x, r) in
          if d
          then let (s, d) = unbalanced_right s in (s, y, d)
          else (s, y, false)
      | Red (l, x, r) ->
          let (l, y, d) = remove_min l in
          let s = Red (l, x, r) in
          if d
          then let (s, d) = unbalanced_right s in (s, y, d)
          else (s, y, false)
    let remove x s =
      let rec remove_aux =
        function
        | Empty -> (Empty, false)
        | Black (l, y, r) ->
            let c = Atom.compare x y in
            if c < 0
            then
              let (l, d) = remove_aux l in
              let s = Black (l, y, r) in
              (if d then unbalanced_right s else (s, false))
            else
              if c > 0
              then
                (let (r, d) = remove_aux r in
                 let s = Black (l, y, r) in
                 if d then unbalanced_left s else (s, false))
              else
                (match r with
                 | Empty -> blackify l
                 | _ ->
                     let (r, y, d) = remove_min r in
                     let s = Black (l, y, r) in
                     if d then unbalanced_left s else (s, false))
        | Red (l, y, r) ->
            let c = Atom.compare x y in
            if c < 0
            then
              let (l, d) = remove_aux l in
              let s = Red (l, y, r) in
              (if d then unbalanced_right s else (s, false))
            else
              if c > 0
              then
                (let (r, d) = remove_aux r in
                 let s = Red (l, y, r) in
                 if d then unbalanced_left s else (s, false))
              else
                (match r with
                 | Empty -> (l, false)
                 | _ ->
                     let (r, y, d) = remove_min r in
                     let s = Red (l, y, r) in
                     if d then unbalanced_left s else (s, false)) in
      fst (remove_aux s)
    let union s1 s2 =
      let rec union_aux e1 e2 accu =
        match (e1, e2) with
        | (End, End) -> accu
        | (End, More (x, r, e))|(More (x, r, e), End) ->
            union_aux End (enum r e) (add x accu)
        | ((More (x1, r1, e1) as e1'), (More (x2, r2, e2) as e2')) ->
            let c = Atom.compare x1 x2 in
            if c < 0
            then union_aux (enum r1 e1) e2' (add x1 accu)
            else
              if c > 0
              then union_aux e1' (enum r2 e2) (add x2 accu)
              else union_aux (enum r1 e1) (enum r2 e2) (add x1 accu) in
      union_aux (enum s1 End) (enum s2 End) Empty
    let inter s1 s2 =
      let rec inter_aux e1 e2 accu =
        match (e1, e2) with
        | (End, _)|(_, End) -> accu
        | ((More (x1, r1, e1) as e1'), (More (x2, r2, e2) as e2')) ->
            let c = Atom.compare x1 x2 in
            if c < 0
            then inter_aux (enum r1 e1) e2' accu
            else
              if c > 0
              then inter_aux e1' (enum r2 e2) accu
              else inter_aux (enum r1 e1) (enum r2 e2) (add x1 accu) in
      inter_aux (enum s1 End) (enum s2 End) Empty
    let diff s1 s2 =
      let rec diff_aux e1 e2 accu =
        match (e1, e2) with
        | (End, _) -> accu
        | (More (x, r, e), End) -> diff_aux (enum r e) End (add x accu)
        | ((More (x1, r1, e1) as e1'), (More (x2, r2, e2) as e2')) ->
            let c = Atom.compare x1 x2 in
            if c < 0
            then diff_aux (enum r1 e1) e2' (add x1 accu)
            else
              if c > 0
              then diff_aux e1' (enum r2 e2) accu
              else diff_aux (enum r1 e1) (enum r2 e2) accu in
      diff_aux (enum s1 End) (enum s2 End) Empty
    let compare s1 s2 =
      let rec compare_aux e1 e2 =
        match (e1, e2) with
        | (End, End) -> 0
        | (End, _) -> (-1)
        | (_, End) -> 1
        | (More (x1, r1, e1), More (x2, r2, e2)) ->
            let c = Atom.compare x1 x2 in
            if c <> 0 then c else compare_aux (enum r1 e1) (enum r2 e2) in
      compare_aux (enum s1 End) (enum s2 End)
    let equal s1 s2 = (compare s1 s2) = 0
    let rec subset s1 s2 =
      match (s1, s2) with
      | (Empty, _) -> true
      | (_, Empty) -> false
      | ((Black (l1, x1, r1)|Red (l1, x1, r1)),
         (Black (l2, x2, r2)|Red (l2, x2, r2) as s2)) ->
          let c = Atom.compare x1 x2 in
          if c = 0
          then (subset l1 l2) && (subset r1 r2)
          else
            if c < 0
            then (subset (Black (l1, x1, Empty)) l2) && (subset r1 s2)
            else (subset (Black (Empty, x1, r1)) r2) && (subset l1 s2)
    let rec iter f =
      function
      | Empty -> ()
      | Black (l, x, r)|Red (l, x, r) -> (iter f l; f x; iter f r)
    let rec fold f s accu =
      match s with
      | Empty -> accu
      | Black (l, x, r)|Red (l, x, r) -> fold f r (f x (fold f l accu))
    let rec for_all p =
      function
      | Empty -> true
      | Black (l, x, r)|Red (l, x, r) ->
          (p x) && ((for_all p l) && (for_all p r))
    let rec exists p =
      function
      | Empty -> false
      | Black (l, x, r)|Red (l, x, r) ->
          (p x) || ((exists p l) || (exists p r))
    let filter p s =
      let rec filter_aux accu =
        function
        | Empty -> accu
        | Black (l, x, r)|Red (l, x, r) ->
            filter_aux (filter_aux (if p x then add x accu else accu) l) r in
      filter_aux Empty s
    let partition p s =
      let rec partition_aux ((t, f) as accu) =
        function
        | Empty -> accu
        | Black (l, x, r)|Red (l, x, r) ->
            partition_aux
              (partition_aux (if p x then ((add x t), f) else (t, (add x f)))
                 l) r in
      partition_aux (Empty, Empty) s
    let rec cardinal =
      function
      | Empty -> 0
      | Black (l, x, r)|Red (l, x, r) -> (1 + (cardinal l)) + (cardinal r)
    let rec elements_aux accu =
      function
      | Empty -> accu
      | Black (l, x, r)|Red (l, x, r) ->
          elements_aux (x :: (elements_aux accu r)) l
    let elements s = elements_aux [] s
    let rec min_elt =
      function
      | Empty -> raise Not_found
      | Black (Empty, x, _)|Red (Empty, x, _) -> x
      | Black (l, _, _)|Red (l, _, _) -> min_elt l
    let rec max_elt =
      function
      | Empty -> raise Not_found
      | Black (_, x, Empty)|Red (_, x, Empty) -> x
      | Black (_, _, r)|Red (_, _, r) -> max_elt r
    let choose =
      function
      | Empty -> raise Not_found
      | Black (_, x, _)|Red (_, x, _) -> x
    let split x s =
      let rec split_aux y (l, b, r) =
        let c = Atom.compare x y in
        if c < 0
        then (l, b, (add x r))
        else if c > 0 then ((add x l), b, r) else (l, true, r) in
      fold split_aux s (Empty, false, Empty)
    let merge3 old v1 v2 = failwith "Unimpl."
  end[@@derive_versioned ]
module IRbset =
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
        module OM = Rbset(Atom)
        open OM
        type node = (K.t * (Atom.t * K.t))
        and madt =
          | Empty 
          | Black of node 
          | Red of node 
        module IrminConvert =
          struct
            let elt = let open Irmin.Type in Atom.t
            let mknode t = let open Irmin.Type in pair K.t (pair Atom.t K.t)
            and mkmadt node =
              let open Irmin.Type in
                ((((variant "madt"
                      (fun empty ->
                         fun black ->
                           fun red ->
                             function
                             | Empty -> empty
                             | Black a0 -> black a0
                             | Red a0 -> red a0))
                     |~ (case0 "Empty" Empty))
                    |~ (case1 "Black" node (fun x -> Black x)))
                   |~ (case1 "Red" node (fun x -> Red x)))
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
                  | OM.Black a0 ->
                      (match a0 with
                       | (b0, b1, b2) ->
                           (add_adt t b0) >>=
                             ((fun b0' ->
                                 (add_adt t b2) >>=
                                   (fun b2' -> Lwt.return @@ (b0', (b1, b2'))))))
                        >>= ((fun a0' -> Lwt.return @@ (Black a0')))
                  | OM.Red a0 ->
                      (match a0 with
                       | (b0, b1, b2) ->
                           (add_adt t b0) >>=
                             ((fun b0' ->
                                 (add_adt t b2) >>=
                                   (fun b2' -> Lwt.return @@ (b0', (b1, b2'))))))
                        >>= ((fun a0' -> Lwt.return @@ (Red a0')))) : 
              K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in
                    match a with
                    | Empty -> Lwt.return @@ OM.Empty
                    | Black a0 ->
                        (match a0 with
                         | (b0, (b1, b2)) ->
                             (read_adt t b0) >>=
                               ((fun b0' ->
                                   (read_adt t b2) >>=
                                     (fun b2' -> Lwt.return @@ (b0', b1, b2')))))
                          >>= ((fun a0' -> Lwt.return @@ (OM.Black a0')))
                    | Red a0 ->
                        (match a0 with
                         | (b0, (b1, b2)) ->
                             (read_adt t b0) >>=
                               ((fun b0' ->
                                   (read_adt t b2) >>=
                                     (fun b2' -> Lwt.return @@ (b0', b1, b2')))))
                          >>= ((fun a0' -> Lwt.return @@ (OM.Red a0')))) : 
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
                     | OM.Black a0 ->
                         (match a0 with
                          | (b0, b1, b2) ->
                              (aostore_add b0) >>=
                                ((fun b0' ->
                                    (aostore_add b2) >>=
                                      (fun b2' ->
                                         Lwt.return @@ (b0', (b1, b2'))))))
                           >>= ((fun a0' -> Lwt.return @@ (Black a0')))
                     | OM.Red a0 ->
                         (match a0 with
                          | (b0, b1, b2) ->
                              (aostore_add b0) >>=
                                ((fun b0' ->
                                    (aostore_add b2) >>=
                                      (fun b2' ->
                                         Lwt.return @@ (b0', (b1, b2'))))))
                           >>= ((fun a0' -> Lwt.return @@ (Red a0')))) : 
               t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     match t with
                     | Empty -> Lwt.return @@ OM.Empty
                     | Black a0 ->
                         (match a0 with
                          | (b0, (b1, b2)) ->
                              (aostore_read b0) >>=
                                ((fun b0' ->
                                    (aostore_read b2) >>=
                                      (fun b2' ->
                                         Lwt.return @@ (b0', b1, b2')))))
                           >>= ((fun a0' -> Lwt.return @@ (OM.Black a0')))
                     | Red a0 ->
                         (match a0 with
                          | (b0, (b1, b2)) ->
                              (aostore_read b0) >>=
                                ((fun b0' ->
                                    (aostore_read b2) >>=
                                      (fun b2' ->
                                         Lwt.return @@ (b0', b1, b2')))))
                           >>= ((fun a0' -> Lwt.return @@ (OM.Red a0')))) : 
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
               | Black a0 ->
                   (match a0 with
                    | (b0, (b1, b2)) ->
                        List.fold_left
                          (fun m -> fun k -> m >>= (fun () -> link_to_tree k))
                          (Lwt.return()) [b0; b2])
                     >>= ((fun a0' -> Lwt.return()))
               | Red a0 ->
                   (match a0 with
                    | (b0, (b1, b2)) ->
                        List.fold_left
                          (fun m -> fun k -> m >>= (fun () -> link_to_tree k))
                          (Lwt.return()) [b0; b2])
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
