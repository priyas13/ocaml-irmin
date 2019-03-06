exception Empty 
module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val compare : t -> t -> int
    val to_string : int64 -> string
    val of_string : string -> t
  end
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
    let rank = function | E -> Int64.of_int 0 | T { ra;_} -> ra
    let makeT x a b =
      if (rank a) >= (rank b)
      then
        T
          {
            ra = (Int64.of_int ((Int64.to_int (rank b)) + 1));
            d = x;
            l = a;
            r = b
          }
      else
        T
          {
            ra = (Int64.of_int ((Int64.to_int (rank a)) + 1));
            d = x;
            l = b;
            r = a
          }
    let empty = E
    let is_empty h = h = E
    let rec merge h1 h2 =
      match (h1, h2) with
      | (_, E) -> h1
      | (E, _) -> h2
      | (T { ra = _; d = x; l = a1; r = b1 }, T
         { ra = _; d = y; l = a2; r = b2 }) ->
          if (Atom.compare x y) <= 0
          then makeT x a1 (merge b1 h2)
          else makeT y a2 (merge h1 b2)
    let insert x h =
      merge (T { ra = (Int64.of_int 1); d = x; l = E; r = E }) h
    let find_min =
      function | E -> raise Empty | T { ra = _; d = x; l = _; r = _ } -> x
    let delete_min =
      function
      | E -> raise Empty
      | T { ra = _; d = _; l = a; r = b } -> merge a b
    let pop_min =
      function
      | E -> raise Empty
      | T { ra = _; d = x; l = a; r = b } -> (x, (merge a b))
    let rec elements h =
      if is_empty h
      then []
      else (let (min, h') = pop_min h in min :: (elements h'))
    type edit =
      | Insert of atom 
      | Delete of atom 
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
module IHeap_leftlist =
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
        module OM = Heap_leftlist(Atom)
        open OM
        type node = {
          ra: int64 ;
          d: Atom.t ;
          l: K.t ;
          r: K.t }
        and madt =
          | E 
          | T of node 
        module IrminConvert =
          struct
            
            let mknode t =
              let open Irmin.Type in
                (((((record "node"
                       (fun ra -> fun d -> fun l -> fun r -> { ra; d; l; r }))
                      |+ (field "ra" int64 (fun t -> t.ra)))
                     |+ (field "d" Atom.t (fun t -> t.d)))
                    |+ (field "l" K.t (fun t -> t.l)))
                   |+ (field "r" K.t (fun t -> t.r)))
                  |> sealr
            and mkmadt node =
              let open Irmin.Type in
                (((variant "madt"
                     (fun e -> fun t -> function | E -> e | T a0 -> t a0))
                    |~ (case0 "E" E))
                   |~ (case1 "T" node (fun x -> T x)))
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
                  | OM.E -> Lwt.return @@ E
                  | OM.T a0 ->
                      (match a0 with
                       | { ra; d; l; r;_} ->
                           (add_adt t l) >>=
                             ((fun l' ->
                                 (add_adt t r) >>=
                                   (fun r' ->
                                      Lwt.return @@ { ra; d; l = l'; r = r' }))))
                        >>= ((fun a0' -> Lwt.return @@ (T a0')))) : K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in
                    match a with
                    | E -> Lwt.return @@ OM.E
                    | T a0 ->
                        (match a0 with
                         | { ra; d; l; r;_} ->
                             (read_adt t l) >>=
                               ((fun l' ->
                                   (read_adt t r) >>=
                                     (fun r' ->
                                        Lwt.return @@
                                          {
                                            OM.ra = ra;
                                            OM.d = d;
                                            OM.l = l';
                                            OM.r = r'
                                          }))))
                          >>= ((fun a0' -> Lwt.return @@ (OM.T a0')))) : 
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
                     | OM.E -> Lwt.return @@ E
                     | OM.T a0 ->
                         (match a0 with
                          | { ra; d; l; r;_} ->
                              (aostore_add l) >>=
                                ((fun l' ->
                                    (aostore_add r) >>=
                                      (fun r' ->
                                         Lwt.return @@
                                           { ra; d; l = l'; r = r' }))))
                           >>= ((fun a0' -> Lwt.return @@ (T a0')))) : 
               t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     match t with
                     | E -> Lwt.return @@ OM.E
                     | T a0 ->
                         (match a0 with
                          | { ra; d; l; r;_} ->
                              (aostore_read l) >>=
                                ((fun l' ->
                                    (aostore_read r) >>=
                                      (fun r' ->
                                         Lwt.return @@
                                           {
                                             OM.ra = ra;
                                             OM.d = d;
                                             OM.l = l';
                                             OM.r = r'
                                           }))))
                           >>= ((fun a0' -> Lwt.return @@ (OM.T a0')))) : 
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
               | E -> Lwt.return()
               | T a0 ->
                   (match a0 with
                    | { ra; d; l; r;_} ->
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
