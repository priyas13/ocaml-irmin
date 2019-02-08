module type ATOM  =
  sig
    type t
    val t : t Irmin.Type.t
    val to_string : t -> string
    val of_string : string -> t
    include (Msigs.RESOLVEABLE with type  t :=  t)
  end
module List(Atom:ATOM) =
  struct
    type atom = Atom.t[@@derive ezjsonm]
    type t = atom list[@@derive versioned]
    let empty = []
    let length (x : t) = List.length x
    let split_at l i =
      if (List.length l) > i
      then
        let rec aux xs i acc =
          if i >= 0
          then aux (List.tl xs) (i - 1) ((List.hd xs) :: acc)
          else (acc, xs) in
        aux l i []
      else raise (Invalid_argument "out of bound index")
    let insert l i a =
      if (List.length l) = i
      then List.append l [a]
      else
        (let (rxs, ys) = split_at l i in
         let ixs =
           match rxs with | [] -> [a] | x::[] -> [x; a] | x::y -> x :: a :: y in
         List.rev_append ixs ys)
    let get l i = List.nth l i
    let set l i a =
      let (rxs, ys) = split_at l i in
      let sxs = a :: (List.tl rxs) in List.rev_append sxs ys
    let delete l i =
      let (rxs, ys) = split_at l i in
      let dxs = List.tl rxs in List.rev_append dxs ys
    type edit =
      | Ins of int * atom 
      | Del of int * atom 
      | Rep of int * atom * atom 
    type patch = edit list
    let edit_to_string atom_to_string =
      function
      | Ins (i, a) -> Printf.sprintf "Ins (%i, %s)" i (atom_to_string a)
      | Del (i, a) -> Printf.sprintf "Del (%i, %s)" i (atom_to_string a)
      | Rep (i, a, b) ->
          Printf.sprintf "Rep (%i, %s, %s)" i (atom_to_string a)
            (atom_to_string b)
    let op_diff xs ys =
      let cache =
        Array.init ((length xs) + 1)
          (fun _ -> Array.make ((length ys) + 1) None) in
      let rec loop i j =
        let cache_i = Array.unsafe_get cache i in
        let min3 x y z =
          let m' (a, al) (b, bl) = if a < b then (a, al) else (b, bl) in
          m' (m' x y) z in
        match Array.unsafe_get cache_i j with
        | Some v -> v
        | None ->
            let res =
              match (i, j) with
              | (0, 0) -> (0, [])
              | (0, j) ->
                  let (d, e) = loop 0 (j - 1) in
                  ((d + 1), ((Ins (i, (get ys (j - 1)))) :: e))
              | (i, 0) ->
                  let (d, e) = loop (i - 1) 0 in
                  ((d + 1), ((Del ((i - 1), (get xs (i - 1)))) :: e))
              | _ ->
                  let xsim1 = get xs (i - 1) in
                  let ysim1 = get ys (j - 1) in
                  let (d, e) = loop (i - 1) j in
                  let r1 = ((d + 1), ((Del ((i - 1), xsim1)) :: e)) in
                  let (d, e) = loop i (j - 1) in
                  let r2 = ((d + 1), ((Ins (i, ysim1)) :: e)) in
                  let (d, e) = loop (i - 1) (j - 1) in
                  let r3 =
                    if xsim1 = ysim1
                    then (d, e)
                    else ((d + 1), ((Rep ((i - 1), xsim1, ysim1)) :: e)) in
                  min3 r1 r2 r3 in
            (Array.unsafe_set cache_i j (Some res); res) in
      let (_, e) = loop (length xs) (length ys) in List.rev e
    let index =
      function | Ins (i, _) -> i | Del (i, _) -> i | Rep (i, _, _) -> i
    let shift_edit o =
      function
      | Ins (i, x) -> Ins ((i + o), x)
      | Del (i, x) -> Del ((i + o), x)
      | Rep (i, x, x') -> Rep ((i + o), x, x')
    let rec shift_patch acc o =
      function
      | [] -> List.rev acc
      | e::tl -> shift_patch ((shift_edit o e) :: acc) o tl
    let offset = function | Ins _ -> 1 | Del _ -> (-1) | Rep _ -> 0
    let op_transform p q =
      let cons2 (x, y) (xs, ys) = ((x :: xs), (y :: ys)) in
      let rec go xs a ys b =
        match (xs, a, ys, b) with
        | ([], _, [], _) -> ([], [])
        | (xs, a, [], _) -> ((shift_patch [] a xs), [])
        | ([], _, ys, b) -> ([], (shift_patch [] b ys))
        | (x::xs, a, y::ys, b) ->
            if (index x) < (index y)
            then
              let (p', q') = go xs a (y :: ys) (b + (offset x)) in
              (((shift_edit a x) :: p'), q')
            else
              if (index x) > (index y)
              then
                (let (p', q') = go (x :: xs) (a + (offset y)) ys b in
                 (p', ((shift_edit b y) :: q')))
              else
                (match (x, y) with
                 | _ when x = y -> go xs (a + (offset y)) ys (b + (offset x))
                 | (Ins (i, nx), Ins (_, ny)) ->
                     let n = Atom.resolve nx ny in
                     cons2 ((Rep ((i + a), ny, n)), (Rep ((i + b), nx, n)))
                       (go xs (a + (offset y)) ys (b + (offset x)))
                 | (Rep (i, anc, nx), Rep (_, _, ny)) ->
                     let n = Atom.merge3 ~ancestor:anc nx ny in
                     cons2 ((Rep ((i + a), ny, n)), (Rep ((i + b), nx, n)))
                       (go xs a ys b)
                 | (Ins _, _) ->
                     let (p', q') = go xs a (y :: ys) (b + (offset x)) in
                     (((shift_edit a x) :: p'), q')
                 | (_, Ins _) ->
                     let (p', q') = go (x :: xs) (a + (offset y)) ys b in
                     (p', ((shift_edit b y) :: q'))
                 | (Rep (i, _, nx), Del _) ->
                     let (p', q') = go xs (a + (offset y)) ys b in
                     (p', ((Del ((i + b), nx)) :: q'))
                 | (Del _, Rep (i, _, ny)) ->
                     let (p', q') = go xs a ys (b + (offset x)) in
                     (((Del ((i + a), ny)) :: p'), q')
                 | (Del _, Del _) ->
                     go xs (a + (offset y)) ys (b + (offset x))) in
      go p 0 q 0
    let rec apply off s =
      function
      | [] -> s
      | (Ins (pos, c))::tl ->
          let s' = insert s (pos + off) c in apply (off + 1) s' tl
      | (Rep (pos, _, x'))::tl ->
          let s' = set s (pos + off) x' in apply off s' tl
      | (Del (pos, _))::tl ->
          let s' = delete s (pos + off) in apply (off - 1) s' tl
    let apply s =
      try apply 0 s
      with
      | Invalid_argument _ -> raise (Invalid_argument "incompatible patch")
    let merge3 ~ancestor  l r =
      let p = op_diff ancestor l in
      let q = op_diff ancestor r in
      let (_, q') = op_transform p q in apply l q'
  end[@@derive_versioned ]
module IList =
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
        module OM = List(Atom)
        open OM
        type madt = OM.atom list
        module IrminConvert = struct let () = ()
                                     let () = () end
        module IrminConvertTie =
          struct
            let atom = let open Irmin.Type in list Atom.t
            let madt = let open Irmin.Type in list Atom.t
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
              ((add t) =<< (Lwt.return @@ a) : K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in Lwt.return @@ a) : 
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
                     Lwt.return @@ a) : t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     Lwt.return @@ t) : OM.t Lwt.t)
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
              Lwt.return () >>= (fun () -> Store.set t p v ~info:(info msg))
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
