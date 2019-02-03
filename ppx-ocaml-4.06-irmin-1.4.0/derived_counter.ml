module Make =
  struct
    type t = int[@@derive versioned]
    let inc t x = t + x
    let dec t x = t - x
    let merge ~ancestor  v1 v2 =
      (ancestor + (v1 - ancestor)) + (v2 - ancestor)
  end[@@derive_versioned ]
module IMake =
  struct
    open Lwt.Infix
    open Irmin_unix
    module OM = Make
    open OM
    module K = Irmin.Hash.SHA1
    module G = Git_unix.FS
    module type Config  =
      sig val root : string val shared : string val init : unit -> unit end
    let from_just op msg =
      match op with
      | Some x -> x
      | None -> failwith @@ (msg ^ ": Expected Some. Got None.")
    module MakeVersioned(Config:Config) =
      struct
        type madt = int
        module AO_value =
          (struct
             type madt = t
             type t = madt
             module IrminConvert = struct 
                                          let _ = . end
             let pp = Irmin.Type.pp_json ~minify:false t
             let of_string s =
               let decoder = Jsonm.decoder (`String s) in
               let res =
                 try Irmin.Type.decode_json t decoder
                 with
                 | Invalid_argument s ->
                     failwith @@
                       (sprintf "AO_Value.of_string: Invalid_argument: %s" s) in
               res
           end : (Irmin.Contents.Conv with type  t =  t))
        module AO_store =
          struct
            module S = (((Irmin_git.AO)(G))(K))(AO_value)
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
            let rec add_adt t (a : Make.t) =
              ((add t) ==<< (match a with | a0 -> Lwt.return @@ a0) : 
              K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in
                    match a with | a0 -> Lwt.return @@ a0) : Make.t Lwt.t)
          end
        module type IRMIN_STORE_VALUE_MAKE  =
          sig
            include Irmin.Contents.S
            val of_adt : Make.t -> t Lwt.t
            val to_adt : t -> Make.t Lwt.t
          end
        module BC_value =
          (struct
             include AO_value
             let of_adt (a : OM.t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_add adt = AO_store.add_adt ao_store adt in
                     match a with | a0 -> Lwt.return @@ a0) : t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     match t with | a0 -> Lwt.return @@ a0) : OM.t Lwt.t)
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
                                     let v = OM.merge oldv v1 v2 in
                                     (of_adt v) >>=
                                       (fun merged_v ->
                                          Irmin.Merge.ok merged_v)))))
             let merge = let open Irmin.Merge in option (v t merge)
           end : (IRMIN_STORE_VALUE_MAKE with type  t =  t))
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
              ([%dali_update_adt ]) >>=
                (fun () -> Store.set t p v ~info:(info msg))
          end
        module Vpst :
          sig
            type 'a t
            val return : 'a -> 'a t
            val bind : 'a t -> ('a -> 'b t) -> 'b t
            val with_init_version_do : Make.t -> 'a t -> 'a
            val fork_version : 'a t -> unit t
            val get_latest_version : unit -> Make.t t
            val sync_next_version : ?v:Make.t -> Make.t t
            val liftLwt : 'a Lwt.t -> 'a t
            val pull_remote : string -> unit t
            val fork_remote : string -> unit t
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
                  (sprintf "Merging remote(%s) to local master" remote_uri) in
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote (`Merge cinfo)) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
            let fork_remote remote_uri (st : st) =
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote `Set) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
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
