[%%dali_imodstr_rename module IAdt = struct
  open Lwt.Infix
  open Irmin_unix
  module K = Irmin.Hash.SHA1
  module G = Git_unix.FS
  module type Config = 
  sig
     val root: string
     val shared: string
     val init: unit -> unit
  end
  
 let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

 [%%dali_mergeable_functs module MakeVersioned (Config: Config) (Atom : ATOM)  = struct
  module OM = [%dali_adt_mod_func]
  open OM

  [%%dali_madt_typedef]

    [%%dali_irmin_convert]

          [%%dali_irmin_tie_convert]

         module AO_value : Irmin.Contents.Conv with type t = madt = struct
          type t = madt
          (* This function will create the ppx type extension for Irmin *)
          (*[%%dali_irmin_convert]*)
          let t = IrminConvertTie.madt

          let pp = Irmin.Type.pp_json ~minify:false t
    
          let of_string s =
            let decoder = Jsonm.decoder (`String s) in
            let res = try Irmin.Type.decode_json t decoder 
                with Invalid_argument s -> 
                  (failwith @@ Printf.sprintf "AO_Value.of_string:\
                    \ Invalid_argument: %s" s) in
              res

        end

        module AO_store = struct
          module S = Irmin_git.AO(Git_unix.FS)(AO_value)
          include S

          let create config =
            let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
                "level" Irmin.Private.Conf.(some int) None
            in
            let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
            let level = Irmin.Private.Conf.get config level in
            G.create ?root ?level ()

          (* Somehow pulls the config set by Store.init *)
          (* And creates a Git backend *)
          let create () = create @@ Irmin_git.config Config.root

          let on_add = ref (fun k v -> Lwt.return ())

          let add t v = 
             S.add t v >>= fun k ->
             (!on_add) k v >>= fun _ ->
             Lwt.return k

          let rec add_adt t (a:[%dali_adt_typesig_func]) :  K.t Lwt.t = 
             add t =<< [%dali_add_adt]


          let rec read_adt t (k: K.t) : [%dali_adt_typesig_func] Lwt.t = 
            find t k >>= fun aop ->
            let a = from_just aop "to_adt" in 
            [%dali_read_adt]

        end
        
        module type IRMIN_STORE_VALUE = sig
          include Irmin.Contents.S
          val of_adt: [%dali_adt_typesig_func] -> t Lwt.t
         val to_adt: t -> [%dali_adt_typesig_func] Lwt.t
        end

         module BC_value: IRMIN_STORE_VALUE with type t = madt = struct
            include AO_value


        let of_adt (a:[%dali_adt_typesig_func]) : t Lwt.t  =
      AO_store.create () >>= fun ao_store -> 
      let aostore_add adt =
        AO_store.add_adt ao_store adt in
     [%dali_of_adt]

    let to_adt (t:t) : [%dali_adt_typesig_func] Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      [%dali_to_adt]

    let rec merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
      let open Irmin.Merge.Infix in
      old () >>=* fun old ->
      to_adt (from_just old "merge") >>= fun oldv  ->
      to_adt v1 >>= fun v1  ->
      to_adt v2 >>= fun v2 ->
      let v = OM.merge3 oldv v1 v2 in
      of_adt v >>= fun merged_v ->
      Irmin.Merge.ok merged_v

    let merge = Irmin.Merge.(option (v t merge))
  end

  [%%dali_mergeable_functs module BC_store = struct
    module Store = Irmin_unix.Git.FS.KV(BC_value)
    module Sync = Irmin.Sync(Store)

    type t = Store.t (* = branch *)

    type path = string list

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    (*let update t k v = Store.set t k v*)

    let read t (p:path) = Store.find t p

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let rec update ?msg t (p:path) (v:BC_value.t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      let fname_of_hash hsh = 
        String.sub (Fmt.to_to_string Irmin.Hash.SHA1.pp hsh) 0 7 in
      let link_to_tree k = 
        AO_store.create () >>= fun ao_store ->
        AO_store.find ao_store k >>= fun vop ->
        let v_k = from_just vop "BC_store.update" in
        let path_k = [fname_of_hash k] in
        update t path_k v_k in [%dali_update_adt] >>= fun () ->
        Store.set t p v ~info:(info msg)
  end]

    [%%dali_mergeable_functs module Vpst : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: [%dali_adt_typesig_func] -> 'a t -> 'a
    val with_remote_version_do: string -> 'a t -> 'a
    val fork_version : 'a t -> unit t
    val get_latest_version: unit -> [%dali_adt_typesig_func] t
    val sync_next_version: ?v:[%dali_adt_typesig_func] -> [%dali_adt_typesig_func] t
    val liftLwt : 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
  end = struct
    type store = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : store;
               local    : store;
               name     : string;
               next_id  : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')

    let with_init_version_do (v: [%dali_adt_typesig_func]) (m: 'a t) =
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          BC_value.of_adt v >>= fun (v':BC_value.t) ->
          BC_store.update ~msg:"initial version" 
                          m_br path v' >>= fun () ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

    let fork_version (m: 'a t) : unit t = fun (st: st) ->
      let thread_f () = 
        let child_name = st.name^"_"^(string_of_int st.next_id) in
        let parent_m_br = st.master in
        (* Ideally, the following has to happen: *)
        (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
        (* But, we currently default to an SC mode. Master is global. *)
        let m_br = parent_m_br in
        BC_store.clone m_br (child_name^"_local") >>= fun t_br ->
        let new_st = {master = m_br; local  = t_br; 
                      name = child_name; next_id = 1} in
        m new_st in
      begin
        Lwt.async thread_f;
        Lwt.return ((), {st with next_id=st.next_id+1})
      end 

    let get_latest_version () : [%dali_adt_typesig_func] t = fun (st: st) ->
      BC_store.read st.local path >>= fun (vop:BC_value.t option) ->
      let v = from_just vop "get_latest_version"  in
      BC_value.to_adt v >>= fun td ->
      Lwt.return (td,st)

    let pull_remote remote_uri = fun (st: st) ->
      (* Pull and merge remote to master *)
      let cinfo = info (Printf.sprintf "Merging remote(%s) to local master" 
                          remote_uri) in
      let remote = Irmin.remote_uri remote_uri in
      BC_store.Sync.pull st.master remote 
                            (`Merge  cinfo) >>= fun res -> 
      (match res with
          | Ok _ -> Lwt.return ((),st)
          | Error _ -> failwith "Error while pulling the remote")

       let with_remote_version_do remote_uri m = 
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          let remote = Irmin.remote_uri remote_uri in
          BC_store.Sync.pull m_br remote `Set >>= fun res ->
          (match res with
              | Ok _ -> Lwt.return ()
              | Error _ -> failwith "Error while pulling the remote") >>= fun _ ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

   let sync_next_version ?v : [%dali_adt_typesig_func] t = fun (st: st) ->
      (* How do you commit the next version? Simply update path?
       * GK:Yes *)
      (* 1. Commit to the local branch *)
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         BC_value.of_adt v >>= fun v' -> 
         BC_store.update ~msg:"Committing local state" 
                         st.local path v') >>= fun () ->
      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->

      m >>= fun a -> Lwt.return (a,st)
end]
end]
end]

    