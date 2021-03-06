open Lwt.Infix
open Irmin_unix
open Printf

module OM = Canvas.Make
module K = Irmin.Hash.SHA1

module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: Config)  = struct

  type pixel = {r:char; g:char; b:char}

  type node = {tl_t:K.t; tr_t:K.t; bl_t:K.t; br_t:K.t}

  type t = 
   | N of pixel 
   | B of node

  module AO_value : Irmin.Contents.Conv with type t = t = struct
    type vt = t
    type t = vt
  
    let pixel = 
      let open Irmin.Type in
      record "pixel" (fun r g b -> {r; g; b})
      |+ field "r" char (fun t -> t.r)
      |+ field "g" char (fun t -> t.g)
      |+ field "b" char (fun t -> t.b)
      |> sealr


    let node = 
      let open Irmin.Type in
      record "node" (fun tl_t tr_t bl_t br_t -> {tl_t;tr_t;bl_t;br_t})
      |+ field "tl_t" K.t (fun t -> t.tl_t)
      |+ field "tr_t" K.t (fun t -> t.tr_t)
      |+ field "bl_t" K.t (fun t -> t.bl_t)
      |+ field "br_t" K.t (fun t -> t.br_t)
      |> sealr


    let t =
      let open Irmin.Type in
      variant "t" (fun vp np -> function
          | N v -> vp v  
          | B n -> np n)
      |~ case1 "N" pixel (fun x -> N x)
      |~ case1 "B" node (fun x -> B x)
      |> sealv

    let pp = Irmin.Type.pp_json ~minify:false t
  
    let of_string s =
      let decoder = Jsonm.decoder (`String s) in
      let res = try Irmin.Type.decode_json t decoder 
                with Invalid_argument s -> 
                  (failwith @@ sprintf "AO_Value.of_string:\
                    \ Invalid_argument: %s" s) in
      res
  end

  module AO_store = struct
    (* Immutable collection of all versionedt *)
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      Git_unix.FS.create ?root ?level ()

    (* Creates a Git backend *)
    let create () = create @@ Irmin_git.config Config.root

    let on_add = ref (fun k v -> Lwt.return ())

    let add t v = 
      S.add t v >>= fun k ->
      (!on_add) k v >>= fun _ ->
      Lwt.return k

  (*
   * We can memoize the results of add_adt and read_adt for faster
   * processing. add_adt can use physicaly equality on OM.t objects
   * for faster lookups. read_adt memoization is straightforward.
   *)
  let rec add_adt t (a:OM.t) : K.t Lwt.t =
    add t =<<
      (match a with
       | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
       | OM.B {tl_t;tr_t;bl_t;br_t} -> 
         (add_adt t tl_t >>= fun tl_t' ->
          add_adt t tr_t >>= fun tr_t' ->
          add_adt t bl_t >>= fun bl_t' ->
          add_adt t br_t >>= fun br_t' ->
          Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                           bl_t=bl_t'; br_t=br_t'}))

  let rec read_adt t (k:K.t) : OM.t Lwt.t =
    find t k >>= fun aop ->
    let a = from_just aop "to_adt" in
    match a with
      | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
      | B {tl_t;tr_t;bl_t;br_t} ->
        (read_adt t tl_t >>= fun tl_t' ->
         read_adt t tr_t >>= fun tr_t' ->
         read_adt t bl_t >>= fun bl_t' ->
         read_adt t br_t >>= fun br_t' ->
         Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                             OM.bl_t=bl_t'; OM.br_t=br_t'})
  end

  module type IRMIN_STORE_VALUE = sig
    include Irmin.Contents.S
    val of_adt: OM.t -> t Lwt.t
    val to_adt: t -> OM.t Lwt.t
  end
 
  module BC_value: IRMIN_STORE_VALUE with type t = t = struct
    include AO_value
    
    let of_adt (a:OM.t) : t Lwt.t  =
      AO_store.create () >>= fun ao_store -> 
      let aostore_add adt =
        AO_store.add_adt ao_store adt in
      match a with
       | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
       | OM.B {tl_t;tr_t;bl_t;br_t} -> 
         (aostore_add tl_t >>= fun tl_t' ->
          aostore_add tr_t >>= fun tr_t' ->
          aostore_add bl_t >>= fun bl_t' ->
          aostore_add br_t >>= fun br_t' ->
          Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                           bl_t=bl_t'; br_t=br_t'})

    let to_adt (t:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
        | B {tl_t;tr_t;bl_t;br_t} ->
          (aostore_read tl_t >>= fun tl_t' ->
           aostore_read tr_t >>= fun tr_t' ->
           aostore_read bl_t >>= fun bl_t' ->
           aostore_read br_t >>= fun br_t' ->
           Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                               OM.bl_t=bl_t'; OM.br_t=br_t'})

    let rec merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
      let open Irmin.Merge.Infix in
      old () >>=* fun old ->
      to_adt (from_just old "merge") >>= fun oldv  ->
      to_adt v1 >>= fun v1  ->
      to_adt v2 >>= fun v2 ->
      let v = OM.merge oldv v1 v2 in
      of_adt v >>= fun merged_v ->
      Irmin.Merge.ok merged_v

    let merge = Irmin.Merge.(option (v t merge))
  end

  module BC_store = struct
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
        update t path_k v_k in
      (match v with
        | N _  -> Lwt.return ()
        | B {tl_t; tr_t; bl_t; br_t} -> 
          List.fold_left  
            (fun m k -> m >>= fun () -> 
                        link_to_tree k) 
            (Lwt.return ())
            [tl_t; tr_t; bl_t; br_t]) >>= fun () ->
      Store.set t p v ~info:(info msg)
  end

  module type VPST = sig
    type 'a t
    val return : 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: OM.t -> 'a t -> 'a 
    val with_remote_version_do: string -> 'a t -> 'a
    val fork_version: 'a t -> unit t
    val get_latest_version: unit -> OM.t t
    val sync_next_version: ?v:OM.t -> OM.t t
    val liftLwt: 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
  end

  module Vpst : VPST = struct
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

    let with_init_version_do (v: OM.t) (m: 'a t) =
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

    let get_latest_version () : OM.t t = fun (st: st) ->
      BC_store.read st.local path >>= fun (vop:BC_value.t option) ->
      let v = from_just vop "get_latest_version"  in
      BC_value.to_adt v >>= fun td ->
      Lwt.return (td,st)

    let pull_remote remote_uri = fun (st: st) ->
      (* Pull and merge remote to master *)
      let cinfo = info (sprintf "Merging remote(%s) to local master" 
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
              | Error _ -> failwith "Error while \
                                     \pulling the remote") >>= fun _ ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end
      (* Fork master from remote master *)

    let sync_next_version ?v : OM.t t = fun (st: st) ->
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
	end 
end
