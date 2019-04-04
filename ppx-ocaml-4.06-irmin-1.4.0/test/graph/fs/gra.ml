open Printf
open Lwt.Infix
open Irmin_unix
open Msigs
module type Config  =
sig val root : string val shared : string val init : unit -> unit end
let from_just op msg =
match op with
| Some x -> x
| None -> failwith @@ (msg ^ ": Expected Some. Got None.")
module MakeVersioned(Config:Config)(Atom:Graph_imp.ATOM) =
struct
module OG = Graph_imp.Make
module OS = OG.OS
open OG
open OS
module ISet = Iset_imp.MakeVersioned(Config)(Graph_imp.Edge_type)
open ISet
module K = Irmin.Hash.SHA1
module G = Git_unix.Mem
   type nlabel = string
   
   type elabel = string

   type node = int64

   type lnode = (node * nlabel)

   type edge = (node * node)

   type ledge = (edge * elabel)

   type in_edge = K.t

   type out_edge = K.t

   type context = (in_edge * (node * (nlabel * out_edge)))

   type gnode = (context * K.t)
    
   and gmadt = 
           | E_G 
           | G of gnode
  module IrminConvert =
    struct
      let nlabel = let open Irmin.Type in string 
      let elabel = let open Irmin.Type in string 
      let node = let open Irmin.Type in int64
      let lnode = let open Irmin.Type in pair node nlabel 
      let edge = let open Irmin.Type in pair node node 
      let ledge = let open Irmin.Type in pair edge elabel
      let (in_edge : in_edge Irmin.Type.t) = let open Irmin.Type in K.t
      let (out_edge : out_edge Irmin.Type.t) = let open Irmin.Type in K.t
      let context = let open Irmin.Type in pair K.t (pair node (pair nlabel K.t))
      let mkgnode t = let open Irmin.Type in pair context K.t
      let mkgmadt gnode =
              let open Irmin.Type in
                (((variant "gmadt"
                     (fun eg ->
                        fun g ->
                          function | E_G -> eg | G a0-> g a0))
                    |~ (case0 "E_G" E_G))
                   |~ (case1 "G" gnode (fun x -> G x)))
                  |> sealv
    end
  module IrminConvertTie =
    struct
     let gmadt,gnode = let open Irmin.Type in mu2 (fun gmadt gnode -> IrminConvert.mkgmadt gnode, IrminConvert.mkgnode gmadt)
    end
  module AO_value : (Irmin.Contents.Conv with type  t =  gmadt) =
  struct
    type t = gmadt
    let t = IrminConvertTie.gmadt
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
  end

  module type TAG_TREE = sig
    type t
    type tag
    type value 
    val tag_of_string: string -> tag
    val tag_of_hash: K.t -> tag
    val empty: unit -> t
    val add: t -> tag -> value -> t Lwt.t
    val set_prefix : string list -> unit
  end


  module type MY_TREE = TAG_TREE with type value = gmadt
  module type ISET_TREE = TAG_TREE with type value = ISet.AO_value.t 
  
  let transform_tree : type a b c. (module MY_TREE with type t=a 
                                                    and type tag=b)
                -> (module IRMIN_DATA_STRUCTURE with type t = c) 
                -> (c -> gmadt)
                -> (module TAG_TREE with type t=a 
                                     and type tag=b 
                                     and type value=c) =
    fun (module T) (module V) f ->
      let module Vtree = struct
          type t = T.t
          type tag = T.tag
          type value = V.t
          let tag_of_string = T.tag_of_string
          let tag_of_hash = T.tag_of_hash
          let empty = T.empty
          let set_prefix = T.set_prefix
          let add t tag vt = 
            T.add t 
            tag 
            (f vt)
        end in
      (module Vtree: TAG_TREE with type t=T.t 
                               and type tag=T.tag 
                               and type value=V.t)


  module AO_store  = struct
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    type adt=OG.t

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      Git_unix.FS.create ?root ?level ()

    (* Creates a Git backend *)
    let create () = create @@ Irmin_git.config Config.root

    let add_and_link (type a) (module T:MY_TREE with type t=a) 
                     t v (tree:a) : (K.t*a) Lwt.t=
      let k = K.digest AO_value.t v in
      S.mem t k >>= fun b ->
      if b then Lwt.return (k,tree)
      else begin
        (S.add t v) >>= fun k ->
        let tag = T.tag_of_hash k in
        T.add tree tag 
        v >>= fun tree' ->
        Lwt.return (k,tree')
      end

  end

  module type IRMIN_STORE_VALUE = sig
  type adt
  include Irmin.Contents.S
  val of_adt : (module TAG_TREE 
                 with type t='a 
                  and type value=t) 
              -> adt -> 'a -> (t*'a) Lwt.t
  val to_adt: t -> adt Lwt.t
end

module type IRMIN_STORE = 
sig
  type t
  type repo
  type path = string list
  type tree
  type value
  module Sync:Irmin.SYNC with type db = t
  module Tree: TAG_TREE with type t=tree and type value=value
  val init : ?root:'a -> ?bare:'b -> unit -> repo Lwt.t
  val master : repo -> t Lwt.t
  val clone : t -> string -> t Lwt.t
  val get_branch : repo -> branch_name:string -> t Lwt.t
  val merge : t ->
    into:t ->
    info:Irmin.Info.f -> (unit, Irmin.Merge.conflict) result Lwt.t
  val read : t -> path -> value option Lwt.t
  val info : string -> Irmin.Info.f
  val update : ?msg:string -> t -> path -> value -> unit Lwt.t
  val with_tree : t -> path -> info:Irmin.Info.f ->
                  (tree option -> tree option Lwt.t) -> unit Lwt.t
end

  module rec BC_value: IRMIN_STORE_VALUE with type t = t 
                                          and type adt=gmadt = struct
    include AO_value

    type adt=OG.t

    let of_vadt: type a b c. 
              (module IRMIN_DATA_STRUCTURE with type adt=a 
                                            and type t = b) 
            -> (module TAG_TREE with type value = b 
                                 and type t = c)
            -> a -> c -> (b*c) Lwt.t =
      fun (module V) (module T) vadt tr ->
        V.of_adt (module T) vadt tr

    let rec of_adt : type a. (module MY_TREE with type t=a) ->
                        (OG.t) -> (a -> (gmadt*a) Lwt.t) =
      fun  (module T) (adt:adt) ->
        (*
         * We momentarily override Lwt's bind and return so as to pass
         * the tree around without making a mess.
         *)
        let (>>=) m f = fun tr -> 
          m tr >>= fun (k,tr') -> f k tr' in
        let return x = fun tr -> Lwt.return (x,tr) in
        begin
          let is_tree = transform_tree (module T) 
                           (module ISet)
                           (fun iwt -> ISet iwt) in
          let module ISetTree = 
              (val is_tree : ISET_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ISet) (module ISetTree)
                  (fst (fst adt)) >>= fun iset ->
          let _ = flush_all() in
           let is_tree = transform_tree (module T) 
                           (module ISet)
                           (fun iwt -> ISet iwt) in
          let module ISetTree = 
              (val is_tree : ISET_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ISet) (module ISetTree)
                  adt >>= fun iset ->
          let _ = flush_all() in
          return @@ G (c, g)
        end

    let madt_to_adt (t:gmadt) : OG.t Lwt.t =
      ISet.to_adt t.warehouse_table >>= fun warehouse_table ->
      let open Graph_imp in
      Lwt.return @@ G(c,g)

    let to_adt (t:t) : OM.db Lwt.t =match t with
      | DB madt -> madt_to_adt madt
      | _ -> failwith "Exhaustiveness in Itpcc.to_adt"


    let merge ~old v1 v2 = failwith "Unimpl."
    let merge = Irmin.Merge.(option (v t merge))
  end

  and BC_store : IRMIN_STORE with type value = t = struct
    module Store = Irmin_unix.Git.FS.KV(BC_value)
    module Sync = Irmin.Sync(Store)
    module Status = Store.Status

    type t = Store.t (* = branch *)
    type repo = Store.repo
    type tree = Store.tree
    type path = string list
    type value = boxed_t

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let rec update ?msg t (p:path) (v:boxed_t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      Store.set t p v ~info:(info msg)

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    module Tree = 
      struct
        type t = Store.tree

        type tag = string list

        type value = boxed_t

        let empty () = Store.Tree.empty

        let prefix = ref []

        let set_prefix p = prefix := p

        let tag_of_string str = (!prefix)@[str]

        let tag_of_hash k = 
          let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
          let fname_k = String.sub sha_str 0 10 in
            tag_of_string fname_k

        let add t k v = 
          if k = ["head"] 
          then Store.Tree.add t k v
          else Lwt.return t
          (*Store.Tree.mem t k >>= fun b ->
          if b then Lwt.return t
          else Store.Tree.add t k v
          Lwt.return t*)
      end

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    (*let update t k v = Store.set t k v*)

    let read t (p:path) = 
      Store.find t p 

    let with_tree t path ~info f = Store.with_tree t path f
                                    ~info:info
                                    ~strategy:`Set

    let status t = Store.status t
  end

  module type VPST = sig
    type 'a t
    val return : 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: OM.db -> 'a t -> 'a 
    val with_remote_version_do: string -> 'a t -> 'a
    val with_persistent_version_do: 'a t -> 'a
    val get_latest_version: unit -> OM.db t
    val sync_next_version: ?v:OM.db -> string list 
                                  -> string -> OM.db t
    val liftLwt: 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
    val get_size_on_disk: unit -> int t
  end

  module Vpst : VPST = struct
    type store = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : store;
               name     : string;
               next_id  : int;
               seq_no    : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')
    
    let with_init_version_do (v : adt) (m : 'a t) =
      Lwt_main.run
      begin 
        BC_store.init () >>= fun repo ->
        BC_store.master repo >>= fun m_br ->
        BC_store.with_tree m_br ["state"]
          ~info:(BC_store.info "Initial version")
          begin fun trop ->
            let module Tree = BC_store.Tree in
            let tr = match trop with 
              | Some tr -> tr
              | None -> Tree.empty () in
            let tmod = (module Tree : MY_TREE 
                         with type t = BC_store.tree) in
            BC_value.of_adt tmod v tr >>= fun (v',tr') ->
            let head_tag = Tree.tag_of_string "head" in
            Tree.add tr' head_tag v' >>= fun tr'' ->
            Lwt.return @@ Some tr''
          end >>= fun () ->
        let st = { master = m_br; name = "1"; 
                   next_id = 1; seq_no = 1 } in
        m st >>= (fun (a, _) -> Lwt.return a)
      end

    let get_latest_version () =
      (fun (st : st) ->
         (BC_store.read st.master (*st.local*) ["state"; "head"]) >>=
           (fun (vop : boxed_t option) ->
              let v = from_just vop "get_latest_version" in
              (BC_value.to_adt v) >>= fun td -> 
              Lwt.return (td, st)) : 
      OM.db t)

    let with_persistent_version_do (m : 'a t) =
      Lwt_main.run
        begin 
          BC_store.init () >>= fun repo ->
          BC_store.master repo >>= fun m_br ->
          (* TODO: name has to be random to avoid collisions *)
          let st = { master = m_br; name = "1"; 
                     next_id = 1; seq_no = 1 } in
          m st >>= (fun (a,_) -> Lwt.return a)
        end

    let pull_remote remote_uri = fun (st: st) ->
      (* Pull and merge remote to master *)
      let cinfo = info (sprintf "Merging remote(%s) to local master" 
                          remote_uri) in
      let remote = Irmin.remote_uri remote_uri in
      let _ = printf "Pulling from %s\n" remote_uri in
      let _ = flush_all () in
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
                                     pulling the remote") >>= fun _ ->
          let st = {master=m_br; name="1"; 
                    next_id=1; seq_no=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end
      (* Fork master from remote master *)

    let sync_next_version ?v uris txn_name = fun (st:st) ->
      (* 1. Commit to the master branch *)
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         begin
           let module Tree = BC_store.Tree in
           let tr = Tree.empty () in
           let _ = Tree.set_prefix ["state"; txn_name] in
           let tmod = (module Tree : MY_TREE 
                        with type t = BC_store.tree) in
           BC_value.of_adt tmod v tr >>= fun (v',tr') ->
           let _ = printf "of_adt done\n" in
           let _ = flush_all () in
           BC_store.update st.master ["state"; "head"] v'
           (*BC_store.with_tree st.master ["state"; txn_name]
             ~info:(info @@
                    sprintf "%d. %s committing" st.seq_no txn_name)
             begin fun trop ->
               let module Tree = BC_store.Tree in
               let tr = match trop with
                 | Some tr -> tr
                 | None -> Tree.empty () in
                let tmod = (module Tree : MY_TREE 
                             with type t = BC_store.tree) in
                BC_value.of_adt tmod v tr >>= fun (v',tr') ->
                let _ = printf "of_adt done\n" in
                let _ = flush_all () in
                let _ = head := Some v' in
                let _ = printf "Returning tree\n" in
                let _ = flush_all () in
                Lwt.return @@ Some tr'
             end >>= fun _ ->
           BC_store.update st.master ["state"; "head"] @@
              from_just !head "Head version" *)
        end) >>= fun () ->
      (* 2. Pull remotes to master *)
      let pull_vpst = List.fold_left 
          (fun (pre: unit t) uri -> 
            bind pre 
                (fun () -> 
                  bind (pull_remote uri) (fun () ->
                  return ()))) 
          (return ()) uris in
      pull_vpst st >>= fun ((),st') ->
      (*
      (* 3. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st'.master ~into:st'.local ~info:cinfo >>= fun _ ->
      (* 4. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st'.local ~into:st'.master ~info:cinfo >>= fun _ ->
      *)
      get_latest_version () {st' with seq_no = st'.seq_no + 1}
        
    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)

    let get_size_on_disk () = fun st ->
        Lwt.return (7000000, st)
  end
 
end