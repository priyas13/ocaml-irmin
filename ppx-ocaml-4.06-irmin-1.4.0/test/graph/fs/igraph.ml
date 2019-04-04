open Lwt.Infix
open Irmin_unix
open Printf
open Msigs

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module OG = Graph_imp.Make

module OS = OG.OS

module type Config  =
sig val root : string val shared : string val init : unit -> unit end

module MakeVersioned (Config: CONFIG) 
                     (Atom:Graph_imp.ATOM)
                      (V: IRMIN_DATA_STRUCTURE
                         with type adt = OS.t) :
                         IRMIN_DATA_STRUCTURE with type adt = OG.t = 
struct

module OS = OG.OS
open OG
open OS
module ISet = Iset_imp.MakeVersioned(Config)(Graph_imp.Edge_type)
open ISet
module K = Irmin.Hash.SHA1
module G = Git_unix.Mem
   type adt = OG.t 
   type nlabel = string
   
   type elabel = string

   type node = int64

   type lnode = (node * nlabel)

   type edge = (node * node)

   type ledge = (edge * elabel)

   type in_edge = V.t

   type out_edge = V.t

   type context = (in_edge * (node * (nlabel * out_edge)))

   type gnode = (context * K.t)
    
   and gmadt = 
           | E_G 
           | G of gnode
  type t = 
    | Me of gmadt
    | Child of V.t

  type boxed_t = t

    let nlabel = let open Irmin.Type in string 
      let elabel = let open Irmin.Type in string 
      let node = let open Irmin.Type in int64
      let lnode = let open Irmin.Type in pair node nlabel 
      let edge = let open Irmin.Type in pair node node 
      let ledge = let open Irmin.Type in pair edge elabel
      let (in_edge : in_edge Irmin.Type.t) = let open Irmin.Type in V.t
      let (out_edge : out_edge Irmin.Type.t) = let open Irmin.Type in V.t
      let context = let open Irmin.Type in pair in_edge (pair node (pair nlabel in_edge))
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
      let gmadt,gnode = let open Irmin.Type in mu2 (fun gmadt gnode -> mkgmadt gnode, mkgnode gmadt)

  let t = 
    let open Irmin.Type in
    variant "t" (fun m c -> function
        | Me a  -> m a
        | Child a -> c a)
    |~ case1 "Me" gmadt (fun x -> Me x)
    |~ case1 "Child" V.t (fun x -> Child x)
    |> sealv

    module type TAG_TREE = sig
  type t
  type tag
  type value
  val tag_of_string: string -> tag
  val tag_of_hash: K.t -> tag
  val empty: unit -> t
  val add: t -> tag -> value -> t Lwt.t
  val set_prefix: string list -> unit
end

module type IRMIN_DATA_STRUCTURE = sig
  type adt
  (*module BC_value: IRMIN_STORE_VALUE with type t=t 
                                      and type adt = adt*)
  include Irmin.Contents.S
  val of_adt : (module TAG_TREE 
                 with type t='a 
                  and type value=t) 
              -> adt -> 'a -> (t*'a) Lwt.t
  val to_adt: t -> adt Lwt.t
end

  module AO_value : Irmin.Contents.Conv with type t = t = struct
    type t = boxed_t

    let t = t

    let pp = Irmin.Type.pp_json ~minify:false t
  
    let of_string s =
      let decoder = Jsonm.decoder (`String s) in
      let res = try Irmin.Type.decode_json t decoder 
                with Invalid_argument s -> 
                  (failwith @@ sprintf "AO_Value.of_string:\
                    \ Invalid_argument: %s" s) in
      let _ = match res with
        | Ok _ -> ()
        | Error (`Msg str) -> 
          (printf "Decoding error: %s\n" str;
          printf "While decoding: %s\n" s) in 
      res
  end

  module type MY_TREE = TAG_TREE with type value=t

  module type V_TREE = TAG_TREE with type value=V.t

  let my_tree_to_v_tree : type a b. (module MY_TREE with type t=a 
                                                     and type tag=b)
                                -> (module V_TREE with type t=a 
                                                   and type tag=b) =
    fun (module T) ->
      let module Vtree = struct
          type t = T.t
          type tag = T.tag
          type value = V.t
          let tag_of_string = T.tag_of_string
          let tag_of_hash = T.tag_of_hash
          let empty = T.empty
          let set_prefix = T.set_prefix
          let add t tag vt = 
            T.add t tag (Child vt)
        end in
      (module Vtree: V_TREE with type t=T.t 
                             and type tag=T.tag)

  module AO_store : AO_STORE with type adt=adt 
                              and type value=t = struct
    (* Immutable collection of all versionedt *)
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
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
      Lwt.return (k,tree')


    (*module PHashtbl = struct
      include Hashtbl.Make(struct 
        type t = adt
        let addr (o:adt) : int64 = 
          let _ = printf "magic enter\n" in
          let _ = flush_all () in
          let addr = Obj.magic o in
          let _ = printf "magic exit\n" in
          let _ = flush_all () in
          addr
        let equal x y = Int64.equal (addr x) (addr y)
        let hash x = Hashtbl.hash (addr x)
      end)
      let find t key = 
        let _ = printf "PHashtbl.find enter\n" in
        let _ = flush_all () in
        let v = find t key in
        let _ = printf "PHashtbl.find exit\n" in
        let _ = flush_all () in
        v
    end*)

    let (read_cache: (K.t, adt) Hashtbl.t) = Hashtbl.create 131072

    let (write_cache: (string, K.t) Hashtbl.t) = Hashtbl.create 131072
        
    let int64_addr (x:adt) :int64 = Obj.magic x

    let string_addr (x:adt) : string = Obj.magic x

    let rec thread_f () =
      Lwt_io.printf "Cache sizes: %d (read), %d (write)\n" 
        (Hashtbl.length read_cache) 
        (Hashtbl.length write_cache) >>= fun _ ->
      Lwt_unix.sleep 120.0 >>= fun () ->
      thread_f ()

    let _ = Lwt.async thread_f

    let rec add_adt : type a. (module MY_TREE with type t=a) ->
             t -> adt -> (a -> (K.t*a) Lwt.t) =
      fun  (module T) t (adt:adt) ->
        let vtree = my_tree_to_v_tree (module T) in
        let module Vtree = (val vtree : V_TREE with type t=T.t 
                                                and type tag=T.tag) in
        let add_to_store (v:gmadt) = 
          fun tr ->
            add_and_link (module T:MY_TREE with type t = a) 
                         t (Me v) tr >>= fun (k,tr') -> 
            Lwt.return (k,tr') in
        let of_vadt vadt = fun tr ->
          V.of_adt (module Vtree: V_TREE with type t=T.t) vadt tr in
        let add_adt = add_adt (module T:MY_TREE with type t = a) t in
        (*
         * We momentarily override Lwt's bind and return so as to pass
         * the tree around without making a mess.
         *)
        let (>>=) m f = fun tr -> 
          m tr >>= fun (k,tr') -> f k tr' in
        let return x = fun tr -> Lwt.return (x,tr) in
        try
          let k = Hashtbl.find write_cache (string_addr adt) in
          let adt' = Hashtbl.find read_cache k in
          if string_addr adt <> string_addr adt' 
          then raise Not_found
          else return k
        with Not_found ->
          begin match adt with
           | OG.E_G -> 
             (add_to_store E_G >>= fun k ->
              Hashtbl.add write_cache (string_addr adt) k;
            return k)
          | OG.G ((ie, n, l, oe), g) ->
            (add_adt g >>= fun g' -> 
             of_vadt ie >>= fun ie' ->
             of_vadt oe >>= fun oe' ->
             add_to_store 
               (G ((ie', (n, (l, oe'))), g')) >>= fun k -> 
               Hashtbl.add write_cache (string_addr adt) k;
              return k)
        end

    let rec read_adt t (k:K.t) : adt Lwt.t =
      try 
        Lwt.return @@ Hashtbl.find read_cache k
      with Not_found -> begin 
        find t k >>= fun aop ->
        let to_vadt v = V.to_adt v in
        let a = from_just aop "to_adt" in
        (match a with
          | Me (G ((ie, (n, (l, oe))), g)) ->
            (read_adt t g >>= fun g' ->
             to_vadt ie >>= fun ie' ->
             to_vadt oe >>= fun oe'->
             let adt = OG.G((ie', n, l, oe'), g') in
             let _ = Hashtbl.add read_cache k adt in
             Lwt.return adt)
          | Me E_G -> 
            (let adt = OG.E_G in
             let _ = Hashtbl.add read_cache k adt in
             Lwt.return adt)
          | Child _ -> failwith "read_adt.Exhaustiveness") 
      end

    let find_or_fail t (k:K.t) : AO_value.t Lwt.t =
      find t k >>= fun vop ->
      Lwt.return @@ from_just vop "find_or_fail"
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
                                          and type adt=adt = struct
    include AO_value

    type adt=OG.t
    
    let of_adt : type a. (module MY_TREE with type t = a) -> adt
                         -> (a) -> (t*a) Lwt.t = 
      fun (module T) adt ->
        let vtree = my_tree_to_v_tree (module T) in
        let module Vtree = (val vtree : V_TREE with type t=T.t 
                                          and type tag=T.tag) in
        let of_vadt vadt = fun tr ->
          V.of_adt (module Vtree) vadt tr >>= fun (v,tr') ->
          Lwt.return (v,tr') in
        (*
         * Momentarily overriding Lwt's bind and return with our own
         * bind and return to pass around the tree.
         *)
        let return x = fun tr -> Lwt.return (x,tr) in
        let lift m = 
          fun tr -> m >>= fun x -> 
                    Lwt.return (x,tr) in
        let (>>=) m f = fun (tr) -> 
            m tr >>= fun (k,tr') -> f k tr' in
        lift (AO_store.create ()) >>= fun ao_store -> 
        let aostore_add =
          AO_store.add_adt (module T) ao_store in
        match adt with
         | OG.G ((ie,n,l,oe), g) -> 
           (aostore_add g >>= fun g' ->
            of_vadt ie >>= fun ie' ->
            of_vadt oe >>= fun oe' ->
            return @@ Me (G ((ie',(n,(l,(oe')))), g')))
         | OG.E_G -> return @@ Me E_G

    let to_adt (t:t) : adt Lwt.t =
      let to_vadt v = V.to_adt v in
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | Me (G ((ie,(n,(l,oe))), g)) ->
          (aostore_read g >>= fun g' ->
           to_vadt ie >>= fun ie' ->
           to_vadt oe >>= fun oe' ->
           Lwt.return @@ OG.G ((ie',n,l,oe'), g'))
        | Me E_G -> Lwt.return @@ OG.E_G
        | Child _ -> failwith "to_adt.exhaustiveness"


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

    module Tree = 
      struct
        type t = Store.tree

        type tag = string list

        type value = boxed_t

        let empty () = Store.Tree.empty

        let tag_of_string str = [str]

        let set_prefix p = 
          failwith "Irbmap.BC_store.Tree.set_prefix Unimpl."

        let tag_of_hash k = 
          let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
          let fname_k = String.sub sha_str 0 10 in
            [fname_k]

        let add t k v = Store.Tree.add t k v
      end

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    (*let update t k v = Store.set t k v*)

    let read t (p:path) = 
      Store.find t p (*>>= fun vop ->
      Lwt.return (match vop with
      | None -> None
      | Some (Me m) -> Some m
      | Some (Child _) -> failwith "BC_store.read.exhaustiveness")*)

    let string_of_path p = String.concat "/" p

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s

    let with_tree t path ~info f = Store.with_tree t path f
                                    ~info:info
                                    ~strategy:`Set

    let status t = Store.status t

    let rec update ?msg t (p:path) (v:boxed_t) = 
      let msg = match msg with
        | Some s -> s
        | None -> "Setting "^(string_of_path p) in
      Store.set t p v ~info:(info msg)
  end

  (*
   * The following to make rbmap an irmin data structure
   *)
  let of_adt = BC_value.of_adt

  let to_adt = BC_value.to_adt

  let merge = BC_value.merge

  let of_string = AO_value.of_string

  let pp = AO_value.pp


  module type VPST = sig
    type 'a t
    val return : 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val with_init_version_do: OG.t -> 'a t -> 'a 
    val with_remote_version_do: string -> 'a t -> 'a
    val get_latest_version: unit -> OG.t t
    val sync_next_version: ?v:OG.t -> string list -> OG.t t
    val liftLwt: 'a Lwt.t -> 'a t
    val pull_remote: string -> unit t
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
      OG.t t)

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

    let sync_next_version ?v (uris:string list) = fun (st:st) ->
      try
        (* 1. Commit to the master branch *)
        (match v with 
         | None -> Lwt.return ()
         | Some v -> 
           BC_store.with_tree st.master ["state"]
             ~info:(info @@
                    sprintf "%d. setting latest version" st.seq_no)
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
      with _ -> failwith "Some error occured"
        
    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)
  end

end
