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
module MakeVersioned(Config:Config)(Atom:Set_imp.ATOM) =
struct
module OM = Set_imp.Make(Atom)
open OM
module K = Irmin.Hash.SHA1
module G = Git_unix.FS
type adt = OM.t
  type node = {
          l: K.t ;
          v: Atom.t ;
          r: K.t ;
          h: int64 }
        and madt =
          | Empty 
          | Node of node
  type t = 
    | Me of madt

    type boxed_t = t

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
          let t = 
    let open Irmin.Type in
    variant "t" (fun m -> function
        | Me a  -> m a)
    |~ case1 "Me" IrminConvertTie.madt (fun x -> Me x)
    |> sealv
  module AO_value : (Irmin.Contents.Conv with type  t =  t) =
  struct
    type t = boxed_t
    let t = t
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


  module type MY_TREE = TAG_TREE with type value=t

  module AO_store  =
  struct
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S
    type adt = OM.t

    let create config =
      let level =
        Irmin.Private.Conf.key ~doc:"The Zlib compression level."
          "level" (let open Irmin.Private.Conf in some int) None in
      let root =
        Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      G.create ?root ?level ()

    let create () = create @@ Irmin_git.config Config.root

    (*let on_add = ref (fun k v -> printf "%s\n" 
                                   (Fmt.to_to_string K.pp k); 
                                 Lwt.return ())*)

    let add_and_link (type a) (module T:MY_TREE with type t=a) 
                     t v (tree:a) : (K.t*a) Lwt.t=
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
      Lwt.return (k,tree')

    module PHashtbl = Hashtbl.Make(struct 
        type t = adt
        let equal x y = x == y
        let hash x = Hashtbl.hash_param 2 10 x
      end)

    let (read_cache: (K.t, adt) Hashtbl.t) = Hashtbl.create 5051

    let (write_cache: (string, K.t) Hashtbl.t) = Hashtbl.create 5051

     let int64_addr (x:adt) :int64 = Obj.magic x

    let string_addr (x:adt) : string = Obj.magic x
        
    let rec add_adt : type a. (module MY_TREE with type t=a) -> t 
                                -> adt -> (a -> (K.t*a) Lwt.t) =
    fun  (module T) t (adt:adt) ->
      (*
       * We momentarily override Lwt's bind and return so as to pass
       * the tree around without making a mess.
       *)
let add_to_store (v:madt) = 
          fun tr ->
          add_and_link (module T:MY_TREE with type t = a) t (Me v) tr >>= fun (k,tr') ->
          Lwt.return (k, tr') in 
      let add_adt = add_adt (module T:MY_TREE with type t = a) t in
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
           | OM.Empty -> (add_to_store Empty >>= fun k ->
              Hashtbl.add write_cache (string_addr adt) k;
              return k)
           | OM.Node {l;v;r;h} -> 
             (add_adt l >>= fun l' ->
              add_adt r >>= fun r' ->
              add_to_store (Node {l=l'; v; r=r'; h}) >>= fun k ->
               Hashtbl.add write_cache (string_addr adt) k;
              return k)
      end

    let rec read_adt t (k:K.t) : OM.t Lwt.t =
      try 
        Lwt.return @@ Hashtbl.find read_cache k
      with Not_found -> begin 
        find t k >>= fun aop ->
        let a = from_just aop "to_adt" in
        match a with
          | Me Empty -> Lwt.return @@ OM.Empty 
          | Me (Node {l;v;r;h}) ->
            (read_adt t l >>= fun l' ->
             read_adt t r >>= fun r' ->
             Lwt.return @@ OM.Node {OM.l=l'; OM.v=v; OM.r=r'; OM.h=h})
      end
  end



  let merge_time = ref 0.0
  let merge_count = ref 0
  let _name = ref "Anon"

  module BC_value =
  struct
    include AO_value

    type adt = OM.t

    let of_adt : type a. (module TAG_TREE with type value=t and type t = a) -> adt
                         -> a -> (t*a) Lwt.t = fun (module T) adt ->
     (*
      * Momentarily overriding Lwt's bind and return with our own
      * bind and return to pass around the tree.
      *)
     let return x = fun tr -> Lwt.return (x,tr) in
     let lift m = fun tr -> m >>= fun x -> Lwt.return (x,tr) in
     let (>>=) m f = 
       fun tr -> m tr >>= fun (k,tr') -> f k tr' in
     lift (AO_store.create ()) >>= fun ao_store -> 
     let aostore_add =
       AO_store.add_adt (module T) ao_store in
     match adt with
            | OM.Empty -> return @@ Me Empty
          | OM.Node {l;v;r; h} -> 
            (aostore_add l >>= fun l' ->
             aostore_add r >>= fun r' ->
             return @@ Me (Node {l=l';v; r=r';h}))

    let to_adt (t:t) : adt Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
       match t with
           | Me Empty -> Lwt.return @@ OM.Empty
           | Me (Node {l;v;r;h}) ->
             (aostore_read l >>= fun l' ->
              aostore_read r >>= fun r' ->
              Lwt.return @@ OM.Node {OM.l=l'; OM.v = v; OM.r=r'; OM.h = h})

    (*let rec merge ~old:(old : t Irmin.Merge.promise)  (v1 : t)
      (v2 : t) =
      if v1 = v2 then Irmin.Merge.ok v1
      else
        begin 
          let t1 = Sys.time () in
          let open Irmin.Merge.Infix in
          let _ = printf "Merge called\n" in
          let _ = flush_all() in
          let merged_v = ref v1 in (* Hack! see below. *)
          old() >>=* fun old ->
          to_adt (from_just old "merge") >>= fun oldv ->
          to_adt v1 >>= fun v1 ->
          to_adt v2 >>= fun v2 ->
          let v = OM.merge3 oldv v1 v2 in
          BC_store.init () >>= fun repo ->
          BC_store.master repo >>= fun t ->
          BC_store.with_tree t ["state"]
            ~info:(BC_store.info "Mergefn")
            begin fun trop ->
              let tr = from_just trop "merge.trop" in
              let tmod = (module BC_store.Tree : 
                           MY_TREE with type t = BC_store.tree) in
              of_adt tmod v tr >>= fun (v',tr') ->
              let _ = merged_v := v' in
              Lwt.return @@ Some tr'
            end >>= fun () ->
          let t2 = Sys.time () in
          let _ = merge_time := !merge_time +. (t2-.t1) in
          let _ = merge_count := !merge_count + 1 in
          Irmin.Merge.ok !merged_v
        end*)

        let merge ~old v1 v2 = failwith "Unimpl."

    let merge = let open Irmin.Merge in option (v t merge)
  end

  module BC_store : IRMIN_STORE with type value = t =
    struct
      module Store = Irmin_unix.Git.FS.KV(BC_value)
      module Sync = Irmin.Sync(Store)
  
      module Tree = 
        struct
          type t = Store.tree

          type tag = string list

          type value = boxed_t

          let empty () = Store.Tree.empty

          let set_prefix p = 
          failwith "Irbmap.BC_store.Tree.set_prefix Unimpl."

          let tag_of_string str = [str]

          let tag_of_hash k = 
            let sha_str = Fmt.to_to_string Irmin.Hash.SHA1.pp k in
            let fname_k = String.sub sha_str 0 10 in
              [fname_k]

          let add t k v = Store.Tree.add t k v
        end

      type t = Store.t
      type repo = Store.repo
      type tree = Store.tree
      type path = string list
      type value = boxed_t

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

      let with_tree t path ~info f = Store.with_tree t path f
                                      ~info:info
                                      ~strategy:`Set

      let rec update ?msg  t (p : path) (v : BC_value.t) =
        let msg =
          match msg with
          | Some s -> s
          | None -> "Setting " ^ (string_of_path p) in
        Store.set t p v ~info:(info msg)

      
    end

  module type VPST =
    sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val with_init_version_do : OM.t -> 'a t -> 'a
      val with_remote_version_do : string -> 'a t -> 'a
      (*val fork_version : 'a t -> unit t*)
      val get_latest_version : unit -> OM.t t
      val sync_next_version : ?v:OM.t -> string list -> OM.t t
      val liftLwt : 'a Lwt.t -> 'a t
      val pull_remote : string -> unit t
    end 

    module Vpst : VPST =
    struct
      type store = BC_store.t
      type st =
        {
        master: store ;
        name: string ;
        next_id: int ;
        seq_no: int}

      type 'a t = st -> ('a * st) Lwt.t

      let info s = Irmin_unix.info "[repo %s] %s" Config.root s

      let path = ["state"]

      let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

      let bind (m1 : 'a t) (f : 'a -> 'b t) =
        (fun st -> (m1 st) >>= (fun (a, st') -> f a st') : 'b t)

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
            let tmod = 
            (module Tree : MY_TREE 
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
      OM.t t)

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
