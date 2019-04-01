open Irmin_unix
open Printf
open Msigs
open Rubis
open Lwt.Infix

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: CONFIG)  = struct

  module OM = Rubis
  module K = Irmin.Hash.SHA1

  module VersionedRubis = Versioned_types.Make(Config)

  module Serialization = Versioned_types.Serialization

  open VersionedRubis

  type adt = OM.db

  module IItemTable = Irbmap.MakeVersioned(Config) 
                                (Id)(Item)
                                (ItemTable)(IItem)
  module IBuyerWalletTable = Irbmap.MakeVersioned(Config) 
                                (Id)(BuyerWallet)
                                (BuyerWalletTable)(IBuyerWallet)
  module ISellerWalletTable = Irbmap.MakeVersioned(Config) 
                                (IdPair)(SellerWallet)
                                (SellerWalletTable)(ISellerWallet)
  module IBidTable= Irbmap.MakeVersioned(Config)
                                (IdTriple)(Bid)
                                (BidTable)(IBid)
  module IItemBidTable = Irbmap.MakeVersioned(Config) 
                                (IdPair)(ItemBid)
                                (ItemBidTable)(IItemBid)
  module IWalletBidTable = Irbmap.MakeVersioned(Config) 
                                (IdPair)(WalletBid)
                                (WalletBidTable)(IWalletBid)
  module IWalletItemTable = Irbmap.MakeVersioned(Config) 
                                (IdPair)(WalletItem)
                                (WalletItemTable)(IWalletItem)
  module IPenalityTable = Irbmap.MakeVersioned(Config) 
                                (Id)(Penality)
                                (PenalityTable)(IPenality)


  type madt = {item_table : IItemTable.t; 
               buyerwallet_table: IBuyerWalletTable.t;
               sellerwallet_table: ISellerWalletTable.t;
               bid_table: IBidTable.t;
               itembid_table: IItemBidTable.t;
               walletbid_table: IWalletBidTable.t;
               walletitem_table: IWalletItemTable.t;
               penality_table: IPenalityTable.t}

  type t = 
    | DB of madt
    | IItemTable of IItemTable.t
    | IBuyerWalletTable of IBuyerWalletTable.t 
    | ISellerWalletTable of ISellerWalletTable.t
    | IBidTable of IBidTable.t
    | IItemBidTable of IItemBidTable.t
    | IWalletBidTable of IWalletBidTable.t
    | IWalletItemTable of IWalletItemTable.t
    | IPenalityTable of IPenalityTable.t


  type boxed_t = t

  let (madt : madt Irmin.Type.t) = 
    let open Irmin.Type in 
    record "madt" 
      (fun item_table buyerwallet_table 
           sellerwallet_table bid_table
           itembid_table walletbid_table 
           walletitem_table penality_table ->
           {item_table;
            buyerwallet_table;
            sellerwallet_table;
            bid_table;
            itembid_table;
            walletbid_table;
            walletitem_table;
            penality_table})
    |+ field "item_table" IItemTable.t
              (fun t -> t.item_table)
    |+ field "buyerwallet_table" IBuyerWalletTable.t
              (fun t -> t.buyerwallet_table)
    |+ field "sellerwallet_table" ISellerWalletTable.t
              (fun t -> t.sellerwallet_table)
    |+ field "bid_table" IBidTable.t
              (fun t -> t.bid_table)
    |+ field "itembid_table" IItemBidTable.t
              (fun t -> t.itembid_table)
    |+ field "walletbid_table" IWalletBidTable.t
              (fun t -> t.walletbid_table)
    |+ field "walletitem_table" IWalletItemTable.t
              (fun t -> t.walletitem_table)
    |+ field "penality_table" IPenalityTable.t
              (fun t -> t.penality_table)
    |> sealr

  let (t: t Irmin.Type.t) = 
    let open Irmin.Type in
    variant "t" (fun db i b s bi ib wb wi p -> function
        | DB a  -> db a
        | IItemTable a -> i a
        | IBuyerWalletTable a -> b a
        | ISellerWalletTable a -> s a
        | IBidTable a -> bi a
        | IItemBidTable a -> ib a
        | IWalletBidTable a -> wb a
        | IWalletItemTable a -> wi a
        | IPenalityTable a -> p a)
    |~ case1 "DB" madt (fun x -> DB x)
    |~ case1 "IItemTable" IItemTable.t (fun x -> IItemTable x)
    |~ case1 "IBuyerWalletTable" IBuyerWalletTable.t (fun x -> IBuyerWalletTable x)
    |~ case1 "ISellerWalletTable" ISellerWalletTable.t (fun x -> ISellerWalletTable x)
    |~ case1 "IBidTable" IBidTable.t (fun x -> IBidTable x)
    |~ case1 "IItemBidTable" IItemBidTable.t (fun x -> IItemBidTable x)
    |~ case1 "IWalletBidTable" IWalletBidTable.t (fun x -> IWalletBidTable x)
    |~ case1 "IWalletItemTable" IWalletItemTable.t (fun x -> IWalletItemTable x)
    |~ case1 "IPenalityTable" IPenalityTable.t (fun x -> IPenalityTable x)
    |> sealv

  module AO_value : Irmin.Contents.Conv with type t = t = struct 
    type t = boxed_t

    let t = t

    include Serialization(struct 
                            type t = boxed_t
                            let t = t
                          end)
  end

  module type MY_TREE = TAG_TREE with type value=t
  module type IITEM_TREE = TAG_TREE with type value=IItemTable.t
  module type IBUYERWALLET_TREE = TAG_TREE with type value=IBuyerWalletTable.t
  module type ISELLERWALLET_TREE = TAG_TREE with type value=ISellerWalletTable.t
  module type IBID_TREE = TAG_TREE with type value=IBidTable.t
  module type IITEMBID_TREE = TAG_TREE with type value=IItemBidTable.t
  module type IWALLETBID_TREE = TAG_TREE with type value=IWalletBidTable.t
  module type IWALLETITEM_TREE = TAG_TREE with type value=IWalletItemTable.t
  module type IPENALITY_TREE = TAG_TREE with type value=IPenalityTable.t

  let transform_tree : type a b c. (module MY_TREE with type t=a 
                                                    and type tag=b)
                -> (module IRMIN_DATA_STRUCTURE with type t = c) 
                -> (c -> t)
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
            T.add t tag (f vt)
        end in
      (module Vtree: TAG_TREE with type t=T.t 
                               and type tag=T.tag 
                               and type value=V.t)

  module AO_store  = struct
    module S = Irmin_git.AO(Git_unix.FS)(AO_value)
    include S

    type adt=OM.db

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
        T.add tree tag v >>= fun tree' ->
        Lwt.return (k,tree')
      end

  end

  module rec BC_value: IRMIN_STORE_VALUE with type t = t 
                                          and type adt=adt = struct
    include AO_value

    type adt=OM.db

    let of_vadt: type a b c. 
              (module IRMIN_DATA_STRUCTURE with type adt=a 
                                            and type t = b) 
            -> (module TAG_TREE with type value = b 
                                 and type t = c)
            -> a -> c -> (b*c) Lwt.t =
      fun (module V) (module T) vadt tr ->
        V.of_adt (module T) vadt tr

    let rec of_adt : type a. (module MY_TREE with type t=a) ->
                        (OM.db) -> (a -> (boxed_t*a) Lwt.t) =
      fun  (module T) (adt:adt) ->
        (*
         * We momentarily override Lwt's bind and return so as to pass
         * the tree around without making a mess.
         *)
        let (>>=) m f = fun tr -> 
          m tr >>= fun (k,tr') -> f k tr' in
        let return x = fun tr -> Lwt.return (x,tr) in
        begin
          let ii_tree = transform_tree (module T) 
                           (module IItemTable)
                           (fun iit -> IItemTable iit) in
          let module IItemTree = 
              (val ii_tree : IITEM_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IItemTable) (module IItemTree)
                  adt.item_table >>= fun iitem_table ->
          let _ = printf "item table added\n" in
          let _ = flush_all() in
          let ibw_tree = transform_tree (module T) 
                           (module IBuyerWalletTable)
                           (fun ibwt -> IBuyerWalletTable ibwt) in
          let module IBuyerWalletTree = 
              (val ibw_tree : IBUYERWALLET_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IBuyerWalletTable) (module IBuyerWalletTree)
                  adt.buyerwallet_table >>= fun ibuyerwallet_table ->
          let _ = printf "buyerwallet table added\n" in
          let _ = flush_all() in
          let isw_tree = transform_tree (module T) 
                           (module ISellerWalletTable)
                           (fun iwt -> ISellerWalletTable iwt) in
          let module ISellerWalletTree = 
              (val isw_tree : ISELLERWALLET_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ISellerWalletTable) (module ISellerWalletTree)
                  adt.sellerwallet_table >>= fun isellerwallet_table ->
          let _ = printf "sellerwallet table added\n" in
          let _ = flush_all() in
          let ib_tree = transform_tree (module T) 
                           (module IBidTable)
                           (fun iwt -> IBidTable iwt) in
          let module IBidTree = 
              (val ib_tree : IBID_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IBidTable) (module IBidTree)
                  adt.bid_table >>= fun ibid_table ->
          let _ = printf "Bid table added\n" in
          let _ = flush_all() in
          let iib_tree = transform_tree (module T) 
                           (module IItemBidTable)
                           (fun iwt -> IItemBidTable iwt) in
          let module IItemBidTree = 
              (val iib_tree : IITEMBID_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IItemBidTable) (module IItemBidTree)
                  adt.itembid_table >>= fun iitembid_table ->
          let _ = printf "Itembid table added\n" in
          let _ = flush_all() in
          let iwb_tree = transform_tree (module T) 
                           (module IWalletBidTable)
                           (fun iwt -> IWalletBidTable iwt) in
          let module IWalletBidTree = 
              (val iwb_tree : IWALLETBID_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IWalletBidTable) (module IWalletBidTree)
                  adt.walletbid_table >>= fun iwalletbid_table ->
          let _ = printf "Walletbid table added\n" in
          let _ = flush_all() in
          let iwi_tree = transform_tree (module T) 
                           (module IWalletItemTable)
                           (fun iwt -> IWalletItemTable iwt) in
          let module IWalletItemTree = 
              (val iwi_tree : IWALLETITEM_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IWalletItemTable) (module IWalletItemTree)
                  adt.walletitem_table >>= fun iwalletitem_table ->
          let _ = printf "Walletitem table added\n" in
          let _ = flush_all() in
          let ip_tree = transform_tree (module T) 
                           (module IPenalityTable)
                           (fun iwt -> IPenalityTable iwt) in
          let module IPenalityTree = 
              (val ip_tree : IPENALITY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IPenalityTable) (module IPenalityTree)
                  adt.penality_table >>= fun ipenality_table ->
          let _ = printf "Penality table added\n" in
          let _ = flush_all() in
          return @@ DB {item_table=iitem_table;
                        buyerwallet_table=ibuyerwallet_table;
                        sellerwallet_table=isellerwallet_table;
                        bid_table=ibid_table;
                        itembid_table=iitembid_table;
                        walletbid_table=iwalletbid_table;
                        walletitem_table=iwalletitem_table;
                        penality_table=ipenality_table;}
        end

    let madt_to_adt (t:madt) : OM.db Lwt.t =
      IItemTable.to_adt t.item_table >>= fun item_table ->
      IBuyerWalletTable.to_adt t.buyerwallet_table >>= fun buyerwallet_table -> 
      ISellerWalletTable.to_adt t.sellerwallet_table >>= fun sellerwallet_table ->
      IBidTable.to_adt t.bid_table >>= fun bid_table ->
      IItemBidTable.to_adt t.itembid_table >>= fun itembid_table ->
      IWalletBidTable.to_adt t.walletbid_table >>= fun walletbid_table ->
      IWalletItemTable.to_adt t.walletitem_table >>= fun walletitem_table ->
      IPenalityTable.to_adt t.penality_table >>= fun penality_table ->
      let open Rubis in
      Lwt.return @@ {item_table=item_table;
                     buyerwallet_table=buyerwallet_table;
                     sellerwallet_table=sellerwallet_table;
                     bid_table=bid_table;
                     itembid_table=itembid_table;
                     walletbid_table=walletbid_table;
                     walletitem_table=walletitem_table;
                     penality_table=penality_table;}

    let to_adt (t:t) : OM.db Lwt.t =match t with
      | DB madt -> madt_to_adt madt
      | _ -> failwith "Exhaustiveness in Irubis.to_adt"


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
