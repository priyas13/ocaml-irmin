open Printf
open Msigs
open Lwt.Infix
module Id = Rubis.Id

type id = Id.t

let id = Id.t

let counter_merge = Rubis.counter_merge

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module Serialization(S:sig 
                        type t 
                        val t: t Irmin.Type.t 
                      end) = struct 
  open S

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

module MakeVersionedDS(Config:CONFIG)
                      (OM:MERGEABLE)
                      (AO_value:Irmin.Contents.Conv with type t = OM.t) =
struct
  module S = Irmin_git.AO(Git_unix.FS)(AO_value)

  include AO_value

  type adt = OM.t

  let create config =
    let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
        "level" Irmin.Private.Conf.(some int) None
    in
    let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
    let level = Irmin.Private.Conf.get config level in
    Git_unix.FS.create ?root ?level ()

  (* Creates a Git backend *)
  let create () = create @@ Irmin_git.config Config.root

  module type MY_TREE = TAG_TREE with type value=AO_value.t

  let add_and_link: type a. (module MY_TREE with type t=a) 
                    -> S.t -> AO_value.t -> a -> (K.t*a) Lwt.t =
    fun (module T) t v tree ->
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
          Lwt.return (k,tree')
     
  let of_adt : type a. (module MY_TREE with type t=a) ->
                    adt -> a -> (t*a) Lwt.t = 
    fun (module T) adt tr ->
      begin
        create () >>= fun ao_store ->
        add_and_link (module T) 
                ao_store adt tr >>= fun (_,tr') ->
        Lwt.return (adt,tr')
      end

  let to_adt (t:t) : adt Lwt.t =
    Lwt.return t

  let merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
    if v1=v2 then Irmin.Merge.ok v1
    else begin 
      let open Irmin.Merge.Infix in
      old () >>=* fun old ->
      let old = from_just old "merge" in
      let v = OM.merge ~ancestor:old v1 v2 in
      Irmin.Merge.ok v
    end
      
  let merge = Irmin.Merge.(option (v t merge))

end

module Make(Config:CONFIG) = 
struct
  
  module IItem : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.Item.t = 
  struct

    module OM = Rubis.Item 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.Item in
        record "t" (fun item_id item_desc item_initialprice item_maxbid -> 
                    {item_id; item_desc; item_initialprice; item_maxbid})
        |+ field "item_id" id (fun t -> t.item_id)
        |+ field "item_desc" string (fun t -> t.item_desc)
        |+ field "item_initialprice" int64 (fun t -> t.item_initialprice)
        |+ field "item_maxbid" int64 (fun t -> t.item_maxbid)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

    module IBuyerWallet : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.BuyerWallet.t = 
  struct

    module OM = Rubis.BuyerWallet 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.BuyerWallet in
        record "t" (fun buy_id buyer_balance -> 
                    {buy_id; buyer_balance})
        |+ field "buy_id" id (fun t -> t.buy_id)
        |+ field "buyer_balance" int64 (fun t -> t.buyer_balance)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

      module ISellerWallet : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.SellerWallet.t = 
  struct

    module OM = Rubis.SellerWallet 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.SellerWallet in
        record "t" (fun sell_id sell_i_id seller_balance -> 
                    {sell_id; sell_i_id; seller_balance})
        |+ field "sell_id" id (fun t -> t.sell_id)
        |+ field "sell_i_id" id (fun t -> t.sell_i_id)
        |+ field "seller_balance" int64 (fun t -> t.seller_balance)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


      module IBid : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.Bid.t = 
  struct

    module OM = Rubis.Bid

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.Bid in
        record "t" (fun b_id b_buy_id b_i_id bid_amount -> 
                    {b_id; b_buy_id; b_i_id; bid_amount})
        |+ field "b_id" id (fun t -> t.b_id)
        |+ field "b_buy_id" id (fun t -> t.b_buy_id)
        |+ field "b_i_id" id (fun t -> t.b_i_id)
        |+ field "bid_amount" int64 (fun t -> t.bid_amount)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

      module IItemBid : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.ItemBid.t = 
  struct

    module OM = Rubis.ItemBid

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.ItemBid in
        record "t" (fun i_id i_buy_id -> 
                    {i_id; i_buy_id})
        |+ field "i_id" id (fun t -> t.i_id)
        |+ field "i_b_id" id (fun t -> t.i_buy_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


        module IWalletBid : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.WalletBid.t = 
  struct

    module OM = Rubis.WalletBid

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.WalletBid in
        record "t" (fun w_buy_id w_b_id timestamp -> 
                    {w_buy_id; w_b_id; timestamp})
        |+ field "w_buy_id" id (fun t -> t.w_buy_id)
        |+ field "w_b_id" id (fun t -> t.w_b_id)
        |+ field "timestamp" (option float) (fun t -> t.timestamp)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

          module IWalletItem : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.WalletItem.t = 
  struct

    module OM = Rubis.WalletItem

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.WalletItem in
        record "t" (fun w_id w_i_id -> 
                    {w_id; w_i_id})
        |+ field "w_id" id (fun t -> t.w_id)
        |+ field "w_i_id" id (fun t -> t.w_i_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


  module IPenality : IRMIN_DATA_STRUCTURE 
    with type adt = Rubis.Penality.t = 
  struct

    module OM = Rubis.Penality

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Rubis.Penality in
        record "t" (fun u_p_id penality_amount -> 
                    {u_p_id; penality_amount})
        |+ field "u_p_id" id (fun t -> t.u_p_id)
        |+ field "penality_amount" int64 (fun t -> t.penality_amount)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end




end 

  