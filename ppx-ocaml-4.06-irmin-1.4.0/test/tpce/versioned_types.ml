open Printf
open Msigs
open Lwt.Infix
module Id = Tpce.Id

type id = Id.t

let id = Id.t

let counter_merge = Tpce.counter_merge

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

  module IAccountPermission : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.AccountPermission.t =
    struct 
    module OM = Tpce.AccountPermission

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.AccountPermission in
        record "t" (fun ap_ca_id ap_tax_id -> {ap_ca_id; ap_tax_id})
        |+ field "ap_ca_id" id (fun t -> t.ap_ca_id)
        |+ field "ap_tax_id" id (fun t -> t.ap_tax_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

   module ICustomer : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Customer.t =
    struct 
    module OM = Tpce.Customer

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Customer in
        record "t" (fun c_id c_tax_id c_tier -> {c_id; c_tax_id; c_tier})
        |+ field "c_id" id (fun t -> t.c_id)
        |+ field "c_tax_id" id (fun t -> t.c_tax_id)
        |+ field "c_tier" id (fun t -> t.c_tier)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ICustomerAccount : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.CustomerAccount.t =
    struct 
    module OM = Tpce.CustomerAccount

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.CustomerAccount in
        record "t" (fun ca_id ca_b_id ca_c_id ca_bal -> {ca_id; ca_b_id; ca_c_id; ca_bal})
        |+ field "ca_id" id (fun t -> t.ca_id)
        |+ field "ca_b_id" id (fun t -> t.ca_b_id)
        |+ field "ca_c_id" id (fun t -> t.ca_c_id)
        |+ field "ca_bal" int64 (fun t -> t.ca_bal)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  
  module ICustomerTaxRate : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.CustomerTaxRate.t =
    struct 
    module OM = Tpce.CustomerTaxRate

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.CustomerTaxRate in
        record "t" (fun cx_tx_id cx_c_id -> {cx_tx_id; cx_c_id})
        |+ field "cx_tx_id" id (fun t -> t.cx_tx_id)
        |+ field "cx_c_id" id (fun t -> t.cx_c_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end
    include MakeVersionedDS(Config)(OM)(AO_value)
  end

    module IHolding : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Holding.t =
    struct 
    module OM = Tpce.Holding

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Holding in
        record "t" (fun h_t_id h_ca_id h_s_symb h_price h_qty -> 
                    {h_t_id; h_ca_id; h_s_symb; h_price; h_qty})
        |+ field "h_t_id" id (fun t -> t.h_t_id)
        |+ field "h_ca_id" id (fun t -> t.h_ca_id)
        |+ field "h_s_symb" id (fun t -> t.h_s_symb)
        |+ field "h_price" int64 (fun t -> t.h_price)
        |+ field "h_qty" int64 (fun t -> t.h_qty)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module IHoldingHistory : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.HoldingHistory.t =
    struct 
    module OM = Tpce.HoldingHistory

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.HoldingHistory in
        record "t" (fun hh_h_t_id hh_t_id hh_b_qty hh_a_qty -> 
                    {hh_h_t_id; hh_t_id; hh_b_qty; hh_a_qty})
        |+ field "hh_h_t_id" id (fun t -> t.hh_h_t_id)
        |+ field "hh_t_id" id (fun t -> t.hh_t_id)
        |+ field "hh_b_qty" int64 (fun t -> t.hh_b_qty)
        |+ field "hh_a_qty" int64 (fun t -> t.hh_a_qty)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module IHoldingSummary : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.HoldingSummary.t =
    struct 
    module OM = Tpce.HoldingSummary

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.HoldingSummary in
        record "t" (fun hs_ca_id hs_s_symb hs_qty -> 
                    {hs_ca_id; hs_s_symb; hs_qty})
        |+ field "hs_ca_id" id (fun t -> t.hs_ca_id)
        |+ field "hs_s_symb" id (fun t -> t.hs_s_symb)
        |+ field "hs_qty" int64 (fun t -> t.hs_qty)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

    module IBroker : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Broker.t =
    struct 
    module OM = Tpce.Broker

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Broker in
        record "t" (fun b_id b_num_trades b_comm -> 
                    {b_id; b_num_trades; b_comm})
        |+ field "b_id" id (fun t -> t.b_id)
        |+ field "b_num_trades" int64 (fun t -> t.b_num_trades)
        |+ field "b_comm" int64 (fun t -> t.b_comm)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ICharge : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Charge.t =
    struct 
    module OM = Tpce.Charge

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Charge in
        record "t" (fun ch_tt_id ch_c_tier ch_chrg -> 
                    {ch_tt_id; ch_c_tier; ch_chrg})
        |+ field "ch_tt_id" id (fun t -> t.ch_tt_id)
        |+ field "ch_c_tier" int64 (fun t -> t.ch_c_tier)
        |+ field "ch_chrg" int64 (fun t -> t.ch_chrg)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ICommissionRate : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.CommissionRate.t =
    struct 
    module OM = Tpce.CommissionRate

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.CommissionRate in
        record "t" (fun cr_c_tier cr_tt_id cr_ex_id cr_from_qty cr_to_qty cr_rate -> 
                    {cr_c_tier; cr_tt_id; cr_ex_id; cr_from_qty; cr_to_qty; cr_rate})
        |+ field "cr_c_tier" id (fun t -> t.cr_c_tier)
        |+ field "cr_tt_id" id (fun t -> t.cr_tt_id)
        |+ field "cr_ex_id" id (fun t -> t.cr_ex_id)
        |+ field "cr_from_qty" int64 (fun t -> t.cr_from_qty)
        |+ field "cr_to_qty" int64 (fun t -> t.cr_to_qty)
        |+ field "cr_rate" int64 (fun t -> t.cr_rate)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

    module ISettlement : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Settlement.t =
    struct 
    module OM = Tpce.Settlement

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Settlement in
        record "t" (fun se_t_id se_cash_type se_amt -> 
                    {se_t_id; se_cash_type; se_amt})
        |+ field "se_t_id" id (fun t -> t.se_t_id)
        |+ field "se_cash_type" bool (fun t -> t.se_cash_type)
        |+ field "se_amt" int64 (fun t -> t.se_amt)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ITrade : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Trade.t =
    struct 
    module OM = Tpce.Trade

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Trade in
        record "t" (fun t_id t_ca_id tt_id t_is_cash t_qty t_symb 
                        t_exec_name t_trade_price t_dts t_chrg t_comm -> 
                    {t_id; t_ca_id; tt_id; t_is_cash; t_qty; t_symb; 
                        t_exec_name; t_trade_price; t_dts; t_chrg; t_comm})
        |+ field "t_id" id (fun t -> t.t_id)
        |+ field "t_ca_id" id (fun t -> t.t_ca_id)
        |+ field "tt_id" id (fun t -> t.tt_id)
        |+ field "t_is_cash" bool (fun t -> t.t_is_cash)
        |+ field "t_qty" int64 (fun t -> t.t_qty)
        |+ field "t_symb" id (fun t -> t.t_symb)
        |+ field "t_exec_name" string (fun t -> t.t_exec_name)
        |+ field "t_trade_price" int64 (fun t -> t.t_trade_price)
        |+ field "t_dts" (option float) (fun t -> t.t_dts)
        |+ field "t_chrg" int64 (fun t -> t.t_chrg)
        |+ field "t_comm" int64 (fun t -> t.t_comm)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ITradeRequest : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.TradeRequest.t =
    struct 
    module OM = Tpce.TradeRequest

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.TradeRequest in
        record "t" (fun tr_t_id tr_tt_id tr_b_id tr_s_symb tr_qty tr_bid_price -> 
                    {tr_t_id; tr_tt_id; tr_b_id; tr_s_symb; tr_qty; tr_bid_price})
        |+ field "tr_t_id" id (fun t -> t.tr_t_id)
        |+ field "tr_tt_id" id (fun t -> t.tr_tt_id)
        |+ field "tr_b_id" id (fun t -> t.tr_b_id)
        |+ field "tr_s_symb" id (fun t -> t.tr_s_symb)
        |+ field "tr_qty" int64 (fun t -> t.tr_qty)
        |+ field "tr_bid_price" int64 (fun t -> t.tr_bid_price)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

   module ITradeHistory : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.TradeHistory.t =
    struct 
    module OM = Tpce.TradeHistory

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.TradeHistory in
        record "t" (fun th_t_id th_dts -> {th_t_id; th_dts})
        |+ field "th_t_id" id (fun t -> t.th_t_id)
        |+ field "th_dts" (option float) (fun t -> t.th_dts)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ITradeType : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.TradeType.t =
    struct 
    module OM = Tpce.TradeType

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.TradeType in
        record "t" (fun tt_id tt_is_sell tt_is_market -> {tt_id; tt_is_sell; tt_is_market})
        |+ field "tt_id" id (fun t -> t.tt_id)
        |+ field "tt_is_sell" int64 (fun t -> t.tt_is_sell)
        |+ field "tt_is_market" int64 (fun t -> t.tt_is_market)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

   module ILastTrade : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.LastTrade.t =
    struct 
    module OM = Tpce.LastTrade

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.LastTrade in
        record "t" (fun lt_s_symb lt_price lt_dts lt_vol -> {lt_s_symb; lt_price; lt_dts; lt_vol})
        |+ field "lt_s_symb" id (fun t -> t.lt_s_symb)
        |+ field "lt_price" int64 (fun t -> t.lt_price)
        |+ field "lt_dts" (option float) (fun t -> t.lt_dts)
        |+ field "lt_vol" int64 (fun t -> t.lt_vol)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


   module ISecurity : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Security.t =
    struct 
    module OM = Tpce.Security

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Security in
        record "t" (fun s_symb s_ex_id s_co_id -> {s_symb; s_ex_id; s_co_id})
        |+ field "s_symb" id (fun t -> t.s_symb)
        |+ field "s_ex_id" id (fun t -> t.s_ex_id)
        |+ field "s_co_id" id (fun t -> t.s_co_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end
 
  module ICompany : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Company.t =
    struct 
    module OM = Tpce.Company

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Company in
        record "t" (fun co_id -> {co_id})
        |+ field "co_id" id (fun t -> t.co_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


   module IExchange : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.Exchange.t =
    struct 
    module OM = Tpce.Exchange

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.Exchange in
        record "t" (fun ex_id -> {ex_id})
        |+ field "co_id" id (fun t -> t.ex_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

   module ITaxRate : IRMIN_DATA_STRUCTURE
    with type adt = Tpce.TaxRate.t =
    struct 
    module OM = Tpce.TaxRate

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Tpce.TaxRate in
        record "t" (fun tax_id tx_rate -> {tax_id; tx_rate})
        |+ field "tax_id" id (fun t -> t.tax_id)
        |+ field "tx_rate" int64 (fun t -> t.tx_rate)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end


end





