open Irmin_unix
open Printf
open Msigs
open Tpce
open Lwt.Infix

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module MakeVersioned (Config: CONFIG)  = struct

  module OM = Tpce
  module K = Irmin.Hash.SHA1

  module VersionedTpce = Versioned_types.Make(Config)

  module Serialization = Versioned_types.Serialization

  open VersionedTpce

  type adt = OM.db

module ICustomerTable = Irbmap.MakeVersioned(Config)(Id)(Customer)(CustomerTable)(ICustomer)


module ICustomerAccountTable = Irbmap.MakeVersioned(Config)(IdTriple)(CustomerAccount)(CustomerAccountTable)(ICustomerAccount)

module IHoldingTable = Irbmap.MakeVersioned(Config)(IdTriple)(Holding)(HoldingTable)(IHolding)

module IHoldingHistoryTable = Irbmap.MakeVersioned(Config)(IdPair)(HoldingHistory)(HoldingHistoryTable)(IHoldingHistory)

module IChargeTable = Irbmap.MakeVersioned(Config)(Id)(Charge)(ChargeTable)(ICharge)

module ICommissionRateTable = Irbmap.MakeVersioned(Config)(IdTriple)(CommissionRate)(CommissionRateTable)(ICommissionRate)

module IHoldingSummaryTable = Irbmap.MakeVersioned(Config)(IdPair)(HoldingSummary)(HoldingSummaryTable)(IHoldingSummary)

module IBrokerTable = Irbmap.MakeVersioned(Config)(Id)(Broker)(BrokerTable)(IBroker)

module ITradeTable = Irbmap.MakeVersioned(Config)(IdQuad)(Trade)(TradeTable)(ITrade)

module ILastTradeTable = Irbmap.MakeVersioned(Config)(Id)(LastTrade)(LastTradeTable)(ILastTrade)

module ITradeHistoryTable = Irbmap.MakeVersioned(Config)(Id)(TradeHistory)(TradeHistoryTable)(ITradeHistory)

module ITradeRequestTable = Irbmap.MakeVersioned(Config)(IdQuad)(TradeRequest)(TradeRequestTable)(ITradeRequest)

module ITaxRateTable = Irbmap.MakeVersioned(Config)(Id)(TaxRate) (TaxRateTable) (ITaxRate)

module IExchangeTable = Irbmap.MakeVersioned(Config)(Id)(Exchange) (ExchangeTable) (IExchange)

module ICompanyTable = Irbmap.MakeVersioned(Config)(Id)(Company) (CompanyTable) (ICompany)

module ISecurityTable = Irbmap.MakeVersioned(Config)(Id)(Security) (SecurityTable) (ISecurity)

module ITradeTypeTable = Irbmap.MakeVersioned(Config)(Id)(TradeType) (TradeTypeTable) (ITradeType)

module ISettlementTable = Irbmap.MakeVersioned(Config)(Id)(Settlement) (SettlementTable) (ISettlement)

module IAccountPermissionTable = Irbmap.MakeVersioned(Config)(Id)(AccountPermission) (AccountPermissionTable) (IAccountPermission)

type madt = {customer_table: ICustomerTable.t; 
           customer_account_table: ICustomerAccountTable.t;
           holding_table: IHoldingTable.t;
           holding_history_table : IHoldingHistoryTable.t;
           charge_table : IChargeTable.t;
           commission_rate_table : ICommissionRateTable.t;
           holding_summary_table: IHoldingSummaryTable.t;
           broker_table: IBrokerTable.t;
           trade_table: ITradeTable.t;
           last_trade_table : ILastTradeTable.t;
           trade_history_table : ITradeHistoryTable.t;
           trade_request_table : ITradeRequestTable.t;
           trade_type_table : ITradeTypeTable.t;
           tax_rate_table : ITaxRateTable.t;
           settlement_table : ISettlementTable.t;
           exchange_table : IExchangeTable.t;
           company_table : ICompanyTable.t;
           security_table : ISecurityTable.t;
           account_permission_table : IAccountPermissionTable.t}

  type t = 
    | DB of madt
    | ICustomerTable of ICustomerTable.t 
    | ICustomerAccountTable of ICustomerAccountTable.t
    | IHoldingTable of IHoldingTable.t
    | IHoldingHistoryTable of IHoldingHistoryTable.t
    | IChargeTable of IChargeTable.t
    | ICommissionRateTable of ICommissionRateTable.t
    | IHoldingSummaryTable of IHoldingSummaryTable.t
    | IBrokerTable of IBrokerTable.t
    | ITradeTable of ITradeTable.t
    | ILastTradeTable of ILastTradeTable.t
    | ITradeHistoryTable of ITradeHistoryTable.t
    | ITradeRequestTable of ITradeRequestTable.t
    | ITradeTypeTable of ITradeTypeTable.t
    | ITaxRateTable of ITaxRateTable.t
    | ISettlementTable of ISettlementTable.t
    | IExchangeTable of IExchangeTable.t
    | ICompanyTable of ICompanyTable.t
    | ISecurityTable of ISecurityTable.t
    | IAccountPermissionTable of IAccountPermissionTable.t


  type boxed_t = t

  let (madt : madt Irmin.Type.t) = 
    let open Irmin.Type in 
    record "madt" 
      (fun customer_table 
           customer_account_table
           holding_table
           holding_history_table
           charge_table
           commission_rate_table
           holding_summary_table
           broker_table
           trade_table
           last_trade_table
           trade_history_table
           trade_request_table
           trade_type_table
           tax_rate_table
           settlement_table
           exchange_table
           company_table
           security_table
           account_permission_table -> 
      {customer_table; 
           customer_account_table;
           holding_table;
           holding_history_table;
           charge_table;
           commission_rate_table;
           holding_summary_table;
           broker_table;
           trade_table;
           last_trade_table;
           trade_history_table;
           trade_request_table;
           trade_type_table;
           tax_rate_table;
           settlement_table;
           exchange_table;
           company_table;
           security_table;
           account_permission_table})
    |+ field "customer_table" ICustomerTable.t 
              (fun t -> t.customer_table)
    |+ field "customer_account_table" ICustomerAccountTable.t
              (fun t -> t.customer_account_table)
    |+ field "holding_table" IHoldingTable.t
              (fun t -> t.holding_table)
    |+ field "holding_history_table" IHoldingHistoryTable.t
              (fun t -> t.holding_history_table)
    |+ field "charge_table" IChargeTable.t
              (fun t -> t.charge_table)
    |+ field "commission_rate_table" ICommissionRateTable.t
              (fun t -> t.commission_rate_table)
    |+ field "holding_summary_table" IHoldingSummaryTable.t
              (fun t -> t.holding_summary_table)
    |+ field "broker_table" IBrokerTable.t
              (fun t -> t.broker_table)
    |+ field "trade_table" ITradeTable.t
              (fun t -> t.trade_table)
    |+ field "last_trade_table" ILastTradeTable.t
              (fun t -> t.last_trade_table)
    |+ field "trade_history_table" ITradeHistoryTable.t
              (fun t -> t.trade_history_table)
    |+ field "trade_request_table" ITradeRequestTable.t
              (fun t -> t.trade_request_table)
    |+ field "trade_type_table" ITradeTypeTable.t
              (fun t -> t.trade_type_table)
    |+ field "tax_rate_table" ITaxRateTable.t
              (fun t -> t.tax_rate_table)
    |+ field "settlement_table" ISettlementTable.t
              (fun t -> t.settlement_table)
    |+ field "exchange_table" IExchangeTable.t
              (fun t -> t.exchange_table)
    |+ field "company_table" ICompanyTable.t
              (fun t -> t.company_table)
    |+ field "security_table" ISecurityTable.t
              (fun t -> t.security_table)
    |+ field "account_permission_table" IAccountPermissionTable.t
              (fun t -> t.account_permission_table)
    |> sealr

  let (t: t Irmin.Type.t) = 
    let open Irmin.Type in
    variant "t" (fun db c ca h hh ch com hs b t lt th tr tt tx se ex co s ap -> function
        | DB a  -> db a
        | ICustomerTable a -> c a
        | ICustomerAccountTable a -> ca a 
        | IHoldingTable a -> h a 
        | IHoldingHistoryTable a -> hh a 
        | IChargeTable a -> ch a 
        | ICommissionRateTable a -> com a
        | IHoldingSummaryTable a -> hs a
        | IBrokerTable a -> b a 
        | ITradeTable a -> t a 
        | ILastTradeTable a -> lt a 
        | ITradeHistoryTable a -> th a 
        | ITradeRequestTable a -> tr a 
        | ITradeTypeTable a -> tt a 
        | ITaxRateTable a -> tx a 
        | ISettlementTable a -> se a 
        | IExchangeTable a -> ex a 
        | ICompanyTable a -> co a 
        | ISecurityTable a -> s a 
        | IAccountPermissionTable a -> ap a)
    |~ case1 "DB" madt (fun x -> DB x)
    |~ case1 "ICustomerTable" ICustomerTable.t (fun x -> ICustomerTable x)
    |~ case1 "ICustomerAccountTable" ICustomerAccountTable.t (fun x -> ICustomerAccountTable x)
    |~ case1 "IHoldingTable" IHoldingTable.t (fun x -> IHoldingTable x)
    |~ case1 "IHoldingHistoryTable" IHoldingHistoryTable.t (fun x -> IHoldingHistoryTable x)
    |~ case1 "IChargeTable" IChargeTable.t (fun x -> IChargeTable x)
    |~ case1 "ICommissionRateTable" ICommissionRateTable.t (fun x -> ICommissionRateTable x)
    |~ case1 "IHoldingSummaryTable" IHoldingSummaryTable.t (fun x -> IHoldingSummaryTable x)
    |~ case1 "IBrokerTable" IBrokerTable.t (fun x -> IBrokerTable x)
    |~ case1 "ITradeTable" ITradeTable.t (fun x -> ITradeTable x)
    |~ case1 "ILastTradeTable" ILastTradeTable.t (fun x -> ILastTradeTable x)
    |~ case1 "ITradeHistoryTable" ITradeHistoryTable.t (fun x -> ITradeHistoryTable x)
    |~ case1 "ITradeRequestTable" ITradeRequestTable.t (fun x -> ITradeRequestTable x)
    |~ case1 "ITradeTypeTable" ITradeTypeTable.t (fun x -> ITradeTypeTable x)
    |~ case1 "ITaxRateTable" ITaxRateTable.t (fun x -> ITaxRateTable x)
    |~ case1 "ISettlementTable" ISettlementTable.t (fun x -> ISettlementTable x)
    |~ case1 "IExchangeTable" IExchangeTable.t (fun x -> IExchangeTable x)
    |~ case1 "ICompanyTable" ICompanyTable.t (fun x -> ICompanyTable x)
    |~ case1 "ISecurityTable" ISecurityTable.t (fun x -> ISecurityTable x)
    |~ case1 "IAccountPermissionTable" IAccountPermissionTable.t (fun x -> IAccountPermissionTable x)
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
  module type ICUSTOMER_TREE = TAG_TREE with type value=ICustomerTable.t
  module type ICUSTOMERACCOUNT_TREE = TAG_TREE with type value=ICustomerAccountTable.t
  module type IHOLDING_TREE = TAG_TREE with type value=IHoldingTable.t
  module type IHOLDINGHISTORY_TREE = TAG_TREE with type value=IHoldingHistoryTable.t
  module type ICHARGE_TREE = TAG_TREE with type value=IChargeTable.t
  module type ICOMMISSIONRATE_TREE = TAG_TREE with type value=ICommissionRateTable.t
  module type IHOLDINGSUMMARY_TREE = TAG_TREE with type value=IHoldingSummaryTable.t
  module type ITRADE_TREE = TAG_TREE with type value=ITradeTable.t
  module type ILASTRADE_TREE = TAG_TREE with type value=ILastTradeTable.t
  module type ITRADEHISTORY_TREE = TAG_TREE with type value=ITradeHistoryTable.t
  module type ITRADEREQUEST_TREE = TAG_TREE with type value=ITradeRequestTable.t
  module type ITRADETYPE_TREE = TAG_TREE with type value=ITradeTypeTable.t
  module type ITAXRATE_TREE = TAG_TREE with type value=ITaxRateTable.t
  module type ISETTLEMENT_TREE = TAG_TREE with type value=ISettlementTable.t
  module type IEXCHANGE_TREE = TAG_TREE with type value=IExchangeTable.t
  module type ICOMPANY_TREE = TAG_TREE with type value=ICompanyTable.t
  module type ISECURITY_TREE = TAG_TREE with type value=ISecurityTable.t
  module type IACCOUNTPERMISSION_TREE = TAG_TREE with type value=IAccountPermissionTable.t
  module type IBROKER_TREE = TAG_TREE with type value=IBrokerTable.t


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

          let ic_tree = transform_tree (module T) 
                           (module ICustomerTable)
                           (fun iwt -> ICustomerTable iwt) in
          let module ICustomerTree = 
              (val ic_tree : ICUSTOMER_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ICustomerTable) (module ICustomerTree)
                  adt.customer_table >>= fun icustomer_table ->
          let _ = printf "customer account table added\n" in
          let _ = flush_all() in

          let ica_tree = transform_tree (module T) 
                           (module ICustomerAccountTable)
                           (fun iwt -> ICustomerAccountTable iwt) in
          let module ICustomerAccountTree = 
              (val ica_tree : ICUSTOMERACCOUNT_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ICustomerAccountTable) (module ICustomerAccountTree)
                  adt.customer_account_table >>= fun icustomeraccount_table ->
          let _ = printf "customer table added\n" in
          let _ = flush_all() in

          let ih_tree = transform_tree (module T) 
                           (module IHoldingTable)
                           (fun iwt -> IHoldingTable iwt) in
          let module IHoldingTree = 
              (val ih_tree : IHOLDING_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IHoldingTable) (module IHoldingTree)
                  adt.holding_table >>= fun iholding_table ->
          let _ = printf "holding table added\n" in
          let _ = flush_all() in

          let ihs_tree = transform_tree (module T) 
                           (module IHoldingSummaryTable)
                           (fun iwt -> IHoldingSummaryTable iwt) in
          let module IHoldingSummaryTree = 
              (val ihs_tree : IHOLDINGSUMMARY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IHoldingSummaryTable) (module IHoldingSummaryTree)
                  adt.holding_summary_table >>= fun iholdingsummary_table ->
          let _ = printf "holding table added\n" in
          let _ = flush_all() in

          let ich_tree = transform_tree (module T) 
                           (module IChargeTable)
                           (fun iwt -> IChargeTable iwt) in
          let module IChargeTree = 
              (val ich_tree : ICHARGE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IChargeTable) (module IChargeTree)
                  adt.charge_table >>= fun icharge_table ->
          let _ = printf "charge table added\n" in
          let _ = flush_all() in

          let ihh_tree = transform_tree (module T) 
                           (module IHoldingHistoryTable)
                           (fun iwt -> IHoldingHistoryTable iwt) in
          let module IHoldingHistoryTree = 
              (val ihh_tree : IHOLDINGHISTORY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IHoldingHistoryTable) (module IHoldingHistoryTree)
                  adt.holding_history_table >>= fun iholdinghistory_table ->
          let _ = printf "holding table added\n" in
          let _ = flush_all() in

          let it_tree = transform_tree (module T) 
                           (module ITradeTable)
                           (fun iwt -> ITradeTable iwt) in
          let module ITradeTree = 
              (val it_tree : ITRADE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ITradeTable) (module ITradeTree)
                  adt.trade_table >>= fun itrade_table ->
          let _ = printf "trade table added\n" in
          let _ = flush_all() in

           let itt_tree = transform_tree (module T) 
                           (module ITradeTypeTable)
                           (fun iwt -> ITradeTypeTable iwt) in
          let module ITradeTypeTree = 
              (val itt_tree : ITRADETYPE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ITradeTypeTable) (module ITradeTypeTree)
                  adt.trade_type_table >>= fun itradetype_table ->
          let _ = printf "trade type table added\n" in
          let _ = flush_all() in 

           let itr_tree = transform_tree (module T) 
                           (module ITradeRequestTable)
                           (fun iwt -> ITradeRequestTable iwt) in
          let module ITradeRequestTree = 
              (val itr_tree : ITRADEREQUEST_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ITradeRequestTable) (module ITradeRequestTree)
                  adt.trade_request_table >>= fun itraderequest_table ->
          let _ = printf "trade request table added\n" in
          let _ = flush_all() in 

           let iex_tree = transform_tree (module T) 
                           (module IExchangeTable)
                           (fun iwt -> IExchangeTable iwt) in
          let module IExchangeTree = 
              (val iex_tree : IEXCHANGE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IExchangeTable) (module IExchangeTree)
                  adt.exchange_table >>= fun iexchange_table ->
          let _ = printf "exchange table added\n" in
          let _ = flush_all() in 

           let ico_tree = transform_tree (module T) 
                           (module ICompanyTable)
                           (fun iwt -> ICompanyTable iwt) in
          let module ICompanyTree = 
              (val ico_tree : ICOMPANY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ICompanyTable) (module ICompanyTree)
                  adt.company_table >>= fun icompany_table ->
          let _ = printf "company table added\n" in
          let _ = flush_all() in 

           let ise_tree = transform_tree (module T) 
                           (module ISettlementTable)
                           (fun iwt -> ISettlementTable iwt) in
          let module ISettlementTree = 
              (val ise_tree : ISETTLEMENT_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ISettlementTable) (module ISettlementTree)
                  adt.settlement_table >>= fun isettlement_table ->
          let _ = printf "settlement table added\n" in
          let _ = flush_all() in

           let is_tree = transform_tree (module T) 
                           (module ISecurityTable)
                           (fun iwt -> ISecurityTable iwt) in
          let module ISecurityTree = 
              (val is_tree : ISECURITY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ISecurityTable) (module ISecurityTree)
                  adt.security_table >>= fun isecurity_table ->
          let _ = printf "security table added\n" in
          let _ = flush_all() in 

           let icom_tree = transform_tree (module T) 
                           (module ICommissionRateTable)
                           (fun iwt -> ICommissionRateTable iwt) in
          let module ICommissionRateTree = 
              (val icom_tree : ICOMMISSIONRATE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ICommissionRateTable) (module ICommissionRateTree)
                  adt.commission_rate_table >>= fun icommissionrate_table ->
          let _ = printf "commission table added\n" in
          let _ = flush_all() in 

            let ith_tree = transform_tree (module T) 
                           (module ITradeHistoryTable)
                           (fun iwt -> ITradeHistoryTable iwt) in
          let module ITradeHistoryTree = 
              (val ith_tree : ITRADEHISTORY_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ITradeHistoryTable) (module ITradeHistoryTree)
                  adt.trade_history_table >>= fun itradehistory_table ->
          let _ = printf "trade history table added\n" in
          let _ = flush_all() in 

            let iap_tree = transform_tree (module T) 
                           (module IAccountPermissionTable)
                           (fun iwt -> IAccountPermissionTable iwt) in
          let module IAccountPermissionTree = 
              (val iap_tree : IACCOUNTPERMISSION_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IAccountPermissionTable) (module IAccountPermissionTree)
                  adt.account_permission_table >>= fun iaccountpermission_table ->
          let _ = printf "permission table added\n" in
          let _ = flush_all() in 

            let icom_tree = transform_tree (module T) 
                           (module ITaxRateTable)
                           (fun iwt -> ITaxRateTable iwt) in
          let module ITaxRateTree = 
              (val icom_tree : ITAXRATE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ITaxRateTable) (module ITaxRateTree)
                  adt.tax_rate_table >>= fun itaxrate_table ->
          let _ = printf "tax rate table added\n" in
          let _ = flush_all() in 

               let ilt_tree = transform_tree (module T) 
                           (module ILastTradeTable)
                           (fun iwt -> ILastTradeTable iwt) in
          let module ILastTradeTree = 
              (val ilt_tree : ILASTRADE_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module ILastTradeTable) (module ILastTradeTree)
                  adt.last_trade_table >>= fun ilasttrade_table ->
          let _ = printf "last trade table added\n" in
          let _ = flush_all() in

               let ib_tree = transform_tree (module T) 
                           (module IBrokerTable)
                           (fun iwt -> IBrokerTable iwt) in
          let module IBrokerTree = 
              (val ib_tree : IBROKER_TREE with type t=T.t 
                                               and type tag=T.tag) in
          of_vadt (module IBrokerTable) (module IBrokerTree)
                  adt.broker_table >>= fun ibroker_table ->
          let _ = printf "broker table added\n" in
          let _ = flush_all() in 


          return @@ DB {customer_table=icustomer_table;
                        customer_account_table=icustomeraccount_table;
                        holding_table=iholding_table;
                        holding_history_table=iholdinghistory_table;
                        charge_table=icharge_table;
                        commission_rate_table=icommissionrate_table;
                        holding_summary_table=iholdingsummary_table;
                        broker_table=ibroker_table;
                        trade_table=itrade_table;
                        last_trade_table=ilasttrade_table;
                        trade_history_table=itradehistory_table;
                        trade_request_table=itraderequest_table;
                        trade_type_table=itradetype_table;
                        tax_rate_table=itaxrate_table;
                        settlement_table=isettlement_table;
                        exchange_table=iexchange_table;
                        company_table=icompany_table;
                        security_table=isecurity_table;
                        account_permission_table=iaccountpermission_table;}
        end


    let madt_to_adt (t:madt) : OM.db Lwt.t =
      ICustomerTable.to_adt t.customer_table >>= fun customer_table ->
      ICustomerAccountTable.to_adt t.customer_account_table >>= fun customeraccount_table -> 
      IHoldingTable.to_adt t.holding_table >>= fun holding_table ->
      IHoldingHistoryTable.to_adt t.holding_history_table >>= fun holdinghistory_table ->
      IChargeTable.to_adt t.charge_table >>= fun charge_table ->
      ICommissionRateTable.to_adt t.commission_rate_table >>= fun commissionrate_table ->
      IHoldingSummaryTable.to_adt t.holding_summary_table >>= fun holdingsummary_table ->
      IBrokerTable.to_adt t.broker_table >>= fun broker_table ->
      ITradeTable.to_adt t.trade_table >>= fun trade_table ->
      ILastTradeTable.to_adt t.last_trade_table >>= fun lasttrade_table ->
      ITradeHistoryTable.to_adt t.trade_history_table >>= fun tradehistory_table ->
      ITradeRequestTable.to_adt t.trade_request_table >>= fun traderequest_table ->
      ITradeTypeTable.to_adt t.trade_type_table >>= fun tradetype_table ->
      ITaxRateTable.to_adt t.tax_rate_table >>= fun taxrate_table ->
      ISettlementTable.to_adt t.settlement_table >>= fun settlement_table ->
      IExchangeTable.to_adt t.exchange_table >>= fun exchange_table ->
      ICompanyTable.to_adt t.company_table >>= fun company_table ->
      ISecurityTable.to_adt t.security_table >>= fun security_table ->
      IAccountPermissionTable.to_adt t.account_permission_table >>= fun accountpermission_table ->
      let open Tpce in
      Lwt.return @@ {customer_table=customer_table;
                     customer_account_table=customeraccount_table;
                     holding_table=holding_table;
                     holding_history_table=holdinghistory_table;
                     charge_table=charge_table;
                     commission_rate_table=commissionrate_table;
                     holding_summary_table=holdingsummary_table;
                     broker_table=broker_table;
                     trade_table=trade_table;
                     last_trade_table=lasttrade_table;
                     trade_history_table=tradehistory_table;
                     trade_request_table=traderequest_table;
                     trade_type_table=tradetype_table;
                     tax_rate_table=taxrate_table;
                     settlement_table=settlement_table;
                     exchange_table=exchange_table;
                     company_table=company_table;
                     security_table=security_table;
                     account_permission_table=accountpermission_table;}

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
