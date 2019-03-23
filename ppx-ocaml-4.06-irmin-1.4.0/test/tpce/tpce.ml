module Id = struct 
  type t = int64

  let t = Irmin.Type.int64

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare = Int64.compare

  let of_int = Int64.of_int

  let random ?x y = 
    let x = match x with 
      | Some x -> Int64.of_int x
      | None -> 0L in
    let y = Int64.of_int y in
    Int64.add (Random.int64 (Int64.sub y x)) x
end

type id = Id.t

let id = Id.t

let counter_merge lca v1 v2 = 
  let (+) a b = Int64.add a b in
  let (-) a b = Int64.sub a b in
  lca + (v1-lca) + (v2-lca)

let minf f l = 
  List.fold_left 
    (fun acc x -> match acc with
       | None -> Some x
       | Some x' -> if Int64.compare (f x) (f x') < 0 
                  then Some x else acc)
    None l

(* Information about the access the customer or an individual other than customer has to
   a given account. Customer account may have trades executed on them by more than one person *)
module AccountPermission = struct 
type t = {ap_ca_id : id; ap_tax_id : id}
let merge ~ancestor:lca v1 v2  =  
    failwith "account_permission is immutable"
end 

(* Information about the customer *)
(* c_id represents the customer identifier 
 * c_tier represents the tier number, tier1 customer are charged more fee 
   and tier3 customers are charged least fee *)
(* c_tier is merged using the counter merge *)
(* c_tax_id is used as tax identifier *)
module Customer = struct
type t = {c_id : id; 
          c_tax_id : id; 
          c_tier : id}
let merge ~ancestor:lca v1 v2  =  
    failwith "account_permission is immutable"

end 

(* Account infomration of each customer *)
(* ca_id represents the customer account identifier *)
(* ca_b_id represents the broker identifer who manages this customer account *)
(* ca_c_id represents the customer identifier who owns this account *)
(* ca_bal represents account's cash balance *)
module CustomerAccount = struct 
type t = {ca_id : id; 
          ca_b_id : id; 
          ca_c_id : id; 
          ca_bal : int64}
  let merge ~ancestor:lca {ca_bal=y1} {ca_bal=y2} =  
    {lca with ca_bal = (counter_merge lca.ca_bal y1 y2)}
end 

(* Information about customer's holdings *)
(* h_t_id is the trade identifier for the trade *)
(* h_ca_id is the customer account identifier *)
(* h_price is the unit purchase price of this security *)
(* h_qty is the quantity of the held trade *)
module Holding = struct
type t = {h_t_id: id; 
          h_ca_id: id; 
          h_s_symb : id; 
          h_price : int64; 
          h_qty: int64}
 let merge ~ancestor:{h_t_id; h_ca_id; h_s_symb; h_price = q;  h_qty=y} 
                     {h_price=q1; h_qty = y1}
                     {h_price= q2; h_qty = y2} =  
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    {h_t_id; h_ca_id; h_s_symb; h_price = q'; h_qty = y'}
end 

(* Information about holding positions that were inserted, updated or deleted 
   and which trade made these changes *)
(* hh_h_t_id is the trade identifier of the trade that originally created the holding row *)
(* hh_t_id is the trade identifier of the current trade (the one that last inserted, deleted 
   or updated the holding identified by hh_h_t_id) *)
(* hh_b_qty is the quantity of the security held before the modifying trade. 
   Initially it is 0 *)
(* hh_a_qty is the quantity of the security held after modifying trade. 
   If holding gets deleted by the modifying trade then it is 0 *)
module HoldingHistory = struct 
type t = {hh_h_t_id : id; 
          hh_t_id : id; 
          hh_b_qty : int64; 
          hh_a_qty : int64}
let merge ~ancestor:{hh_h_t_id; hh_t_id; hh_b_qty=q; 
                       hh_a_qty=y}
          {hh_a_qty=q1; hh_b_qty=y1}
          {hh_a_qty=q2; hh_b_qty=y2} =
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    {hh_h_t_id; hh_t_id; hh_b_qty=q'; hh_a_qty=y'}
end

(* Aggregate information about the customer account's security holdings *)
(* hs_ca_id is the customer account identifier *)
(* hs_s_symb is the symbol for security held *)
(* hs_qty is the total quantity of this security held *)
module HoldingSummary = struct 
type t ={hs_ca_id : id; 
         hs_s_symb : id ; 
         hs_qty: int64}
 let merge ~ancestor:lca {hs_qty=y1} {hs_qty=y2} =  
    {lca with hs_qty = (counter_merge lca.hs_qty y1 y2)}
end 

(* Information about the broker *)
(* b_id is the broker identifier *)
(* b_num_trades is the number of trades this broker has executed so far *)
(* b_comm is the commission broker has earned so far *)
module Broker = struct 
type t = {b_id : id; 
          b_num_trades : int64; 
          b_comm : int64}
 let merge ~ancestor:{b_id; b_num_trades = q; b_comm=y} 
                    {b_num_trades = q1; b_comm=y1}
                    {b_num_trades = q2; b_comm= y2} = 
    let q' = counter_merge q q1 q2 in 
    let y' = counter_merge y y1 y2 in  
    {b_id; b_num_trades = q'; b_comm = y'}
end 

(* Charges for placing a trade request *)
(* Charges are based on the customer's tier and the trade type *)
module Charge = struct 
type t = {ch_tt_id : id; 
          ch_c_tier : id; 
          ch_chrg : int64}
let merge ~ancestor:lca {ch_chrg=y1} {ch_chrg=y2} =  
    {lca with ch_chrg = (counter_merge lca.ch_chrg y1 y2)}
end 

(* Commission rate depends on several factors like the tier in which customer is in , 
   the type of trade and the quantity of securities traded *)
(* cr_c_tier reppresents the customer's tier *)
(* cr_t_id represents the trade identifier *)
(* cr_from_qty represents the lower bound of quantity being traded to match this commission rate *)
(* cr_to_qty represents the higher bound of quantity being traded to match this commission rate *)
(* cr_rate represents the commission rate. Ranges from 0 to 100. 10% is 10.00 *)
(* cr_ex_id represents the exchange that executed the trade *)
module CommissionRate = struct 
type t = {cr_c_tier : id; 
          cr_tt_id : id; 
          cr_ex_id : id; 
          cr_from_qty : int64; 
          cr_to_qty : int64; 
          cr_rate : int64}
let merge ~ancestor:{cr_c_tier; cr_tt_id; cr_ex_id; cr_from_qty = q; cr_to_qty=y; cr_rate=z} 
                    {cr_from_qty = q1; cr_to_qty=y1; cr_rate= z1}
                    {cr_from_qty = q2; cr_to_qty=y2; cr_rate= z2} = 
    let q' = counter_merge q q1 q2 in 
    let y' = counter_merge y y1 y2 in 
    let z' = counter_merge z z1 z2 in 
    {cr_c_tier; cr_tt_id; cr_ex_id; cr_from_qty = q'; cr_to_qty = y'; cr_rate = z'}
end 

(* Settlement contains information about how trades are settled 
   Whether the settlement is on cash or in margin and when the settlement is due *)
(* se_cash_type is false if margin and true if cash account *)
module Settlement = struct 
type t = {se_t_id : id ; 
          se_cash_type : bool; 
          se_amt: int64}
 let merge ~ancestor:lca {se_amt=y1} {se_amt=y2} =  
    {lca with se_amt = (counter_merge lca.se_amt y1 y2)}
end 

(* Information about the trade *)
(* t_id is the trade identifier *)
(* t_ca_id is the customer identifier *)
(* t_qty is the quantities of securities traded *)
(* t_trade_price is the unit price at which the security was traded *)
(* t_chrg is the fee charged for placing this trade request *)
(* t_comm is the commission earned on this trade *)
(* t_is_sell is 1 if this is a sell type transaction and is 0 if this is a buy type transaction *)
(* t_is_cash is true if it is cash type and false if it is margin type *)
(* t_dts is date and time of trade *)
(* t_exec_name is the person executing the trade *)
module Trade = struct 
type t = {t_id : id; 
          t_ca_id : id; 
          tt_id : id; 
          t_is_cash : bool; 
          t_qty : int64; 
          t_symb : id; 
          t_exec_name : string;
          t_trade_price : int64; 
          t_dts : float option; 
          t_chrg : int64; 
          t_comm : int64}
  let merge ~ancestor:lca {t_dts=d1; t_comm= y1} {t_dts=d2; t_comm=y2} = 
    let d = match d1,d2 with 
      | None,None -> None 
      | Some d1,None -> Some d1 
      | None,Some d2 -> Some d2
      | Some d1, Some d2 -> Some (min d1 d2) in
    let y' = counter_merge lca.t_comm y1 y2 in 
    {lca with t_dts=d;
              t_comm=y'}
end 

(* Contains information about pending limit trades that are waiting, for a certain security price 
   before the trades are submitted to the market *)
(* tr_t_id is trade request identifier. This value is used to process the pending requests when the price reaches the desired 
   price *)
(* tr_qty is the quantity of the security the customer had requested to trade *)
(* tr_bid_price is the price per unit the customer wants for the security *)
(* tr_b_id is the broker handling the request *)
(* tr_s_symb is the security symbol which the customer wants to trade *)
module TradeRequest = struct 
type t = {tr_t_id: id; 
          tr_tt_id: id; 
          tr_b_id : id; 
          tr_s_symb : id; 
          tr_qty : int64; 
          tr_bid_price : int64}
 let merge ~ancestor:{tr_t_id; tr_tt_id; tr_b_id; tr_s_symb; tr_qty = q; tr_bid_price =y} 
                    {tr_qty = q1; tr_bid_price=y1}
                    {tr_qty = q2; tr_bid_price= y2} = 
    let q' = counter_merge q q1 q2 in 
    let y' = counter_merge y y1 y2 in  
    {tr_t_id; tr_tt_id; tr_b_id; tr_s_symb; tr_qty = q'; tr_bid_price = y'}
end 

(* Contains information about the history of each trade transaction *)
(* th_t_id is the trade identifier *)
module TradeHistory = struct 
type t = {th_t_id: id; 
          th_dts: float option}
  let merge ~ancestor:lca {th_dts=d1} {th_dts=d2} = 
    let d = match d1,d2 with 
      | None,None -> None 
      | Some d1,None -> Some d1 
      | None,Some d2 -> Some d2
      | Some d1, Some d2 -> Some (min d1 d2) in
    {lca with th_dts=d}
end 

(* Trade type *)
(* tt_is_sell is 1 if it is a sell type and 0 if it is a buy type *)
(* tt_market is 1 if market transaction and 0 if it is limit transaction *)
module TradeType = struct 
type t = {tt_id: id; 
          tt_is_sell:int64; 
          tt_is_market:int64}
 let merge ~ancestor:{tt_id; tt_is_sell = q;  tt_is_market=y} 
                     {tt_is_sell=q1; tt_is_market = y1}
                     {tt_is_sell= q2; tt_is_market = y2} =  
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    {tt_id; tt_is_sell=q'; tt_is_market = y'}
end 

(* One row for each security with the latest trade price and
   security symbol *)
module LastTrade = struct
type t = {lt_s_symb : id; 
          lt_price : int64; 
          lt_dts : float option; 
          lt_vol : int64}
  let merge ~ancestor:lca {lt_dts=d1; lt_price= y1; lt_vol=z1} {lt_dts=d2; lt_price=y2; lt_vol=z2} = 
    let d = match d1,d2 with 
      | None,None -> None 
      | Some d1,None -> Some d1 
      | None,Some d2 -> Some d2
      | Some d1, Some d2 -> Some (min d1 d2) in
    let y' = counter_merge lca.lt_price y1 y2 in 
    let z' = counter_merge lca.lt_vol z1 z2 in 
    {lca with lt_dts=d;
              lt_price=y';
              lt_vol = z'}
end  

(* Each security information *)
module Security = struct 
type t = {s_symb : id; 
          s_ex_id : id; 
          s_co_id : id}
let merge ~ancestor:lca v1 v2  =  
    failwith "account_permission is immutable"
end 


(* co_id is the company identifier *)
module Company = struct 
type t = {co_id : id}
let merge ~ancestor:lca v1 v2  =  
    failwith "account_permission is immutable"
end 

module Exchange = struct 
type t = {ex_id : id}
let merge ~ancestor:lca v1 v2  =  
    failwith "account_permission is immutable"
end 

module TaxRate = struct 
type t = {tax_id : id; 
          tx_rate:int64}
 let merge ~ancestor:lca {tx_rate=y1} {tx_rate=y2} =  
    {lca with tx_rate = (counter_merge lca.tx_rate y1 y2)}
end 


module IdPair = struct 
  type t = id*id

  let t = let open Irmin.Type in pair id id

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare (x1,y1) (x2,y2) = 
    match Id.compare x1 x2, Id.compare y1 y2 with
      | 0, v2 -> v2
      | v1, _ -> v1
end

module IdTriple = struct 
  type t = id*(id*id)

  let t = let open Irmin.Type in pair id (pair id id)

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare (x1,(y1,z1)) (x2,(y2,z2)) = 
    match Id.compare x1 x2, Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, v3 -> v3
      | 0, v2, _ -> v2
      | v1, _, _ -> v1
end

module IdQuad = struct 
  type t = id*(id*(id*id))

  let t = let open Irmin.Type in pair id (pair id (pair id id))

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare (w1,(x1,(y1,z1))) (w2,(x2,(y2,z2))) = 
    match Id.compare w1 w2, Id.compare x1 x2, 
          Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, 0, v4 -> v4
      | 0, 0, v3, _ -> v3
      | 0, v2, _, _ -> v2
      | v1, _, _, _ -> v1
end

module IdQuin = struct 
  type t = id*(id*(id*(id*id)))

  let t = let open Irmin.Type in 
    (pair id (pair id (pair id (pair id id))))

  let to_string : t -> string = 
    Fmt.to_to_string (Irmin.Type.pp_json t)

  let compare (u1,(w1,(x1,(y1,z1)))) (u2, (w2,(x2,(y2,z2)))) = 
    match Id.compare u1 u2, Id.compare w1 w2, Id.compare x1 x2, 
          Id.compare y1 y2, Id.compare z1 z2 with
      | 0, 0, 0, 0, v5 -> v5
      | 0, 0, 0, v4, _ -> v4
      | 0, 0, v3, _, _ -> v3
      | 0, v2, _, _, _ -> v2
      | v1, _, _, _, _ -> v1
end

(* Contains all the customers where each row represents one customer *)
(* Each customer consists of fields like customer id and customer tier *)
(* Its like a table with each row consisting representing customer and 
   its columns are customer id and customer tier *)
module CustomerTable = Rbmap.Make(Id)(Customer)

(* Each row corresponds to account associated with each customer *)
module CustomerAccountTable = Rbmap.Make(IdTriple)(CustomerAccount)

module HoldingTable = Rbmap.Make(IdTriple)(Holding)

module HoldingHistoryTable = Rbmap.Make(IdPair)(HoldingHistory)

module ChargeTable = Rbmap.Make(Id)(Charge)

module CommissionRateTable = Rbmap.Make(IdTriple)(CommissionRate)

module HoldingSummaryTable = Rbmap.Make(IdPair)(HoldingSummary)

(* Each row corresponds to one broker and there is one broker 
   serving around 100 customers *)
module BrokerTable = Rbmap.Make(Id)(Broker)

(* Each row corresponds to a trade *)
module TradeTable = Rbmap.Make(IdQuad)(Trade)

(* Each row corresponds to each security with the latest trade price *)
module LastTradeTable = Rbmap.Make(Id)(LastTrade)

(* Each row corresponds to each trade history *)
module TradeHistoryTable = Rbmap.Make(Id)(TradeHistory)

(* Each row corresponds to each trade request *)
module TradeRequestTable = Rbmap.Make(IdQuad)(TradeRequest)

module TaxRateTable = Rbmap.Make(Id)(TaxRate)

module ExchangeTable = Rbmap.Make(Id)(Exchange)

module CompanyTable = Rbmap.Make(Id)(Company)

module SecurityTable = Rbmap.Make(Id)(Security)

module TradeTypeTable = Rbmap.Make(Id)(TradeType)

module SettlementTable = Rbmap.Make(Id)(Settlement)

module AccountPermissionTable = Rbmap.Make(Id)(AccountPermission)


(* db is a database consisting of columns for each type.
   It is a collection of tables representing each type. *)
type db = {customer_table: CustomerTable.t; 
           customer_account_table: CustomerAccountTable.t;
           holding_table: HoldingTable.t;
           holding_history_table : HoldingHistoryTable.t;
           charge_table : ChargeTable.t;
           commission_rate_table : CommissionRateTable.t;
           holding_summary_table: HoldingSummaryTable.t;
           broker_table: BrokerTable.t;
           trade_table: TradeTable.t;
           last_trade_table : LastTradeTable.t;
           trade_history_table : TradeHistoryTable.t;
           trade_request_table : TradeRequestTable.t;
           trade_type_table : TradeTypeTable.t;
           tax_rate_table : TaxRateTable.t;
           settlement_table : SettlementTable.t;
           exchange_table : ExchangeTable.t;
           company_table : CompanyTable.t;
           security_table : SecurityTable.t;
           account_permission_table : AccountPermissionTable.t}

open Customer 
open CustomerAccount
open HoldingTable
open HoldingSummary
open Broker
open Trade 
open Holding
open HoldingHistory
open CommissionRate
open Charge 
open TradeHistory
open TradeRequest
open Exchange 
open Company
open Settlement 
open TaxRate 
open TradeType
open Security 
open AccountPermission

(* type 'a t takes a database db and returns a pair of result and the updated db *)
module Txn = struct
  type 'a t = db -> 'a*db

  let bind m f = fun db ->
    let (a,db') = m db in
    f a db'

  let return a = fun db -> (a,db)
end

module Select1 = struct
  open Printf

  (* customer_table db contains all the customers in the market
     we select the customer with c_id as x in the customer table present 
     in the database db *)
  let customer_table (x) db =
    try
      let t = db.customer_table in
      let res = CustomerTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Customer<%s> not found"
        (Id.to_string (x))

  (* db is the table containing all the types
     we select the customer account belonging to customer y and  
     whose customer account id is x and the broker taking care of this account is z *)
  let customer_account_table (x,y,z) db =
    try
      let t = db.customer_account_table in
      let res = CustomerAccountTable.find (x,(y,z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "CustomerAccount<%s> not found"
        (IdTriple.to_string (x,(y,z)))

  (* db is the database 
     we want to select the holding having holding id as x
     and customer account id as y *)
  let holding_table (x,y,z) db =
    try
      let t = db.holding_table in
      let res = HoldingTable.find (x,(y,z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Holding<%s> not found"
        (IdTriple.to_string (x, (y,z)))
  
  (* db is the database where we first select the holding histoty table 
     and then retrieve the holding history with holding initiated trade as x 
   and current trade changing the holding as y *)
  let holding_history_table (x, y) db =
    try
      let t = db.holding_history_table in
      let res = HoldingHistoryTable.find (x, y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "HoldingHistory<%s> not found"
        (IdPair.to_string (x, y))

  let holding_summary_table (x, y) db =
    try
      let t = db.holding_summary_table in
      let res = HoldingSummaryTable.find (x, y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Holdingsummary<%s> not found"
        (IdPair.to_string (x, y))

  (* Select the broker with broker id as x in the broker table *)
  let broker_table (x) db =
    try
      let t = db.broker_table in
      let res = BrokerTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Broker<%s> not found"
        (Id.to_string (x))

  (* select the trade with trade id as x and customer account id as y *)
  let trade_table (x,y, z, w) db =
    try
      let t = db.trade_table in
      let res = TradeTable.find (x,(y, (z, (w)))) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Customer<%s> not found"
        (IdQuad.to_string (x,(y, (z, (w)))))

    (* select the charge with trade id as x and customer tier as y *)
  let charge_table (x) db =
    try
      let t = db.charge_table in
      let res = ChargeTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Charge<%s> not found"
        (Id.to_string (x))

      (* select the commission rate table  with trade id as x and customer tier as y *)
  let commission_rate_table (x,(y, z)) db =
    try
      let t = db.commission_rate_table in
      let res = CommissionRateTable.find (x,(y, z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Charge<%s> not found"
        (IdTriple.to_string (x,(y, z)))

  let last_trade_table (x) db =
    try
      let t = db.last_trade_table in
      let res = LastTradeTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Charge<%s> not found"
        (Id.to_string (x))


  let trade_type_table (x) db =
    try
      let t = db.trade_type_table in
      let res = TradeTypeTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Trade Type <%s> not found"
        (Id.to_string (x))


  let settlement_table (x) db =
    try
      let t = db.settlement_table in
      let res = SettlementTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Settlement <%s> not found"
        (Id.to_string (x))

    let trade_history_table (x) db =
    try
      let t = db.trade_history_table in
      let res = TradeHistoryTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Trade histoty <%s> not found"
        (Id.to_string (x))

    let company_table (x) db =
    try
      let t = db.company_table in
      let res = CompanyTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Company Table <%s> not found"
        (Id.to_string (x))

  let security_table (x) db =
    try
      let t = db.security_table in
      let res = SecurityTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Security Table <%s> not found"
        (Id.to_string (x))

    let account_permission_table (x) db =
    try
      let t = db.account_permission_table in
      let res = AccountPermissionTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "AP <%s> not found"
        (Id.to_string (x))

      let tax_rate_table (x) db =
    try
      let t = db.tax_rate_table in
      let res = TaxRateTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "AP <%s> not found"
        (Id.to_string (x))
end

(* Select selects a list of rows in any particular table *)
module Select = struct
   
  let customer_table sigf db =
    let t = db.customer_table in
    let res = CustomerTable.select sigf t in
    (res, db)

  let customer_account_table sigf db =
    let t = db.customer_account_table in
    let res = CustomerAccountTable.select sigf t in
    (res, db)

  let holding_table sigf db =
    let t = db.holding_table in
    let res = HoldingTable.select sigf t in
    (res, db)

  let holding_history_table sigf db =
    let t = db.holding_history_table in
    let res = HoldingHistoryTable.select sigf t in
    (res, db)

  let holding_summary_table sigf db =
    let t = db.holding_summary_table in
    let res = HoldingSummaryTable.select sigf t in
    (res, db)

  let broker_table sigf db =
    let t = db.broker_table in
    let res = BrokerTable.select sigf t in
    (res, db)

  let trade_table sigf db =
    let t = db.trade_table in
    let res = TradeTable.select sigf t in
    (res, db)

  let trade_request_table sigf db =
    let t = db.trade_request_table in
    let res = TradeRequestTable.select sigf t in
    (res, db)

  let trade_type_table sigf db =
    let t = db.trade_type_table in
    let res = TradeTypeTable.select sigf t in
    (res, db)

    let security_table sigf db =
    let t = db.security_table in
    let res = SecurityTable.select sigf t in
    (res, db)

  let commission_rate_table sigf db =
    let t = db.commission_rate_table in
    let res = CommissionRateTable.select sigf t in
    (res, db)

end

module Insert = struct
  let holding_summary_table hs db = 
    let open HoldingSummary in
    let t = db.holding_summary_table in
    let t'= HoldingSummaryTable.insert (hs.hs_ca_id, hs.hs_s_symb) hs t in
        ((),{db with holding_summary_table=t'})


  let holding_history_table hs db = 
    let open HoldingHistory in
    let t = db.holding_history_table in
    let t'= HoldingHistoryTable.insert (hs.hh_h_t_id, hs.hh_t_id) hs t in
        ((),{db with holding_history_table=t'})

  let holding_table hs db = 
    let open Holding in
    let t = db.holding_table in
    let t'= HoldingTable.insert (hs.h_t_id, (hs.h_ca_id, hs.h_s_symb)) hs t in
        ((),{db with holding_table=t'})

    let trade_history_table th db = 
    let open TradeHistory in
    let t = db.trade_history_table in
    let t'= TradeHistoryTable.insert (th.th_t_id) th t in
        ((),{db with trade_history_table=t'})

  let trade_table th db = 
    let open Trade in
    let t = db.trade_table in
    let t'= TradeTable.insert (th.t_id, (th.t_ca_id, (th.tt_id, (th.t_symb)))) th t in
        ((),{db with trade_table=t'})

    let trade_request_table th db = 
    let open TradeRequest in
    let t = db.trade_request_table in
    let t'= TradeRequestTable.insert (th.tr_t_id, (th.tr_tt_id, (th.tr_b_id, (th.tr_s_symb)))) th t in
        ((),{db with trade_request_table=t'})

  let settlement_table th db = 
    let open Settlement in
    let t = db.settlement_table in
    let t'= SettlementTable.insert (th.se_t_id) th t in
        ((),{db with settlement_table=t'})

  let customer_table th db = 
    let open Customer in
    let t = db.customer_table in
    let t'= CustomerTable.insert (th.c_id) th t in
        ((),{db with customer_table=t'})

   let customer_account_table th db = 
    let open CustomerAccount in
    let t = db.customer_account_table in
    let t'= CustomerAccountTable.insert (th.ca_id, (th.ca_b_id, th.ca_c_id)) th t in
        ((),{db with customer_account_table=t'})
end 

module Update = struct
let holding_summary_table sigf updf db = 
    let t = db.holding_summary_table in
    let t' = HoldingSummaryTable.update sigf updf t in
    ((), {db with holding_summary_table=t'})

let holding_table sigf updf db = 
    let t = db.holding_table in
    let t' = HoldingTable.update sigf updf t in
    ((), {db with holding_table=t'})

let trade_table sigf updf db = 
    let t = db.trade_table in
    let t' = TradeTable.update sigf updf t in
    ((), {db with trade_table=t'})

let broker_table sigf updf db = 
    let t = db.broker_table in
    let t' = BrokerTable.update sigf updf t in
    ((), {db with broker_table=t'})

let customer_account_table sigf updf db = 
    let t = db.customer_account_table in
    let t' = CustomerAccountTable.update sigf updf t in
    ((), {db with customer_account_table=t'})


let last_trade_table sigf updf db = 
    let t = db.last_trade_table in
    let t' = LastTradeTable.update sigf updf t in
    ((), {db with last_trade_table=t'})

let settlement_table sigf updf db = 
    let t = db.settlement_table in
    let t' = SettlementTable.update sigf updf t in
    ((), {db with settlement_table=t'})

end 

module Delete = struct
  let holding_table (h_t_id, h_ca_id, h_symb) db =
    let t = db.holding_table in
    let t' = HoldingTable.remove (h_t_id, (h_ca_id, h_symb)) t in
    ((), {db with holding_table=t'})

    let holding_summary_table (hs_ca_id, hs_s_symb) db =
    let t = db.holding_summary_table in
    let t' = HoldingSummaryTable.remove (hs_ca_id, hs_s_symb) t in
    ((), {db with holding_summary_table=t'})
    
  let trade_request_table (trtid) db =
    let t = db.holding_table in
    let t' = HoldingTable.remove (trtid) t in
    ((), {db with holding_table=t'})

end

module Temp = struct
  let (+) = Int64.add

  let (-) = Int64.sub

  let ( * ) = Int64.mul

  let (>>=) = Txn.bind
end
 
open Temp
open Printf
(* TRANSACTIONS *)

(* Market-feed Transaction *)
(* The process of tracking the current market activity *)
(* Representative of brokage house processing the "ticker-tape" from the market exchange *)
(* Receives the latest trade activity information from market exchnage and update rest of the table *)
(* Trade request table consist of information about pending limit trades that is waiting for certain price *)

let rec market_feed_txn lt_s_symb t_price tr_qty type_limit_sell type_limit_buy =
  let now_dts = Some (Unix.time()) in 
  (* Finds the symbol and modify its row in the last trade table with the new price,
     quantity traded and also modifies the last trade date *)
  Update.last_trade_table (fun ltsymb -> Id.compare ltsymb lt_s_symb) 
                          (fun lt -> {lt with lt_price = t_price; (* latest trade price for this security *)
                                      lt_vol = lt.lt_vol + tr_qty; (* volume of trading on the market for this security so far *)
                                      lt_dts = now_dts} (*time when this row was last updated *)) >>= fun ()->
  (* Once we have the information about the last trade then we need to update the pending trade requests *)
  Select.trade_request_table (fun (_, (trttid, (_, (trsymb)))) ->
                                (IdPair.compare (trttid, trsymb) (type_limit_sell, lt_s_symb))) >>= fun trs1 ->
  Select.trade_request_table (fun (_, (trttid, (_, (trsymb)))) ->
                                (IdPair.compare (trttid, trsymb) (type_limit_buy, lt_s_symb))) >>= fun trs2 ->
  let trs_to_filter = List.append trs1 trs2 in
   (* cheking whether the price customer wants (bid price)
      is more then last trade price in case of type_stop_loss
      and is less in case of limit sell. If customer wants to sell his shares than he wants to check that 
      the bid price is less than the current market price. If customer wants to buy than he wants to 
      check that the bid price is more than the market price *)
   (* Overview of what is going on:
      - If customer has set a bid price for himself (the price per unit security that he wants to trade)
        and the bid price is less than the current market last trade price then he will sell it.
        Because he will get profit in selling his security because market price is more than the 
        price at which he planned to sell his security.
      - If customer has set a bid price for himself (the price per unit security that he wants to trade)
        and the bid price is more than the current market last trade price then he will buy it.
        Because he will get profit in buying the security because market price is low as compared 
        to the price he planned to buy the security *)
  let rec filter_trs_on_bid_price trs = match trs with
    | [] -> []
    | x :: xl -> if (x.tr_tt_id = type_limit_sell && x.tr_bid_price <= t_price) ||
                    (x.tr_tt_id = type_limit_buy && x.tr_bid_price >= t_price) then 
                    x :: filter_trs_on_bid_price xl else
                    filter_trs_on_bid_price xl in 
  (* trs is the trade requests where customer is in profit *)
  let trs = filter_trs_on_bid_price trs_to_filter in 
  (* Now we update the corresponding trades in the trade table with the recent time 
            delete the corresponding trade request 
            insert those trades in the trade history *)
  List.fold_left (fun pre x -> pre >>= fun () ->
                           Update.trade_table 
                           (fun (tid, (_,(_,(_)))) -> Id.compare tid (x.tr_t_id))
                           (fun t -> {t with t_dts = now_dts}) >>= fun () ->
                           Delete.trade_request_table (x.tr_t_id, (x.tr_tt_id, x.tr_b_id)) >>= fun () ->
                           let th1 = {th_t_id = x.tr_t_id; th_dts = now_dts} in 
                           Insert.trade_history_table th1) (Txn.return ()) trs 




(* Trade-Update Transaction *)
(* Process the retrieval of any information requested by any customer or a broker *)
(* Gives information regarding a particular account, a trade transactions or a security *)
(* Basically it is like a statement about past transactiosn *)
(* First job done:
   For the trade id, information for trade is returned and the 
   executor's name for trade is modified *)
let rec trade_update_txn job_to_be_done tid tcaid ttid tsymb exec_name start_t end_t = 
  if (job_to_be_done = 1) then 
   let change_exec_name ex = match ex with 
    |"some _ X some _" -> "some _  some _"
    | _ -> "some _ X some _" in 
      let tr = Select1.trade_table (tid, tcaid, ttid, tsymb) in 
      let _ =  Update.trade_table 
                    (fun (trid, (_,(_,(_)))) -> Id.compare trid (tid))
                    (fun t -> {t with t_exec_name = change_exec_name t.t_exec_name}) in 
                    (* select the corresponding trade only one row *)
      let tr'  = Select1.trade_table (tid, 
                                      tcaid, 
                                      ttid, 
                                      tsymb) in 
      (* select the corresponding trade type only one row *)
      let trty = Select1.trade_type_table (ttid) in  
      (* select settlement information for the trade *)
      let st = Select1.settlement_table (tid) in 
      (* get trade history for the trade *)
      let th = Select1.trade_history_table (tid) in 
      Txn.return()
(* Second job is done *)
  else if (job_to_be_done = 2) then 
        Select.trade_table (fun (_, (tacid, (_, _))) -> (Id.compare tacid tcaid)) >>= fun trs ->
        let rec filter_trs_time_based trs1 = match (trs1) with
          | [] -> []
          | x :: xl -> if x.t_dts >= start_t && x.t_dts <= end_t 
                       then x :: filter_trs_time_based xl 
                       else filter_trs_time_based xl in 
        let trs' = filter_trs_time_based trs in 
        List.fold_left (fun pre x -> pre >>= fun () ->
                            Select1.settlement_table (x.t_id) >>= fun st ->
                            let cash_type = if (x.t_is_cash) 
                             then true
                             else false in 
                            Select1.trade_history_table (x.t_id) >>= fun th ->
                            Update.settlement_table (fun seid -> Id.compare seid x.t_id)
                                                    (fun st -> {st with se_cash_type = cash_type})) 
                            (Txn.return()) trs'
else if (job_to_be_done = 3) then 
     Select.trade_table (fun (_, (tacid, (_, tsymbl))) -> 
     (IdPair.compare (tacid, tsymb) (tcaid, tsymb))) >>= fun trs -> 
     let rec filter_trs_time_based trs1 = match (trs1) with
          | [] -> []
          | x :: xl -> if x.t_dts >= start_t && x.t_dts <= end_t 
                       then x :: filter_trs_time_based xl 
                       else filter_trs_time_based xl in 
        let trs' = filter_trs_time_based trs in 
     Select.trade_type_table (fun t -> Id.compare t ttid) >>= fun ttys ->
     Select.security_table (fun s -> Id.compare s tsymb) >>= fun sec ->
     List.fold_left (fun pre x -> pre >>= fun () ->
                            Select1.settlement_table (x.t_id) >>= fun st ->
                            Select1.trade_history_table (x.t_id) >>= fun th ->
                            Txn.return())
                     (Txn.return()) trs'
    else Txn.return()




(* Trade-cleanup Transaction *)
(* Used to cancel any trade in requested trade list from the database *)
let trade_cleanup_txn tid trtid = 
  let now_dts = Some (Unix.time()) in
  (* Get all pending trade requests *)
  Select.trade_request_table (fun (t, (_, (_, (_)))) ->
                                (Id.compare (t) (trtid))) >>= fun trs1 ->
  (* Insert all the trades into trade history and delete all the pending trades *)
  List.fold_left (fun pre x -> pre >>= fun () ->
                               let th = {th_t_id = trtid;
                                         th_dts = now_dts} in 
                               let _ = Insert.trade_history_table th in 
                               Update.trade_table (fun (t, (_, (_, _))) -> Id.compare t trtid)
                                                  (fun l -> {l with t_dts = now_dts}) >>= fun () ->
                               Delete.trade_request_table (x.tr_t_id, (x.tr_tt_id, x.tr_b_id)))
                 (Txn.return()) trs1




(* Trade-Order Transaction *)
(* Buy or sell of a trade by a customer or brokage *)
(* Allows the person trading to execute buys or sells at current market price *)
(* Customer places a request on the brokage house to initiate a trade *)
let trade_order_txn acctid bid cid coid exec_tax_id symbol tid trade_type_id requested_price trade_qty exec_name = 
  (* Retrieving the information about customer, customer account and broker who are involved in this trade *)
  let buy_value = ref (Int64.of_int 0) in 
  let sell_value = ref (Int64.of_int 0) in 
  let needed_quantity = ref trade_qty in 
  let ca = Select1.customer_account_table (acctid, bid, cid) in 
  Select1.customer_table (cid) >>= fun c ->
  let ctaxid = c.c_tax_id in 
  let b = Select1.broker_table (bid) in 
  (* check on whether the executor tax id matches the customer's tax id or not
     if it matches then order can be possible else it is not possible *)
  (* Here exec_tax_id is the tax identifier for the person executing the trade *)
  (* Also checks in the account permission, whether the executor id is same as 
     account permission tax id associated with the account acctid *)
  if (exec_tax_id != ctaxid) then Txn.return()
    (* Estimating the overall impact of executing the requested trade *)
    else Select1.account_permission_table (acctid) >>= fun ap ->
         if (ap.ap_tax_id != exec_tax_id) then Txn.return() else 
         (* Get information on security involved in the trade 
            and comapany handling the security *) 
         Select1.company_table coid >>= fun co ->
         Select1.security_table (symbol) >>= fun sec ->
         (* Getting the current market price from the last trade table *)
         Select1.last_trade_table (symbol) >>= fun lt ->
         let market_price = lt.lt_price in 
         (* Getting the trade type which indicates whether the sell is 
            market type or sell type or limit type *)
         Select1.trade_type_table (trade_type_id) >>= fun tty ->
         (* If the trade is market type that means buy or sell the 
            security immediately at the market price where 
            the market price is equal to the last traded price for that security *)
         Select1.holding_summary_table (acctid, symbol) >>= fun hs ->
         Select.holding_table (fun (htid, (hcaid, hsymb)) ->
                    IdTriple.compare (htid, (hcaid, hsymb)) (tid, (acctid, symbol))) >>= fun hos ->
         if (tty.tt_is_market = Int64.of_int 1) then 
         (* If it is not market order then we need to calculate the price *)
         (* Modifying the customer's holding to reflect the result of buy or sell trade *)
         (* if it is sell type means customer is requesting for selling the holdings *)
         if (tty.tt_is_sell = Int64.of_int 1) (* sell type *)
           then if (hs.hs_qty > Int64.of_int 0) then
                (* Calculating the profit or loss based on trading it *)
                let _ = (List.fold_left 
                               (fun pre x -> 
                                pre >>= fun () ->
                                if (x.h_qty > !needed_quantity) 
                                 (* This is the case where person have enough security to trade *)
                                 (* So only a portion of holds will be sold *)
                                 (* Selling price will depend on the requested price that is the current market price in this case *)
                                 then begin
                                      buy_value := Int64.add (!buy_value) (Int64.mul !needed_quantity x.h_price);
                                      sell_value := Int64.add (!sell_value) (Int64.mul !needed_quantity market_price);
                                      needed_quantity := Int64.of_int 0;
                                      Txn.return()
                                      end 
                                 else (* All holdings will be sold *)
                                      begin
                                      buy_value := Int64.add (!buy_value) (Int64.mul x.h_qty x.h_price);
                                      sell_value := Int64.add (!sell_value) (Int64.mul x.h_qty market_price);
                                      needed_quantity := Int64.sub !needed_quantity x.h_qty;
                                      Txn.return()
                                      end)
                                 (Txn.return()) hos) in 
                  ()
                                 (* If the needed quantity is still greater than 0 then the person will be liquidatng the 
                                    current holdings to meet what he wants to buy or sell *)
           else (* buy *)
               if (hs.hs_qty < Int64.of_int 0) then 
               (* Calculating the profit or loss based on trading it *)
                let _ = (List.fold_left (fun pre x -> pre >>= fun () ->
                                if (x.h_qty + !needed_quantity < Int64.of_int 0) 
                                 (* This is the case where person doesn't has enough security 
                                    so only a portion of this holding would be bought 
                                    He can only buy at the current market price which depends on the 
                                    last trade *)
                                 then begin
                                      sell_value := Int64.add (!sell_value) (Int64.mul !needed_quantity x.h_price);
                                      buy_value := Int64.add (!buy_value) (Int64.mul !needed_quantity market_price);
                                      needed_quantity := Int64.of_int 0;
                                      Txn.return()
                                      end 
                                 else (* All holdings will be bought *)
                                      begin
                                      x.h_qty = Int64.of_int (- (Int64.to_int (x.h_qty)));
                                      sell_value := Int64.add (!sell_value) (Int64.mul x.h_qty x.h_price);
                                      buy_value := Int64.add (!buy_value) (Int64.mul x.h_qty market_price);
                                      needed_quantity := Int64.sub !needed_quantity x.h_qty;
                                      Txn.return()
                                      end)
                                 (Txn.return()) hos) in ()
                 else ();
            if (sell_value > buy_value) then
            let _ = (let now_dts = Some (Unix.time()) in
            (* calculating the tax that can be incurred because of thsi trade *)
            Select1.tax_rate_table(c.c_tax_id) >>= fun tax ->
            let tax_amount = Int64.mul (Int64.sub !sell_value !buy_value) tax.tx_rate in 
            (* Calculating the commission rate and charge *)
            Select1.commission_rate_table (c.c_tier, (tid, exec_tax_id)) >>= fun comm ->
            Select1.charge_table (tid) >>= fun ch ->
            let comm_amount = ((Int64.div (comm.cr_rate) (Int64.of_int 100)) * ((trade_qty) * (market_price))) in 
            (* Insert a new row for this trade in the trade table *)
            (* This is a cash type *)
            let ntid = Random.int64 10000000000L in
            let new_tr= { t_id = ntid;
                          t_ca_id = acctid; 
                          tt_id = trade_type_id;
                          t_is_cash = true;
                          t_qty = trade_qty;
                          t_symb = symbol;
                          t_exec_name = exec_name;
                          t_trade_price = market_price;
                          t_dts = now_dts;
                          t_chrg = ch.ch_chrg;
                          t_comm = comm_amount} in 
            Insert.trade_table new_tr) in Txn.return()
        else (* limit order *) (* in this case the person waits for specific price *)
             (* So we add it in the trade request table *)
           let _ = (let nrtid = Random.int64 10000000000L in
           let now_dts = Some (Unix.time()) in 
           let new_treq = {tr_t_id = nrtid;
                           tr_tt_id = trade_type_id;
                           tr_b_id = bid;
                           tr_s_symb = symbol;
                           tr_qty = trade_qty;
                           tr_bid_price = requested_price} in 
           Insert.trade_request_table new_treq >>= fun () ->
           let new_th = {th_t_id = nrtid;
                         th_dts = now_dts} in 
           Insert.trade_history_table new_th) in Txn.return()
     


(* Trade-Result Transaction *)
(* Completing a stock market trade *)
(* Stock market trade processing is done through this transaction *)
(* Brokage house receiving information from the market exchange about the 
   final confirmation and price of the trade *)
(* Represents the final trade 
   - Customer's holding is updated to reflect that trade is completed 
   - Broker commission is updated *)
(* Trade id is passed as argument which is used to retrieve the information about the trade *)
(* Symbol represents the security symbol *)

let trade_result_txn tid caid ttid cid bid symbol trade_price = 
  (* Getting information on the trade and the customer's account *)
  (* Selecting the trade with id tid in the trade table *)
  Select1.trade_table (tid,caid, ttid, symbol) >>= fun t ->
  (* caid is the customer account id *)
  let caid = t.t_ca_id in
  (* tqty is the trade quantity *)
  let tqty = t.t_qty in
  (* tprice is the trade per unit price *)
  let tprice = t.t_trade_price in
  (* tchrg is the trade charge *)
  let tchrg = t.t_chrg in
   let buy_value = ref (Int64.of_int 0) in 
  let sell_value = ref (Int64.of_int 0) in 
  let needed_quantity = ref tqty in
  Select1.trade_type_table (ttid) >>= fun tt ->
  (* Selecting the history summary for the customer account *)
  Select1.holding_summary_table (caid, symbol) >>= fun hs ->
  (* Selecting the customer account with customer accoutn id ad caid *)
  Select1.customer_account_table (caid, bid, cid) >>= fun c_account ->
  let bid = c_account.ca_b_id in 
  let cid = c_account.ca_c_id in
  (* Selecting the customer that holds the account caid *) 
  Select1.customer_table cid >>= fun c ->
  let ctier = c.c_tier in
  Select1.charge_table tid >>= fun ch ->
  Select1.tax_rate_table(c.c_tax_id) >>= fun tax ->
  Select.commission_rate_table (fun (x,(y, z)) -> 
                             IdTriple.compare (x,(y, z)) (c.c_tier, (tid, c.c_tax_id))) >>= fun cr ->
  let rec filter_comm cr = match cr with 
   | x :: xl -> if x.cr_from_qty <= tqty && x.cr_to_qty >= tqty then x else filter_comm xl in 
  let ccharge = ch.ch_chrg in 
                  Select.holding_table (fun (htid, (hcaid, hsymb)) ->
                        IdTriple.compare (htid, (hcaid, hsymb)) (tid, (caid, symbol))) >>= fun hos -> 
  (* Modifying the customer's holding to reflect the result of buy or sell trade *)
  (* if it is sell type means customer is requesting for selling the holdings *)
  if (tt.tt_is_sell = Int64.of_int 1) (* sell type *)
  then 
       if (hs.hs_qty = Int64.of_int 0) (* No prior holding exists so we need to insert one *)
       then (let hs' = {hs_ca_id = caid; hs_s_symb = symbol; hs_qty = Int64.of_int 0-tqty} in 
            Insert.holding_summary_table hs')
       else if (hs.hs_qty != tqty) (* prior holding exists with the customer *)
            then 
                 let _ = Update.holding_summary_table
                      (fun (hscaid, hsssymb) -> IdPair.compare (hscaid, hsssymb) (caid, symbol))
                      (fun hsi -> {hsi with hs_qty = hs.hs_qty - tqty}) in Txn.return()
            else if (hs.hs_qty > Int64.of_int 0) 
                 then 
                    let _ = List.fold_left  
                            (fun pre x -> pre >>= fun () ->
                              if (x.h_qty) < tqty then 
                                let h' = {hh_h_t_id = x.h_t_id; 
                                          hh_t_id = tid; 
                                          hh_b_qty = x.h_qty; 
                                          hh_a_qty = x.h_qty - !needed_quantity} in 
                                Update.holding_table  (fun ckey -> IdTriple.compare ckey
                                                       (tid, (caid, symbol))) 
                                                       (fun h -> {h_t_id = x.h_t_id; 
                                                        h_ca_id = x.h_ca_id; 
                                                        h_s_symb = x.h_s_symb; 
                                                        h_price = x.h_price; 
                                                        h_qty = x.h_qty - !needed_quantity}) >>= fun () ->
                                Insert.holding_history_table h' ;
                                buy_value := Int64.add !buy_value (Int64.mul !needed_quantity (x.h_price));
                                sell_value := Int64.add !sell_value (Int64.mul !needed_quantity (trade_price));
                                needed_quantity := Int64.of_int 0;
                                Txn.return() 
                              else  (* Selling all security held *)
                                let h'= {hh_h_t_id = x.h_t_id; 
                                           hh_t_id = tid; 
                                           hh_b_qty = x.h_qty; 
                                           hh_a_qty = Int64.of_int 0} in 
                                 Insert.holding_history_table h' ;
                                 Delete.holding_table (x.h_t_id, x.h_ca_id, x.h_s_symb) >>= fun () ->
                                 buy_value := Int64.add !buy_value (Int64.mul (x.h_qty) (x.h_price));
                                 sell_value := Int64.add !sell_value (Int64.mul (x.h_qty) (trade_price));
                                 needed_quantity := (!needed_quantity - x.h_qty);
                                 Txn.return())
                                 (Txn.return()) hos in () 
                    ; if (!needed_quantity > Int64.of_int 0) (*sell short*)
                                    then 
                                         (Insert.holding_history_table {hh_h_t_id = tid; 
                                                                       hh_t_id = tid; 
                                                                       hh_b_qty = Int64.of_int 0; 
                                                                       hh_a_qty = Int64.of_int (- (Int64.to_int !needed_quantity))} ;
                                         Insert.holding_table {h_t_id = tid; 
                                                               h_ca_id = caid; 
                                                               h_s_symb = symbol; 
                                                               h_price = tprice; 
                                                               h_qty = Int64.of_int (- (Int64.to_int !needed_quantity))})
                                    else Txn.return()
                   else if (hs.hs_qty = tqty) 
                      then 
                           Delete.holding_summary_table (caid, symbol) 
                      else Txn.return()

    else (* buy type *)  
    
    if (hs.hs_qty = Int64.of_int 0) (* No prior holding exists so we need to insert one *)
       then  let hsi = {hs_ca_id = caid; hs_s_symb = symbol; hs_qty = tqty} in 
             Insert.holding_summary_table hsi 
       else if ((Int64.of_int (- (Int64.to_int hs.hs_qty))) != tqty) (* prior holding exists with the customer *)
            then 
                 Update.holding_summary_table
                      (fun (hscaid, hsssymb) -> IdPair.compare (hscaid, hsssymb) (caid, symbol))
                      (fun hsi -> {hsi with hs_qty = hs.hs_qty + tqty}) 
            else if (hs.hs_qty < Int64.of_int 0) (* short cover *)
                 then let y  = (List.fold_left
                                (fun pre x -> 
                                   pre >>= fun () ->
                                      if ((x.h_qty + !needed_quantity) < Int64.of_int 0) 
                                      then (*backing back some short sell *)
                                            let ht' = {hh_h_t_id = x.h_t_id; 
                                                       hh_t_id = tid; 
                                                       hh_b_qty = x.h_qty; 
                                                       hh_a_qty = x.h_qty + !needed_quantity} in 
                                            Update.holding_table (fun ckey -> IdTriple.compare ckey
                                                         (tid, (caid, symbol))) 
                                                         (fun h -> {h_t_id = x.h_t_id; 
                                                                    h_ca_id = x.h_ca_id; 
                                                                    h_s_symb = x.h_s_symb; 
                                                                    h_price = x.h_price; 
                                                                    h_qty = x.h_qty + !needed_quantity}) >>= fun () ->
                                            sell_value := Int64.add !sell_value (Int64.mul !needed_quantity (x.h_price));
                                            buy_value := Int64.add !buy_value (Int64.mul !needed_quantity (trade_price));
                                            needed_quantity := Int64.of_int 0;
                                            Insert.holding_history_table ht'
                                      else  (* buying back all short sell *)
                                            (let ht' = {hh_h_t_id = x.h_t_id; 
                                                        hh_t_id = tid; 
                                                        hh_b_qty = x.h_qty; 
                                                        hh_a_qty = Int64.of_int 0} in 
                                            Delete.holding_table (x.h_t_id, x.h_ca_id, x.h_s_symb) >>= fun () ->
                                            x.h_qty = Int64.of_int (- Int64.to_int x.h_qty) ;
                                            sell_value := Int64.add !sell_value (Int64.mul x.h_qty (x.h_price));
                                            buy_value := Int64.add !buy_value (Int64.mul x.h_qty (trade_price));
                                            needed_quantity := !needed_quantity - x.h_qty ;
                                            Insert.holding_history_table ht'))
                                            (Txn.return()) hos) 
                                            
                                      ; if (!needed_quantity > Int64.of_int 0) (* covered all short sells and buying new holdings *)
                                      then 
                                           (Insert.holding_history_table {hh_h_t_id = tid; 
                                                                         hh_t_id = tid; 
                                                                         hh_b_qty = Int64.of_int 0; 
                                                                         hh_a_qty = !needed_quantity} >>= fun () ->
                                           Insert.holding_table {h_t_id = tid; 
                                                                 h_ca_id = caid; 
                                                                 h_s_symb = symbol; 
                                                                 h_price = tprice; 
                                                                 h_qty = !needed_quantity})
                                    else Txn.return() in y
             else if ((Int64.of_int (- (Int64.to_int hs.hs_qty))) = tqty) 
                  then Delete.holding_summary_table (caid, symbol) >>= fun () ->
                  Txn.return();
                  else Txn.return();


(* Computing the commission of the broker who executed the trade *)
    if (sell_value > buy_value) then 
    ((let now_dts = Some (Unix.time()) in   
              let cr' = filter_comm cr in
              (* calculating the tax that can be incurred because of thsi trade *)
              let tax_amount = Int64.mul (Int64.sub !sell_value !buy_value) tax.tx_rate in 
              let comm_amount = ((Int64.div (cr'.cr_rate) (Int64.of_int 100)) * ((tqty) * (tprice))) in 
              Update.trade_table (fun (t, (tcaid, (_, _))) -> IdPair.compare (t,tcaid) (tid,caid))
                                                                (fun l -> {l with t_comm = comm_amount;
                                                                                  t_dts = now_dts});
              let th = {th_t_id = tid; th_dts = now_dts} in 
              Insert.trade_history_table th;
              Update.broker_table (fun brid -> Id.compare brid bid)
                                  (fun b -> {b with b_num_trades = b.b_num_trades + Int64.of_int 1;
                                                    b_comm = b.b_comm + comm_amount}) ;
    (if (tt.tt_is_sell = Int64.of_int 1)  
      then (let se_amount = ref (Int64.of_int 0) in 
            se_amount := Int64.add !se_amount 
                                  (Int64.sub (Int64.mul (tqty) 
                                                        (tprice)) 
                                                        (Int64.sub (ccharge) 
                                                                   (comm_amount)));
                                                                   Txn.return())
      else
          (let se_amount = ref (Int64.of_int 0) in 
           se_amount := !se_amount + (Int64.of_int (- Int64.to_int ((tqty * tprice) - ccharge - comm_amount)));
           se_amount := Int64.sub !se_amount tax_amount;
            (if (t.t_is_cash = true) 
              then (Update.customer_account_table (fun cakey -> IdTriple.compare cakey (caid, (bid, cid)))
                                          (fun ca -> {ca with ca_bal = ca.ca_bal + !se_amount}) >>= fun () ->
                   let s = {se_t_id= tid; se_cash_type = true; se_amt = !se_amount} in 
                   Insert.settlement_table s)
              else
                  let s = {se_t_id= tid; se_cash_type = false; se_amt = !se_amount} in 
                  Insert.settlement_table s)))))
  else Txn.return()
    














