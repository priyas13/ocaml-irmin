module User_id_type = 
 struct 
 type t = char
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = '#'
 let resolve x y = '#'
end 

(* Information about customers of the brokage firm *)
module Customer = 
 struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {c_id : atom; c_tax_id : atom; c_tier : int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.c_id = v1.c_id &&
                                 ancestor.c_id = v2.c_id &&
                                 v1.c_id = v2.c_id) &&
                                 (ancestor.c_tax_id = v1.c_tax_id &&
                                 ancestor.c_tax_id = v2.c_tax_id &&
                                 v1.c_tax_id = v2.c_tax_id) && 
                                 (ancestor.c_tier = v1.c_tier &&
                                 ancestor.c_tier = v2.c_tier &&
                                 v1.c_tier = v2.c_tier)
                               then {c_id = ancestor.c_id; 
                                     c_tax_id = ancestor.c_id; 
                                     c_tier=OC.merge ancestor.c_tier v1.c_tier v2.c_tier}
                               else failwith "Merge not possible" 
 let resolve x y = {c_id = x.c_id; c_tax_id = x.c_tax_id; c_tier = 0}
end 

(* Information related to accounts of each customer *)
module CustomerAccount = 
 struct 
 module OC = Counter.Make 
 type atom = User_id_type.t
 type t = {ca_id : atom; ca_b_id: atom; ca_c_id: atom; ca_bal : int; ca_tax_st : int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.ca_id = v1.ca_id &&
                                  ancestor.ca_id = v1.ca_id &&
                                  v1.ca_id = v2.ca_id) &&
                                  (ancestor.ca_b_id = v1.ca_b_id &&
                                  ancestor.ca_b_id = v1.ca_b_id &&
                                  v1.ca_b_id = v2.ca_b_id) && 
                                  (ancestor.ca_c_id = v1.ca_c_id &&
                                  ancestor.ca_c_id = v1.ca_c_id &&
                                  v1.ca_c_id = v2.ca_c_id)
                               then {ca_id = ancestor.ca_id;
                                     ca_b_id = ancestor.ca_b_id;
                                     ca_c_id = ancestor.ca_c_id;
                                     ca_bal = OC.merge ancestor.ca_bal v1.ca_bal v2.ca_bal;
                                     ca_tax_st = OC.merge ancestor.ca_tax_st v1.ca_tax_st v2.ca_tax_st}
                               else failwith "Merge not possible"
  let resolve x y = {ca_id = x.ca_id; ca_b_id = x.ca_b_id; ca_c_id = x.ca_c_id; ca_bal = 0; ca_tax_st = 0}
end

(* Information about the customer taxes based on state and national tax *)
(* Here cx_tx_id represents the tax identifier and cx_c_id represents the customer identifier that must pay this tax rate *)
module CustomerTaxRate = 
 struct 
 type atom = User_id_type.t
 type t = {cx_tx_id : atom; cx_c_id : atom}
end 

(* Information about the access the customer or an individual other than customer has to a given account *)
module AccountPermission = 
 struct 
 type atom = User_id_type.t 
 type t = {ap_ca_id : atom; ap_acl : string; ap_tax_id : atom}
end 

(* Information about Broker *)
(* Broker identifier, number of trades this broker has executed so far and the total commision this broker has earned so far *)
module Broker = 
 struct 
 module OC = Counter.Make
 type atom = User_id_type.t 
 type t = {b_id : atom; b_num_trades : int; b_comm_total : int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.b_id = v1.b_id &&
                                  ancestor.b_id = v2.b_id &&
                                  v1.b_id = v2.b_id)
                              then {b_id = ancestor.b_id;
                                    b_num_trades = OC.merge ancestor.b_num_trades v1.b_num_trades v2.b_num_trades;
                                    b_comm_total = OC.merge ancestor.b_comm_total v1.b_comm_total v2.b_comm_total}
                              else failwith "Merge not possible"
 let resolve x y = {b_id = x.b_id; b_num_trades = 0 ; b_comm_total = 0}
end 

(* Information about trades *)
(* Trade identifier, customer account identifier and commision earned in this trade *)
module Trade = 
 struct 
 module OC = Counter.Make 
 type atom = User_id_type.t 
 type t = {t_id : atom; t_ca_id: atom; t_comm: int; t_price : int; t_qty : int; t_type : string}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.t_id = v1.t_id &&
                                  ancestor.t_id = v2.t_id &&
                                  v1.t_id = v2.t_id) && 
                                  (ancestor.t_ca_id = v1.t_ca_id &&
                                  ancestor.t_ca_id = v2.t_ca_id &&
                                  v1.t_ca_id = v2.t_ca_id) &&
                                  (ancestor.t_type = v1.t_type &&
                                   v1.t_type = v2.t_type &&
                                   ancestor.t_type = v2.t_type)
                               then {t_id = ancestor.t_id;
                                     t_ca_id = ancestor.t_ca_id;
                                     t_comm = OC.merge ancestor.t_comm v1.t_comm v2.t_comm;
                                     t_price = OC.merge ancestor.t_price v1.t_price v2.t_price;
                                     t_qty = OC.merge ancestor.t_qty v1.t_qty v2.t_qty;
                                     t_type = ancestor.t_type}
                               else failwith "Merge not possible"
 let resolve x y = {t_id = x.t_id; t_ca_id = x.t_ca_id; t_comm = 0; t_price = 0; t_qty = 0; t_type = x.t_type}
end

(* Information about the type of trade *)
(* tt_id represents the trade type identifier and tt_is_sell_or_buy represents whether the trade is a sell trade (1) or a buy trade (0) *)
module TradeType = 
 struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {tt_id : string; tt_is_sell_or_buy : int}
end 

(* Information about customer account's security holdings *)
(* h_t_id represents trade identifier of the trade, h_ca_id represetns customer account identifier and h_qty represents quantity of this security held *)
module Holding = 
 struct 
 module OC = Counter.Make 
 type atom = User_id_type.t 
 type t = {h_t_id : atom; h_ca_id: atom; h_qty: int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.h_t_id = v1.h_t_id &&
                                  ancestor.h_t_id = v2.h_t_id &&
                                  v1.h_t_id = v2.h_t_id) && 
                                  (ancestor.h_ca_id = v1.h_ca_id &&
                                  ancestor.h_ca_id = v2.h_ca_id &&
                                  v1.h_ca_id = v2.h_ca_id)
                               then {h_t_id = ancestor.h_t_id;
                                     h_ca_id = ancestor.h_ca_id;
                                     h_qty = OC.merge ancestor.h_qty v1.h_qty v2.h_qty}
                               else failwith "Merge not possible"
 let resolve x y = {h_t_id = x.h_t_id; h_ca_id = x.h_ca_id; h_qty = 0}
end

(* Information about aggregrate information about the customer account's holdings *)
module HoldingSummary = 
 struct 
 module OC = Counter.Make 
 type atom = User_id_type.t 
 type t = {hs_ca_id : atom; hs_qty: int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.hs_ca_id = v1.hs_ca_id &&
                                  ancestor.hs_ca_id = v2.hs_ca_id &&
                                  v1.hs_ca_id = v2.hs_ca_id) 
                               then {hs_ca_id = ancestor.hs_ca_id;
                                     hs_qty = OC.merge ancestor.hs_qty v1.hs_qty v2.hs_qty}
                               else failwith "Merge not possible"
 let resolve x y = {hs_ca_id = x.hs_ca_id; hs_qty = 0}
end

module Tpce = 
 struct 
 open Customer
 open CustomerAccount 
 open Broker
 open Trade
 open Holding
 open HoldingSummary
 open TradeType 
 module OC = Mvector_list.List(Customer)
 module OCA = Mvector_list.List(CustomerAccount)
 module OB = Mvector_list.List(Broker)
 module OT = Mvector_list.List(Trade)
 module OH = Mvector_list.List(Holding)
 module OHS = Mvector_list.List(HoldingSummary)
 module OTY = Mvector_list.List(TradeType)

 type t = {tradet : OT.t; tradetypet : OTY.t}

 let rec retrieve_trade tid tt = match tt with 
                             | [] -> failwith "no such trade"
                             | x :: x' -> if x.t_id = tid then x 
                                                          else retrieve_trade tid x'

 let rec retrieve_trade_type tty ttyt = match ttyt with 
                                        | [] -> failwith "no such trade type"
                                        | x :: x' -> if x.tt_id = tty then x 
                                                                      else retrieve_trade_type tty x'

 (* Trade-Result Transactions *)
 (* It is designed to emulate the process of completing a stock market trade *)
 (* Customer's holdings are updated to reflect the trade has completed *)
 (* Price of trade is looked *)
 (* Parameters passed to this transaction:
    (1) Trade id which help us to get the information about the trade 
    (2) Account id of the customer account
    (3) Trade quantity 
    (4) Trade price 
    (5) Commision rate 
    (6) Broker id
    (7) Status of the trade *)
let trade_result_txn tid acctid tqty tprice commrate bid tt ttyt = 
	let comm_amount = commrate * (tqty * tprice) in 
    let t = retrieve_trade tid tt in 
    let tty = retrieve_trade (t.t_type) ttyt in 
    tty

end 




















