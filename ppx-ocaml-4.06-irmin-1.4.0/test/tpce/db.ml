open Printf
open Tpce
module U = Utils

module List = struct 
  include List

 	let init ~f n =
    let rec aux i = if i = n then [] else f i :: aux (i+1) in
		aux 0 
end

let _max_securities = 1370
let _max_companies = 1000
let _max_customers = 2000
let _max_accounts = 4000
let _max_tax_id = 20
let _max_brokers = 100
let _max_trade_type_id = 4
let _max_trade_price = 100

let _account_per_customer = 2
let _customers_per_tax_id = 100
let _account_per_broker = 40 
let _customer_per_broker = 20
let _security_per_trade_type = 342
let _max_trade_qty_per_customer = 10 
let _security_per_company = 1 
let _executor_per_company = 1
let _security_traded_per_account = 10 
let _security_traded_per_customer = 50


let seed = 80032723

let _ = Random.init seed

let (>>=) = Txn.bind

let load_customer c ct = 
  let open Customer in 
  let c_id = Id.of_int c in 
  let c_tax_id = Id.of_int ct in 
  let _ = if (c+1) mod 100 = 0 then 
       (printf "Customer (%d, %d) loaded\n" (c+1) (ct+1);
        flush_all()) in 
  Insert.customer_table {c_id; c_tax_id} 


let load_customer_account caid cabid cacid = 
   let open CustomerAccount in 
   let ca_id = Id.of_int caid in 
   let ca_b_id = Id.of_int cabid in 
   let ca_c_id = Id.of_int cacid in 
   let ca_bal = 1000L in 
   let _ = if (caid+1) mod 100 = 0 then 
       (printf "Customer Account (%d, %d, %d) loaded\n" (caid+1) (cabid+1) (cacid+1);
        flush_all()) in 
   Insert.customer_account_table {ca_id; ca_b_id; ca_c_id; ca_bal}



let load_account_permission apcaid aptaxid =
  let open AccountPermission in 
  let ap_ca_id = Id.of_int apcaid in 
  let ap_tax_id = Id.of_int aptaxid in 
  let _ = if (apcaid + 1) mod 100 = 0 then 
          (printf "Account permission (%d, %d) loaded\n" (apcaid+1) (aptaxid+1);
           flush_all()) in 
    Insert.account_permission_table {ap_ca_id; ap_tax_id}



let load_broker bid = 
  let open Broker in 
  let b_id = Id.of_int bid in 
  let b_num_trades = 0L in 
  let b_comm = 0L in  
    Insert.broker_table {b_id; b_num_trades; b_comm}



 let load_trade_type ttid = 
  let open TradeType in 
  let tt_id = Id.of_int ttid in 
  let tt_is_sell = Random.int64 (Int64.of_int 1) in
  let tt_is_market = Random.int64 (Int64.of_int 1) in 
  let _ = if (ttid+1) mod 100 = 0 then 
          (printf "Trade Type (%d) loaded\n" (ttid + 1);
           flush_all()) in 
    Insert.trade_type_table {tt_id; tt_is_sell; tt_is_market}



let load_security ssymb sexid scoid = 
  let open Security in 
  let s_symb = Id.of_int ssymb in 
  let s_ex_id = Id.of_int sexid in 
  let s_co_id = Id.of_int scoid in 
  let _ = if (ssymb+1) mod 100 = 0 then 
          (printf "Security (%d, %d, %d) loaded\n" (ssymb+1) (sexid+1) (scoid+1);
            flush_all())in 
        Insert.security_table {s_symb; s_ex_id; s_co_id}



let load_company coid = 
  let open Company in 
  let co_id = Id.of_int coid in 
  let _ = if (coid+1) mod 100 = 0 then 
          (printf "Company %d loaded\n" (coid+1);
           flush_all()) in 
        Insert.company_table {co_id}



let load_exchange exid = 
  let open Exchange in 
  let ex_id = Id.of_int exid in 
  let _ = if (exid+1) mod 100 = 0 then 
          (printf "Exchange %d loaded\n" (exid+1);
           flush_all()) in 
        Insert.exchange_table {ex_id}


let load_tax_rate txid = 
  let open TaxRate in 
  let tax_id = Id.of_int txid in 
  let tx_rate = Random.int64 3L in 
  let _ = if (txid+1) mod 100 = 0 then 
          (printf "Tax Rate %d loaded\n" (txid+1);
           flush_all()) in 
          Insert.tax_rate_table {tax_id; tx_rate}



let load_settlement setid =
  let open Settlement in 
  let se_t_id = Id.of_int setid in 
  let se_cash_type = true in 
  let se_amt = 0L in 
  let _ = if (setid+1) mod 100 = 0 then 
          (printf "Settlement %d loaded\n" (setid+1);
           flush_all ()) in 
          Insert.settlement_table {se_t_id; se_cash_type; se_amt}




let load_commission_rate crttid crexid = 
  let open CommissionRate in 
  let cr_tt_id = Id.of_int crttid in 
  let cr_ex_id = Id.of_int crexid in 
  let cr_from_qty = Random.int64 (Int64.of_int 10) in 
  let cr_to_qty = U.bounded_random_int64 cr_from_qty (Int64.of_int 10) in 
  let cr_rate = Random.int64 3L in 
  let _ = if (crttid+1) mod 100 = 0 then 
          (printf "Commission rate (%d, %d) loaded\n" (crttid+1) (crexid+1);
           flush_all()) in 
          Insert.commission_rate_table {cr_tt_id; cr_ex_id; cr_from_qty; cr_to_qty; cr_rate}



let load_charge chttid = 
  let open Charge in 
  let ch_tt_id = Id.of_int chttid in 
  let ch_chrg = Random.int64 3L in 
  let _ = if (chttid+1) mod 100 = 0 then 
          (printf "Charge %d loaded\n" (chttid+1);
           flush_all()) in 
          Insert.charge_table {ch_tt_id; ch_chrg}



let load_trade tid tcaid ttid tsymb = 
  let open Trade in 
  let open TradeRequest in 
  let open TradeHistory in 
  let open LastTrade in 
  let t_id = Id.of_int tid in 
  let t_ca_id = Id.of_int tcaid in 
  let tt_id = Id.of_int ttid in 
  let t_is_cash = true in 
  let t_qty = Random.int64 (Int64.of_int 10) in 
  let t_symb = Id.of_int tsymb in
  let t_exec_name = sprintf "item %d" tid in
  let t_trade_price = Random.int64 100L in 
  let t_dts = Some (Unix.time ()) in 
  let t_chrg = Random.int64 10L in 
  let t_comm = 10L in 
  let _ = if (tid+1) mod 100 = 0 then 
          (printf "Trade (%d,%d, %d, %d) loaded\n" (tid+1) (tcaid+1) (ttid+1) (tsymb+1);
            flush_all()) in 
      Insert.trade_table {t_id; t_ca_id; tt_id; t_is_cash; t_qty; t_symb; t_exec_name;
                          t_trade_price; t_dts; t_chrg; t_comm}



let load_trade_request trtid trttid trbid trssymb = 
  let open TradeRequest in 
  let tr_t_id = Int64.of_int trtid in 
  let tr_tt_id = Int64.of_int trttid in 
  let tr_b_id = Int64.of_int trbid in 
  let tr_s_symb = Int64.of_int trssymb in 
  let tr_qty = Random.int64 10L in 
  let tr_bid_price = Random.int64 100L in 
  let _ = if (trtid+1) mod 100 = 0 then 
          (printf "Trade request (%d, %d, %d, %d) loaded\\n" (trtid+1) (trttid+1) (trbid+1) (trssymb+1);
           flush_all()) in
      Insert.trade_request_table {tr_t_id; tr_tt_id; tr_b_id; tr_s_symb; tr_qty; tr_bid_price}



let load_trade_history thtid = 
  let open TradeHistory in 
  let th_t_id = Int64.of_int thtid in 
  let th_dts = Some (Unix.time ()) in 
  let _ = if (thtid+1) mod 100 = 0 then 
           (printf "Trade history %d loaded\n" (thtid+1);
            flush_all()) in 
        Insert.trade_history_table {th_t_id; th_dts}




let load_last_trade ltsymb = 
  let open LastTrade in 
  let lt_s_symb = Int64.of_int ltsymb in 
  let lt_price = Random.int64 100L in 
  let lt_dts = Some (Unix.time ()) in 
  let lt_vol = Random.int64 10L in 
  let _ = if (ltsymb+1) mod 100 = 0 then
          (printf "Last trade %d loaded\n" (ltsymb+1);
           flush_all()) in 
        Insert.last_trade_table {lt_s_symb; lt_price; lt_dts; lt_vol}



let load_holding htid htcaid htsymb = 
  let open Holding in  
  let h_t_id = Id.of_int htid in 
  let h_ca_id = Id.of_int htcaid in 
  let h_s_symb = Id.of_int htsymb in 
  let h_price = Random.int64 100L in 
  let h_qty = Random.int64 (Int64.of_int 10) in 
  let _ = if (htid+1) mod 100 = 0 then 
          (printf "Trade (%d,%d, %d) loaded\n" (htid+1) (htcaid+1) (htsymb+1);
            flush_all()) in 
      Insert.holding_table {h_t_id; h_ca_id; h_s_symb; h_price; h_qty}




let load_holding_history hhhtid hhtid = 
  let open Holding in  
  let hh_h_t_id = Id.of_int hhhtid in 
  let hh_t_id = Id.of_int hhtid in 
  let hh_b_qty = Random.int64 (Int64.of_int 10) in 
  let hh_a_qty = Random.int64 (Int64.of_int 10) in 
  let _ = if (hhhtid+1) mod 100 = 0 then 
          (printf "Trade (%d,%d) loaded\n" (hhhtid+1) (hhtid+1);
            flush_all()) in 
      Insert.holding_history_table {hh_h_t_id; hh_t_id; hh_b_qty; hh_a_qty}




let load_holding_summary hscaid hsssymb = 
  let open HoldingSummary in  
  let hs_ca_id = Id.of_int hscaid in 
  let hs_s_symb = Id.of_int hsssymb in 
  let hs_qty = Random.int64 (Int64.of_int 10) in 
  let _ = if (hscaid+1) mod 100 = 0 then 
          (printf "Trade (%d, %d) loaded\n" (hscaid+1) (hsssymb+1);
            flush_all()) in 
      Insert.holding_summary_table {hs_ca_id; hs_s_symb; hs_qty}

let load_customer () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_customer i j)
     20  pre1)
  100 (Txn.return ())

   let load_customer_account () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
      U.fold (fun k pre3 ->
        pre3 >>= fun () -> load_customer_account i j k)
      100 pre2)
    20 pre1)
    2 (Txn.return ())

let load_account_permission () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_account_permission i j)
      20 pre1)
    100 (Txn.return ())

let load_broker () =
  U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_broker i)
        _max_brokers (Txn.return ())

let load_trade_type () = 
   U.fold (fun i pre1 ->
           pre1 >>= fun () -> load_trade_type i)
           _max_trade_type_id (Txn.return ())

let load_security () = 
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
          pre3 >>= fun () -> load_security i j k)
        _security_per_company pre2)
      _executor_per_company pre1)
    _max_companies (Txn.return ())

let load_holding_summary () = 
    U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
          pre2 >>= fun () -> load_holding_summary i j)
        _max_securities pre1)
    2 (Txn.return ())


let load_company () = 
  U.fold (fun i pre1 ->
        pre1 >>= fun () -> load_company i)
      _max_companies (Txn.return ())


let load_exchange () = 
  U.fold (fun i pre1 ->
        pre1 >>= fun () -> load_exchange i)
      _max_companies (Txn.return ())



let load_trade () = 
    U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
        U.fold (fun l pre4 ->
          pre4 >>= fun () -> load_trade i j k l)
        _max_securities pre3)
      _max_trade_type_id pre2)
    2 pre1)
    10 (Txn.return ())

let load_tax_rate () = 
  U.fold (fun i pre1 ->
        pre1 >>= fun () -> load_tax_rate i)
      _max_tax_id (Txn.return ())

let load_settlement () =
  U.fold (fun i pre1 ->
          pre1 >>= fun () -> load_settlement i)
         10 (Txn.return())

let load_commission_rate () = 
   U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_commission_rate i j)
      _max_securities pre1)
    _max_trade_type_id (Txn.return ())

let load_charge () = 
   U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_charge i)
      _max_trade_type_id (Txn.return ())

let load_trade_request () =
   U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
        U.fold (fun l pre4 ->
          pre4 >>= fun () -> load_trade_request i j k l)
        20 pre3)
      20 pre2)
    5 pre1)
    10 (Txn.return ())

let load_trade_history () = 
   U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_trade_history i)
      10 (Txn.return ())

let load_last_trade () = 
   U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_last_trade i)
      _max_securities (Txn.return ())

let load_holding () = 
    U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
        U.fold (fun k pre3 ->
          pre3 >>= fun () -> load_holding i j k)
        _max_securities pre2)
    2 pre1)
    10 (Txn.return ())

let load_holding_history () = 
    U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
          pre2 >>= fun () -> load_holding_history i j)
        10 pre1)
    10 (Txn.return ())


let populate () =
  begin 
   load_customer() >>= fun _ ->
   load_account_permission() >>= fun _ ->
   load_customer_account() >>= fun _ ->
   load_broker() >>= fun _ ->
   load_company() >>= fun _ ->
   load_exchange() >>= fun _ ->
   load_tax_rate() >>= fun _ ->
   load_security() >>= fun _ ->
   load_trade_type() >>= fun _ ->
   load_settlement() >>= fun _ ->
   load_commission_rate() >>= fun _ ->
   load_charge() >>= fun _ ->
   load_trade () >>= fun _ ->
   load_trade_request() >>= fun _ ->
   load_trade_history () >>= fun _ ->
   load_last_trade () >>= fun _ ->
   load_holding () >>= fun _ ->
   load_holding_history () >>= fun _ ->
   load_holding_summary ()
 end 

let empty = {customer_table = CustomerTable.empty; 
             customer_account_table = CustomerAccountTable.empty;
             holding_table = HoldingTable.empty;
             holding_history_table = HoldingHistoryTable.empty;
             charge_table = ChargeTable.empty;
             commission_rate_table = CommissionRateTable.empty;
             holding_summary_table = HoldingSummaryTable.empty;
             broker_table = BrokerTable.empty;
             trade_table = TradeTable.empty;
             last_trade_table = LastTradeTable.empty;
             trade_history_table = TradeHistoryTable.empty;
             trade_request_table = TradeRequestTable.empty;
             trade_type_table = TradeTypeTable.empty;
             tax_rate_table = TaxRateTable.empty;
             settlement_table = SettlementTable.empty;
             exchange_table = ExchangeTable.empty;
             company_table = CompanyTable.empty;
             security_table = SecurityTable.empty;
             account_permission_table = AccountPermissionTable.empty}

let do_market_feed_txn db = 
  let lt_s_symb = Id.random _max_securities in 
  let t_price = Id.random _max_trade_price in 
  let tr_qty = Id.random _max_trade_qty_per_customer in 
  let type_limit_sell = Id.random _max_trade_type_id in 
  let type_limit_buy = Id.random _max_trade_type_id in 
  snd @@ market_feed_txn lt_s_symb t_price tr_qty type_limit_sell type_limit_buy db


let do_trade_cleanup_txn db =
  let trid = Id.random _max_securities in 
  snd @@ trade_cleanup_txn trid db 

let do_trade_order_txn db =
  let acctid = Id.random 1 in 
  let bid = Id.random 20 in 
  let cid = Id.random _customers_per_tax_id in 
  let coid = Id.random _max_companies in 
  let ctaxid = Random.int64 (Int64.of_int _max_tax_id) in 
  let exec_tax_id = Id.random _max_tax_id in 
  let symbol = Id.random _max_securities in 
  let tid = Id.random 10 in 
  let trade_type_id = Id.random _max_trade_type_id in 
  let requested_price = Id.random 100 in 
  let trade_qty = Id.random 10 in 
  let exec_name = sprintf "item %d" (Int64.to_int tid) in 
  snd @@ trade_order_txn acctid bid cid ctaxid coid exec_tax_id symbol tid trade_type_id requested_price trade_qty exec_name db

let do_trade_result_txn db =
  let tid = Id.random 10 in 
  let caid = Id.random 1 in 
  let ttid = Id.random _max_trade_type_id in 
  let cid = Id.random _customers_per_tax_id in 
  let ctaxid = Random.int64 (Int64.of_int _max_tax_id) in 
  let bid = Id.random 20 in 
  let symbol = Id.random _max_securities in 
  let trade_price = Id.random 100 in 
  snd @@ trade_result_txn tid caid ttid cid ctaxid bid symbol trade_price db 

let do_trade_update_txn db = 
 let job_to_be_done = U.bounded_random_int 1 3 in 
 let tid = Id.random 10 in 
 let tcaid =  Id.random 1 in
 let ttid = Id.random _max_trade_type_id in 
 let tsymb = Id.random _max_securities in 
 let exec_name = sprintf "item %d" (Int64.to_int tid) in 
 let execid = Id.random _max_securities in 
 let coid = Id.random _max_companies in 
 let start_t = Some ((Unix.time ())) in 
 let end_t = Some  (Unix.time ()) in 
 snd @@ trade_update_txn job_to_be_done tid tcaid ttid tsymb exec_name execid coid start_t end_t db 

let dump_keys db = 
  let fp = open_out "keys.db" in
  let dump1 k v = 
    fprintf fp "%s\n" @@ Id.to_string k in
  let dump2 k v = 
    fprintf fp "%s\n" @@ IdPair.to_string k in
  let dump3 k v = 
    fprintf fp "%s\n" @@ IdTriple.to_string k in
  let dump4 k v = 
    fprintf fp "%s\n" @@ IdQuad.to_string k in
  let dump5 k v = 
    fprintf fp "%s\n" @@ IdQuin.to_string k in
  begin 
    fprintf fp "\n> Customer\n";
    CustomerTable.iter dump2 db.customer_table;
    fprintf fp "\n> CustomerAccount\n";
    CustomerAccountTable.iter dump3 db.customer_account_table;
    fprintf fp "\n> CustomerPermission\n";
    AccountPermissionTable.iter dump2 db.account_permission_table;
    fprintf fp "\n> Holding\n";
    HoldingTable.iter dump3 db.holding_table;
    fprintf fp "\n> HoldingHistory\n";
    HoldingHistoryTable.iter dump2 db.holding_history_table;
    fprintf fp "\n> Charge\n";
    ChargeTable.iter dump1 db.charge_table;
    fprintf fp "\n> CommissionRate\n";
    CommissionRateTable.iter dump2 db.commission_rate_table;
    fprintf fp "\n> HoldingSummary\n";
    HoldingSummaryTable.iter dump2 db.holding_summary_table;
    fprintf fp "\n> Broker\n";
    BrokerTable.iter dump1 db.broker_table;
    fprintf fp "\n> Trade\n";
    TradeTable.iter dump4 db.trade_table;
    fprintf fp "\n> LastTrade\n";
    LastTradeTable.iter dump1 db.last_trade_table;
    fprintf fp "\n> TradeHistory\n";
    TradeHistoryTable.iter dump1 db.trade_history_table;
    fprintf fp "\n> TradeRequest\n";
    TradeRequestTable.iter dump4 db.trade_request_table;
    fprintf fp "\n> TaxRate\n";
    TaxRateTable.iter dump1 db.tax_rate_table;
    fprintf fp "\n> Exchange\n";
    ExchangeTable.iter dump1 db.exchange_table;
    fprintf fp "\n> Company\n";
    CompanyTable.iter dump1 db.company_table;
    fprintf fp "\n> Security\n";
    SecurityTable.iter dump3 db.security_table;
    fprintf fp "\n> Tradetype\n";
    TradeTypeTable.iter dump1 db.trade_type_table;
    fprintf fp "\n> Settlement\n";
    SettlementTable.iter dump1 db.settlement_table;
    close_out fp;
    printf "Dumped keys in keys.db\n";
  end 


















  
  
