module User_id_type = 
 struct 
 type t = char
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = '#'
 let resolve x y = '#'
end 

(* Warehouse consists of fields representing warehouse id, warehouse name, warehouse state and year to date balance for that particular warehouse *)
(* In case of merging warehouses by using three-way merge, we need to check if the warehouse is same or not. If it is same then we use the merge function of counter *)
module Warehouse = struct 
  module OC = Counter.Make
  type atom = User_id_type.t
  type t = {w_id : atom; w_name : string; w_state : string; w_ytd : int}
  let compare = Pervasives.compare
  let merge3 ~ancestor v1 v2 = if (ancestor.w_id = v1.w_id) && 
                                  (v1.w_id = v2.w_id) && 
                                  (ancestor.w_id = v2.w_id) &&
                                  (ancestor.w_name = v1.w_name) && 
                                  (v1.w_name = v2.w_name) && 
                                  (ancestor.w_name = v2.w_name) &&
                                  (ancestor.w_state = v1.w_state) && 
                                  (v1.w_state = v2.w_state) && 
                                  (ancestor.w_state = v2.w_state)
                                  then 
                                     {w_id = ancestor.w_id; 
                                      w_name = ancestor.w_name; 
                                      w_state = ancestor.w_state; 
                                      w_ytd = OC.merge ancestor.w_ytd v1.w_ytd v2.w_ytd} 
                                  else failwith "Merge not possible"
  let resolve x y = {w_id = x.w_id; 
                     w_name = x.w_name; 
                     w_state = x.w_state; 
                     w_ytd = 0} 
end 

(* District consists of fields representing the district id, district next order number *)
module District = struct 
  module OC = Counter.Make
  type atom = User_id_type.t 
  type t = {d_id : atom; d_name: string; d_w_id: atom; d_ytd : int; d_next_o_id : int}
  let compare = Pervasives.compare
  let merge3 ~ancestor v1 v2 = if (ancestor.d_id = v1.d_id) && 
                                  (v1.d_id = v2.d_id) && 
                                  (ancestor.d_id = v2.d_id) &&
                                  (ancestor.d_name = v1.d_name) && 
                                  (v1.d_name = v2.d_name) && 
                                  (ancestor.d_name = v2.d_name) &&
                                  (ancestor.d_w_id = v1.d_w_id) && 
                                  (v1.d_w_id = v2.d_w_id) && 
                                  (ancestor.d_w_id = v2.d_w_id)
                                  then 
                                     {d_id = ancestor.d_id; 
                                      d_name = ancestor.d_name; 
                                      d_w_id = ancestor.d_w_id;
                                      d_ytd =  OC.merge (ancestor.d_ytd) (v1.d_ytd) (v2.d_ytd);
                                      d_next_o_id = OC.merge (ancestor.d_next_o_id) (v1.d_next_o_id) (v2.d_next_o_id)} 
                                  else failwith "Merge not possible"
  let resolve x y = {d_id = x.d_id; 
                     d_name = x.d_name; 
                     d_w_id = x.d_w_id;
                     d_ytd =  0;
                     d_next_o_id = 0}
end 

(* Customer consist of fileds representing c_id representing customer id,
   c_d_id representing the district associated with the customer,
   c_w_id representing the warehouse associated with the customer,
   c_ytd representing the year to date balance,
   c_payment_ct representing *)
module Customer = struct 
 module OC = Counter.Make
 type atom = User_id_type.t 
 type t = {c_id : atom; c_d_id : atom; c_w_id : atom; c_ytd : int; c_payment_ct : int; c_bal : int; c_delivery_ct : int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.c_id = v1.c_id) && 
                                  (v1.c_id = v2.c_id) && 
                                  (ancestor.c_id = v2.c_id) &&
                                  (ancestor.c_d_id = v1.c_d_id) && 
                                  (v1.c_d_id = v2.c_d_id) && 
                                  (ancestor.c_d_id = v2.c_d_id) &&
                                  (ancestor.c_w_id = v1.c_w_id) && 
                                  (v1.c_w_id = v2.c_w_id) && 
                                  (ancestor.c_w_id = v2.c_w_id)
                                  then 
                                     {c_id = ancestor.c_id; 
                                      c_d_id = ancestor.c_d_id; 
                                      c_w_id = ancestor.c_w_id;
                                      c_ytd =  OC.merge (ancestor.c_ytd) (v1.c_ytd) (v2.c_ytd);
                                      c_payment_ct = OC.merge (ancestor.c_payment_ct) (v1.c_payment_ct) (v2.c_payment_ct);
                                      c_bal = OC.merge (ancestor.c_bal) (v1.c_bal) (v2.c_bal);
                                      c_delivery_ct = OC.merge (ancestor.c_delivery_ct) (v1.c_delivery_ct) (v2.c_delivery_ct)}
                                  else failwith "Merge not possible"
  let resolve x y =  {c_id = x.c_id; 
                      c_d_id = x.c_d_id; 
                      c_w_id = x.c_w_id;
                      c_ytd =  0;
                      c_payment_ct = 0;
                      c_bal = 0;
                      c_delivery_ct = 0}
end 

(* Order consists of fields representing the order id, o_c_id which represents the customer id corresponding to that order, 
   o_d_id which represents the district id associated with that order, 
   o_w_id which represents the warehouse id associated with that order,
   o_ol_ct which represents the order count *)
module Order = struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {o_id : int; o_c_id : atom; o_d_id : atom; o_w_id : atom; o_ol_ct: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.o_id = v1.o_id) && 
                                  (v1.o_id = v2.o_id) && 
                                  (ancestor.o_id = v2.o_id) &&
                                  (ancestor.o_c_id = v1.o_c_id) && 
                                  (v1.o_c_id = v2.o_c_id) && 
                                  (ancestor.o_c_id = v2.o_c_id) &&
                                  (ancestor.o_d_id = v1.o_d_id) && 
                                  (v1.o_d_id = v2.o_d_id) && 
                                  (ancestor.o_d_id = v2.o_d_id) &&
                                  (ancestor.o_w_id = v1.o_w_id) && 
                                  (v1.o_w_id = v2.o_w_id) && 
                                  (ancestor.o_w_id = v2.o_w_id)
                                  then 
                                     {o_id = OC.merge ancestor.o_id v1.o_id v2.o_id; 
                                      o_c_id = ancestor.o_c_id; 
                                      o_d_id = ancestor.o_d_id;
                                      o_w_id = ancestor.o_w_id;
                                      o_ol_ct =  OC.merge (ancestor.o_ol_ct) (v1.o_ol_ct) (v2.o_ol_ct)}
                                  else failwith "Merge not possible"
  let resolve x y =  {o_id = x.o_id; 
                      o_c_id = x.o_c_id; 
                      o_d_id = x.o_d_id;
                      o_w_id = x.o_w_id;
                      o_ol_ct = 0}
end

module Neworder = struct
 module OC = Counter.Make 
 type atom = User_id_type.t 
 type t = {no_o_id: int; no_d_id: atom; no_w_id: atom}
 let merge3 ~ancestor v1 v2 = if (ancestor.no_o_id = v1.no_o_id) && 
                                  (v1.no_o_id = v2.no_o_id) && 
                                  (ancestor.no_o_id = v2.no_o_id) &&
                                  (ancestor.no_d_id = v1.no_d_id) && 
                                  (v1.no_d_id = v2.no_d_id) && 
                                  (ancestor.no_d_id = v2.no_d_id) &&
                                  (ancestor.no_w_id = v1.no_w_id) && 
                                  (v1.no_w_id = v2.no_w_id) && 
                                  (ancestor.no_w_id = v2.no_w_id)
                                  then 
                                     {no_o_id = OC.merge (ancestor.no_o_id) (v1.no_o_id) (v2.no_o_id); 
                                      no_d_id = ancestor.no_d_id; 
                                      no_w_id = ancestor.no_w_id}
                                  else failwith "Merge not possible"
  let resolve x y = {no_o_id = 0; 
                     no_d_id = x.no_d_id; 
                     no_w_id = x.no_w_id}
end 

module Stock = struct
 module OC = Counter.Make 
 type atom = User_id_type.t
 type t = {s_i_id: atom; s_w_id: atom; s_qty: int; s_ytd: int; s_order_cnt: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.s_i_id = v1.s_i_id) && 
                                  (v1.s_i_id = v2.s_i_id) && 
                                  (ancestor.s_i_id = v2.s_i_id) &&
                                  (ancestor.s_w_id = v1.s_w_id) && 
                                  (v1.s_w_id = v2.s_w_id) && 
                                  (ancestor.s_w_id = v2.s_w_id)
                                 then 
                                     {s_i_id = ancestor.s_i_id;
                                      s_w_id = ancestor.s_w_id;
                                      s_qty = OC.merge (ancestor.s_qty) (v1.s_qty) (v2.s_qty); 
                                      s_ytd = OC.merge (ancestor.s_ytd) (v1.s_ytd) (v2.s_ytd);
                                      s_order_cnt = OC.merge (ancestor.s_order_cnt) (v1.s_order_cnt) (v2.s_order_cnt)}
                                  else failwith "Merge not possible"
  let resolve x y = {s_i_id = x.s_i_id;
                     s_w_id = x.s_w_id;
                     s_qty = 0; 
                     s_ytd = 0;
                     s_order_cnt = 0}
end 

module Item = struct
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {i_id: atom; i_name: string; i_price: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.i_id = v1.i_id) && 
                                  (v1.i_id = v2.i_id) && 
                                  (ancestor.i_id = v2.i_id) &&
                                  (ancestor.i_name = v1.i_name) && 
                                  (v1.i_name = v2.i_name) && 
                                  (ancestor.i_name = v2.i_name)
                                  then 
                                     {i_id = ancestor.i_id;
                                      i_name = ancestor.i_name;
                                      i_price = OC.merge (ancestor.i_price) (v1.i_price) (v2.i_price)}
                                  else failwith "Merge not possible"
 let resolve x y = {i_id = x.i_id;
                    i_name = x.i_name;
                    i_price = 0}
end 

module History = struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {h_c_id: atom; h_c_d_id: atom; h_c_w_id: atom; h_d_id: atom; h_w_id: atom; h_amt: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.h_c_id = v1.h_c_id) && 
                                  (v1.h_c_id = v2.h_c_id) && 
                                  (ancestor.h_c_id = v2.h_c_id) &&
                                  (ancestor.h_c_d_id = v1.h_c_d_id) && 
                                  (v1.h_c_d_id = v2.h_c_d_id) && 
                                  (ancestor.h_c_d_id = v2.h_c_d_id) &&
                                  (ancestor.h_c_w_id = v1.h_c_w_id) && 
                                  (v1.h_c_w_id = v2.h_c_w_id) && 
                                  (ancestor.h_c_w_id = v2.h_c_w_id) &&
                                  (ancestor.h_d_id = v1.h_d_id) && 
                                  (v1.h_d_id = v2.h_d_id) && 
                                  (ancestor.h_d_id = v2.h_d_id) && 
                                  (ancestor.h_w_id = v1.h_w_id) && 
                                  (v1.h_w_id = v2.h_w_id) && 
                                  (ancestor.h_w_id = v2.h_w_id)
                                  then 
                                     {h_c_id = ancestor.h_c_id; 
                                      h_c_d_id = ancestor.h_c_d_id;
                                      h_c_w_id = ancestor.h_c_w_id; 
                                      h_d_id = ancestor.h_d_id;
                                      h_w_id = ancestor.h_w_id; 
                                      h_amt = OC.merge ancestor.h_amt v1.h_amt v2.h_amt}
                                  else failwith "Merge not possible"
  let resolve x y =  {h_c_id = x.h_c_id; 
                      h_c_d_id = x.h_c_d_id;
                      h_c_w_id = x.h_c_w_id; 
                      h_d_id = x.h_d_id;
                      h_w_id = x.h_w_id; 
                      h_amt = 0}
end 

module Orderline = struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {ol_o_id: int; 
           ol_d_id: atom; 
           ol_w_id: atom; 
           ol_num: int; 
           ol_amt: int; 
           ol_i_id: atom; 
           ol_supply_w_id: atom;
           ol_qty: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.ol_o_id = v1.ol_o_id) && 
                                  (v1.ol_o_id = v2.ol_o_id) && 
                                  (ancestor.ol_o_id = v2.ol_o_id) &&
                                  (ancestor.ol_d_id = v1.ol_d_id) && 
                                  (v1.ol_d_id = v2.ol_d_id) && 
                                  (ancestor.ol_d_id = v2.ol_d_id) &&
                                  (ancestor.ol_w_id = v1.ol_w_id) && 
                                  (v1.ol_w_id = v2.ol_w_id) && 
                                  (ancestor.ol_w_id = v2.ol_w_id)
                                  then 
                                     {ol_o_id = OC.merge ancestor.ol_o_id v1.ol_o_id v2.ol_o_id; 
                                      ol_d_id = ancestor.ol_d_id; 
                                      ol_w_id = ancestor.ol_w_id; 
                                      ol_num = OC.merge ancestor.ol_num v1.ol_num v2.ol_num; 
                                      ol_amt = OC.merge ancestor.ol_amt v1.ol_amt v2.ol_amt; 
                                      ol_i_id = ancestor.ol_i_id; 
                                      ol_supply_w_id = ancestor.ol_supply_w_id;
                                      ol_qty = OC.merge ancestor.ol_qty v1.ol_qty v2.ol_qty}
                                  else failwith "Merge not possible"
  let resolve x y = {ol_o_id = 0; 
                     ol_d_id = x.ol_d_id; 
                     ol_w_id = x.ol_w_id; 
                     ol_num = x.ol_num; 
                     ol_amt = 0; 
                     ol_i_id = x.ol_i_id; 
                     ol_supply_w_id = x.ol_supply_w_id;
                     ol_qty = 0}
end

module Itemreq = struct 
module OC = Counter.Make
type atom = User_id_type.t
type t = {ol_num: int ; 
          ol_i_id: atom; 
          ol_supply_w_id: atom; 
          ol_qty: int}
let merge3 ~ancestor v1 v2 = if (ancestor.ol_i_id = v1.ol_i_id) &&
                                (ancestor.ol_i_id = v2.ol_i_id) &&
                                (v1.ol_i_id = v2.ol_i_id) &&
                                (ancestor.ol_supply_w_id = v1.ol_supply_w_id) &&
                                (ancestor.ol_supply_w_id = v2.ol_supply_w_id) &&
                                (v1.ol_supply_w_id = v2.ol_supply_w_id) 
                                then 
                                   {ol_num = OC.merge ancestor.ol_num v1.ol_num v2.ol_num;
                                    ol_i_id = ancestor.ol_i_id;
                                    ol_supply_w_id = ancestor.ol_supply_w_id;
                                    ol_qty = OC.merge ancestor.ol_qty v1.ol_qty v2.ol_qty}
                                else failwith "Merge not possible"
let resolve x y = {ol_num = 0;
                   ol_i_id = x.ol_i_id;
                   ol_supply_w_id = x.ol_supply_w_id;
                   ol_qty = 0}
end 

module Tpcc = struct 
open Warehouse
open District
open Customer
open History
open Order
open Orderline
open Neworder
open Stock
open Item 
open Orderline
open Itemreq
module OCT = Counter.Make
module OW = Mvector_list.List(Warehouse)
module OD = Mvector_list.List(District)
module OC = Mvector_list.List(Customer)
module OO = Mvector_list.List(Order)
module OL = Mvector_list.List(Orderline)
module ONO = Mvector_list.List(Neworder)
module OS = Mvector_list.List(Stock)
module OI = Mvector_list.List(Item)
module OIR = Mvector_list.List(Itemreq)
module OH = Mvector_list.List(History)

type t = {wt : OW.t; dt : OD.t; ct : OC.t; ot : OO.t; olt : OL.t; ont : ONO.t; st : OS.t; it : OI.t; irt : OIR.t; ht : OH.t}

let rec retrieve_warehouse wid wt = match wt with 
                               | [] -> failwith "No such warehouse"
                               | x :: x' -> if x.w_id = wid then x else retrieve_warehouse wid x'

let rec retrieve_district did dw dt = match dt with 
                               | [] -> failwith "No such district"
                               | x :: x' -> if x.d_id = did && x.d_w_id = dw then x else retrieve_district did dw x'

let rec retrieve_customer cid cd cw ct = match ct with 
                                         | [] ->failwith "No such customer"
                                         | x :: x' -> if x.c_id = cid && 
                                                         x.c_d_id = cd && 
                                                         x.c_w_id = cw then x 
                                                                       else retrieve_customer cid cd cw x'

let rec retrieve_order oid ow od oc ot = match ot with 
                                         | [] ->failwith "No such order"
                                         | x :: x' -> if x.o_id = oid && 
                                                         x.o_c_id = oc && 
                                                         x.o_d_id = od &&
                                                         x.o_w_id = ow then x 
                                                                       else retrieve_order oid ow od oc x'

let rec retrieve_stock siid sw st = match st with 
                                         | [] ->failwith "No such stock"
                                         | x :: x' -> if x.s_i_id = siid && 
                                                         x.s_w_id = sw then x 
                                                                          else retrieve_stock siid sw x'

let rec retrieve_item iid it = match it with 
                                         | [] ->failwith "No such stock"
                                         | x :: x' -> if x.i_id = iid then x 
                                                                      else retrieve_item iid x'

let update_warehouse wid wt amt = let wh = retrieve_warehouse wid wt in 
                                   {w_id = wid; 
                                    w_name = wh.w_name; 
                                    w_state = wh.w_state; 
                                    w_ytd = OCT.inc wh.w_ytd amt}

let update_district_ytd did dw dt amt = let d = retrieve_district did dw dt in 
                                         {d_id = did; 
                                          d_name = d.d_name; 
                                          d_w_id = d.d_w_id;
                                          d_ytd = OCT.inc d.d_ytd amt; 
                                          d_next_o_id = d.d_next_o_id}

let update_district_next_oid did dw dt noid = let d = retrieve_district did dw dt in 
                                               {d_id = did; 
                                                d_name = d.d_name; 
                                                d_w_id = dw;
                                                d_ytd = d.d_ytd; 
                                                d_next_o_id = noid}


let update_customer_after_payment_txn cid cd cw ct amt = let c = retrieve_customer cid cd cw ct in
                                                      {c_id = cid;
                                                       c_d_id = c.c_d_id;
                                                       c_w_id = c.c_w_id;
                                                       c_ytd = OCT.inc c.c_ytd amt;
                                                       c_bal = OCT.dec c.c_ytd amt;
                                                       c_payment_ct = c.c_payment_ct + 1;
                                                       c_delivery_ct = c.c_delivery_ct} 


let update_customer_after_payment_txn cid cd cw ct amt = let c = retrieve_customer cid cd cw ct in
                                                      {c_id = cid;
                                                       c_d_id = c.c_d_id;
                                                       c_w_id = c.c_w_id;
                                                       c_ytd = OCT.inc c.c_ytd amt;
                                                       c_bal = OCT.dec c.c_ytd amt;
                                                       c_payment_ct = c.c_payment_ct + 1;
                                                       c_delivery_ct = c.c_delivery_ct}

let rec update_stock sid s st = match st with 
                             | [] -> failwith "No such stock"
                             | x :: x' ->  if x.s_i_id = sid then s 
                                                             else update_stock sid s x' 

let rec update_district did d dt = match dt with 
                                   | [] -> failwith "No such district"
                                   | x :: x' -> if x.d_id = did then d 
                                                                else update_district did d x'

let get_last_order ol = OO.get ol (List.length ol - 1)

let rec get_price_item_id iid it = match it with 
                               | [] -> failwith "No such item"
                               | x :: x' -> if x.i_id = iid then x.i_price
                                                            else get_price_item_id iid x'

let payment_txn wid did dw cw cd cid hamt wt dt ct ht = 
	let _ = update_warehouse wid wt hamt in
	let _ = update_district_ytd did dw dt hamt in 
	let _ = update_customer_after_payment_txn cid cd cw ct hamt in 
	List.append ht [{h_c_id = cid;
	                h_c_d_id = cd;
	                h_c_w_id = cw;
	                h_d_id = did;
	                h_w_id = wid;
	                h_amt = hamt}]

let new_order_txn wid dw did cw cd cid wt dt ot st it nort olt is = 
	let d = retrieve_district did wid dt in 
	let oid = d.d_next_o_id in 
	let d_next_oid' = d.d_next_o_id + 1 in 
	let _ = update_district did {d_id = did;
	                             d_name = d.d_name;
	                             d_w_id = wid;
	                             d_ytd = d.d_ytd;
	                             d_next_o_id = d_next_oid'} in 
	let o = {o_id = oid; 
	         o_c_id = cid; 
	         o_d_id = did; 
	         o_w_id = wid; 
	         o_ol_ct = List.length is} in 
	let newo = {no_o_id = oid;
	            no_d_id =  did;
	            no_w_id = wid} in 
	let _ = List.append ot [o] in 
	let _ = List.append nort [newo] in 
	let perform_ir r = 
	let sr = retrieve_stock (r.ol_i_id) (r.ol_supply_w_id) st in 
	let ir = retrieve_item (r.ol_i_id) it in 
	let olr = {ol_o_id = oid; 
	           ol_d_id = did;
	           ol_w_id = wid;
	           ol_num = r.ol_num;
	           ol_amt = ir.i_price * r.ol_qty;
	           ol_i_id = r.ol_i_id;
	           ol_supply_w_id = r.ol_supply_w_id;
	           ol_qty = r.ol_qty} in 
	let stqr = if sr.s_qty >= r.ol_qty + 10 
	            then sr.s_qty - r.ol_qty
	            else sr.s_qty - r.ol_qty + 100 in 
	update_stock sr.s_i_id {s_i_id = r.ol_i_id;
	                 s_w_id = r.ol_supply_w_id;
	                 s_qty = stqr;
	                 s_ytd = sr.s_ytd + r.ol_qty;
	                 s_order_cnt = sr.s_order_cnt + 1} st ;
	List.append olt [olr] in 
	let rec update_after_new_order rs = match rs with 
	                                | [] -> failwith "No update"
	                                | x :: x' -> perform_ir x :: update_after_new_order x' in 
	update_after_new_order is 


let rec merge ~ancestor v1 v2 = {wt = OW.merge3 ancestor.wt v1.wt v2.wt;
                                 dt = OD.merge3 ancestor.dt v1.dt v2.dt;
                                 ct = OC.merge3 ancestor.ct v1.ct v2.ct;
                                 ot = OO.merge3 ancestor.ot v1.ot v2.ot;
                                 olt = OL.merge3 ancestor.olt v1.olt v2.olt;
                                 ont = ONO.merge3 ancestor.ont v1.ont v2.ont;
                                 st = OS.merge3 ancestor.st v1.st v2.st;
                                 it = OI.merge3 ancestor.it v1.it v2.it;
                                 irt = OIR.merge3 ancestor.irt v1.irt v2.irt;
                                 ht = OH.merge3 ancestor.ht v1.ht v2.ht}
    





(* let order_status_txn oid cw cd cid ct ot ct = 
	let c = retrieve_customer cid cd cw ct in 
	let o = retrieve_order oid cw cd cid ot in *)





















































 
end 