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
                                  (ancestor.w_id = v2.w_id) 
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
                                  (ancestor.d_id = v2.d_id) 
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
 type t = {c_id : atom; c_d_id : atom; c_w_id : atom; c_ytd : int; c_payment_ct : int; c_delivery_ct : int}
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = if (ancestor.c_id = v1.c_id) && 
                                  (v1.c_id = v2.c_id) && 
                                  (ancestor.c_id = v2.c_id) 
                                  then 
                                     {c_id = ancestor.c_id; 
                                      c_d_id = ancestor.c_d_id; 
                                      c_w_id = ancestor.c_w_id;
                                      c_ytd =  OC.merge (ancestor.c_ytd) (v1.c_ytd) (v2.c_ytd);
                                      c_payment_ct = OC.merge (ancestor.c_payment_ct) (v1.c_payment_ct) (v2.c_payment_ct);
                                      c_delivery_ct = OC.merge (ancestor.c_delivery_ct) (v1.c_delivery_ct) (v2.c_delivery_ct)}
                                  else failwith "Merge not possible"
  let resolve x y =  {c_id = x.c_id; 
                      c_d_id = x.c_d_id; 
                      c_w_id = x.c_w_id;
                      c_ytd =  0;
                      c_payment_ct = 0;
                      c_delivery_ct = 0}
end 

(* Order consists of fields representing the order id, o_c_id which represents the customer id corresponding to that order, 
   o_d_id which represents the district id associated with that order, 
   o_w_id which represents the warehouse id associated with that order,
   o_ol_ct which represents the order count *)
module Order = struct 
 module OC = Counter.Make
 type atom = User_id_type.t
 type t = {o_id : atom; o_c_id : atom; o_d_id : atom; o_w_id : atom; o_ol_ct: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.o_id = v1.o_id) && 
                                  (v1.o_id = v2.o_id) && 
                                  (ancestor.o_id = v2.o_id) 
                                  then 
                                     {o_id = ancestor.o_id; 
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
                                  (ancestor.no_o_id = v2.no_o_id) 
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
                                  (ancestor.s_i_id = v2.s_i_id) 
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
                                  (ancestor.i_id = v2.i_id) 
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
                                  (ancestor.h_c_id = v2.h_c_id) 
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
           ol_num: atom; 
           ol_amt: int; 
           ol_i_id: atom; 
           ol_supply_w_id: atom;
           ol_qty: int}
 let merge3 ~ancestor v1 v2 = if (ancestor.ol_o_id = v1.ol_o_id) && 
                                  (v1.ol_o_id = v2.ol_o_id) && 
                                  (ancestor.ol_o_id = v2.ol_o_id) 
                                  then 
                                     {ol_o_id = OC.merge ancestor.ol_o_id v1.ol_o_id v2.ol_o_id; 
                                      ol_d_id = ancestor.ol_d_id; 
                                      ol_w_id = ancestor.ol_w_id; 
                                      ol_num = ancestor.ol_num; 
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

module Tpcc = struct 
module OW = Mvector_list.List(Warehouse)

 
end 