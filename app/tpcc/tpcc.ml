module User_id_type = 
 struct 
 type t = char
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = '#'
 let resolve x y = '#'
end 

module Warehouse = struct 
  type atom = User_id_type.t
  type t = {w_id : atom; w_name : string; w_state : string; w_ytd : int}
end 

module District = struct 
  type atom = User_id_type.t 
  type t = {d_id : atom; d_name: string; d_w_id: atom; d_ytd : int; d_next_o_id : int}
end 

module Customer = struct 
 type atom = User_id_type.t 
 type t = {c_id : atom; c_d_id : atom; c_w_id : atom; c_ytd : int; c_payment_ct : int; c_delivery_ct : int}
end 

module Order = struct 
 type atom = User_id_type.t
 type t = {o_id : atom; o_c_id : atom; o_d_id : atom; o_w_id : atom; o_ol_ct: int}
end

module Neworder = struct 
 type atom = User_id_type.t 
 type t = {no_o_id: int; no_d_id: id; no_w_id: id}
end 

module Stock = struct 
 type atom = User_id_type.t
 type t = {s_i_id: id; s_w_id: id; s_qty: int; s_ytd: int; s_order_cnt: int}
end 

module Item = struct
 type atom = User_id_type.t
 type t = {i_id: atom; i_name: string; i_price: int}
end 

module History = struct 
 type atom = User_id_type.t
 type t = {h_c_id: atom; h_c_d_id: atom; h_c_w_id: atom; h_d_id: atom; h_w_id: atom; h_amt: int}
end 