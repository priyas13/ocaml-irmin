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
  let (+) a b = Int32.add a b in
  let (-) a b = Int32.sub a b in
  lca + (v1-lca) + (v2-lca)

let minf f l = 
  List.fold_left 
    (fun acc x -> match acc with
       | None -> Some x
       | Some x' -> if Int64.compare (f x) (f x') < 0 
                  then Some x else acc)
    None l

(* Tweet contains the id of the person who tweeted, name of the person and the content *)
module Tweet = struct
  type t = {tweet_id: id; author_name: string; content : string}

  let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
end

(* Timeline contains the tweet id and the content *)
module Timeline = struct 
 type t = {tweet_id : id; content : string}
   let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
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

module WarehouseTable = Rbmap.Make(Id)(Warehouse)

module DistrictTable = Rbmap.Make(IdPair)(District)

module OrderTable = Rbmap.Make(IdTriple)(Order)

module NewOrderTable = Rbmap.Make(IdTriple)(NewOrder)

module OrderLineTable = Rbmap.Make(IdQuad)(OrderLine)

module ItemTable = Rbmap.Make(Id)(Item)

module HistTable = Rbmap.Make(IdQuin)(Hist)

module StockTable = Rbmap.Make(IdPair)(Stock)

module CustomerTable = Rbmap.Make(IdTriple)(Customer)

type db = {warehouse_table: WarehouseTable.t; 
           district_table: DistrictTable.t;
           order_table: OrderTable.t;
           neworder_table: NewOrderTable.t;
           orderline_table: OrderLineTable.t;
           item_table: ItemTable.t;
           hist_table: HistTable.t;
           stock_table: StockTable.t;
           customer_table: CustomerTable.t}

(*

let update: type s k v. (module Rbmap.S with type t=s 
                          and type k=k and type v=v) 
            -> (db -> s) -> (k -> int) -> (v -> v) -> db -> unit  = 
  fun (module T) pif sigf updf db ->
    let tab = pif db in
    let tab' = T.update sigf updf tab in
    (pif db) <- tab'
*)

module Txn = struct
  type 'a t = db -> 'a*db

  let bind m f = fun db ->
    let (a,db') = m db in
    f a db'

  let return a = fun db -> (a,db)
end

module Insert = struct
  let order_table o db = 
    let open Order in
    let t = db.order_table in
    let t'= OrderTable.insert (o.o_w_id, (o.o_d_id, o.o_id)) o t in
        ((),{db with order_table=t'})

  let neworder_table no db = 
    let open NewOrder in
    let t = db.neworder_table in
    let t'= NewOrderTable.insert 
              (no.no_w_id, (no.no_d_id, no.no_o_id)) no t in
        ((),{db with neworder_table=t'})

  let orderline_table r db = 
    let open OrderLine in
    let t = db.orderline_table in
    let t' = OrderLineTable.insert
       (r.ol_w_id, (r.ol_d_id, (r.ol_o_id, r.ol_i_id))) r t in
    ((),{db with orderline_table=t'})

  let hist_table r db = 
    let open Hist in
    let t = db.hist_table in
    let t' = HistTable.insert
       (r.h_w_id, (r.h_d_id, (r.h_c_w_id, (r.h_c_d_id, r.h_c_id)))) r t in
    ((),{db with hist_table=t'})

  let item_table o db = 
    let open Item in
    let t = db.item_table in
    let t'= ItemTable.insert (o.i_id) o t in
        ((),{db with item_table=t'})

  let warehouse_table o db = 
    let open Warehouse in
    let t = db.warehouse_table in
    let t'= WarehouseTable.insert (o.w_id) o t in
        ((),{db with warehouse_table=t'})

  let district_table o db = 
    let open District in
    let t = db.district_table in
    let t'= DistrictTable.insert (o.d_w_id, o.d_id) o t in
        ((),{db with district_table=t'})

  let customer_table o db = 
    let open Customer in
    let t = db.customer_table in
    let t'= CustomerTable.insert (o.c_w_id, (o.c_d_id, o.c_id)) o t in
        ((),{db with customer_table=t'})

  let stock_table o db = 
    let open Stock in
    let t = db.stock_table in
    let t'= StockTable.insert (o.s_w_id, o.s_i_id) o t in
        ((),{db with stock_table=t'})

end

module Update = struct 
  let warehouse_table sigf updf db = 
    let t = db.warehouse_table in
    let t' = WarehouseTable.update sigf updf t in
    ((), {db with warehouse_table=t'})

  let district_table sigf updf db = 
    let t = db.district_table in
    let t' = DistrictTable.update sigf updf t in
    ((), {db with district_table=t'})

  let order_table sigf updf db = 
    let t = db.order_table in
    let t' = OrderTable.update sigf updf t in
    ((), {db with order_table=t'})

  let orderline_table sigf updf db = 
    let t = db.orderline_table in
    let t' = OrderLineTable.update sigf updf t in
    ((), {db with orderline_table=t'})

  let stock_table sigf updf db = 
    let t = db.stock_table in
    let t' = StockTable.update sigf updf t in
    ((), {db with stock_table=t'})

  let customer_table sigf updf db = 
    let t = db.customer_table in
    let t' = CustomerTable.update sigf updf t in
    ((), {db with customer_table=t'})
end

module Delete = struct
  let neworder_table (no_w_id, no_d_id, no_o_id) db =
    let t = db.neworder_table in
    let t' = NewOrderTable.remove (no_w_id, (no_d_id,no_o_id)) t in
    ((), {db with neworder_table=t'})
end

module Select1 = struct
  open Printf

  let warehouse_table x db =
    try
      let t = db.warehouse_table in
      let res = WarehouseTable.find x t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Warehouse<%s> not found"
        (Id.to_string x)

  let district_table (x,y) db =
    try
      let t = db.district_table in
      let res = DistrictTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "District<%s> not found"
        (IdPair.to_string (x,y))

  let item_table x db =
    try
      let t = db.item_table in
      let res = ItemTable.find x t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Item<%s> not found"
        (Id.to_string x)

  let order_table (x,y,z) db =
    try
      let t = db.order_table in
      let res = OrderTable.find (x,(y,z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Order<%s> not found"
        (IdTriple.to_string (x,(y,z)))

  let stock_table (x,y) db =
    try
      let t = db.stock_table in
      let res = StockTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Stock<%s> not found"
        (IdPair.to_string (x,y))

  let customer_table (x,y,z) db =
    try
      let t = db.customer_table in
      let res = CustomerTable.find (x,(y,z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Customer<%s> not found"
        (IdTriple.to_string (x,(y,z)))
end

module Select = struct
  let warehouse_table sigf db =
    let t = db.warehouse_table in
    let res = WarehouseTable.select sigf t in
    (res, db)

  let district_table sigf db =
    let t = db.district_table in
    let res = DistrictTable.select sigf t in
    (res, db)

  let item_table sigf db =
    let t = db.item_table in
    let res = ItemTable.select sigf t in
    (res, db)

  let order_table sigf db =
    let t = db.order_table in
    let res = OrderTable.select sigf t in
    (res, db)

  let orderline_table sigf db =
    let t = db.orderline_table in
    let res = OrderLineTable.select sigf t in
    (res, db)

  let neworder_table sigf db =
    let t = db.neworder_table in
    let res = NewOrderTable.select sigf t in
    (res, db)

  let stock_table sigf db =
    let t = db.stock_table in
    let res = StockTable.select sigf t in
    (res, db)

  let customer_table sigf db =
    let t = db.customer_table in
    let res = CustomerTable.select sigf t in
    (res, db)

  let hist_table sigf db =
    let t = db.hist_table in
    let res = HistTable.select sigf t in
    (res, db)
end

module Temp = struct
  let (+) = Int32.add

  let (-) = Int32.sub

  let ( * ) = Int32.mul

  let (>>=) = Txn.bind
end
open Temp

open Warehouse
open District
open Order
open NewOrder
open OrderLine
open Item
open Stock
open Customer
open Hist

open Printf

let dump_stock_keys db = 
  let fp = open_out "stock_keys.db" in
  let dump2 k v = 
    fprintf fp "%s\n" @@ IdPair.to_string k in
  begin 
    fprintf fp "\n> Stock\n";
    StockTable.iter dump2 db.stock_table;
    close_out fp;
    printf "Dumped keys in stock_keys.db\n";
    ((),db)
  end

let new_order_txn w_id d_id c_id (ireqs: item_req list) : unit Txn.t  =
  let _ = printf "new_order_txn\n" in
  let _ = flush_all () in
  let o_id = Random.int64 10000000000L in
  let ord = let open Order in
            {o_id=o_id; o_w_id=w_id; 
             o_d_id=d_id; o_c_id=c_id; 
             o_ol_cnt=Int32.of_int @@ List.length ireqs; 
             o_carrier_id =false} in
  let new_ord = let open NewOrder in
                {no_o_id=o_id; no_w_id=w_id; no_d_id=d_id} in
  Insert.order_table ord >>= fun _ ->
  Insert.neworder_table new_ord >>= fun _ ->
  List.fold_left
    (fun pre ireq -> 
       pre>>= fun () ->
       (*dump_stock_keys >>= fun () ->*)
       Select1.stock_table (ireq._ol_supply_w_id, 
                            ireq._ol_i_id) >>= fun stk ->
       Select1.item_table ireq._ol_i_id >>= fun item ->
       let ol = {ol_o_id=o_id; ol_d_id=d_id; ol_w_id=w_id; 
                 ol_num=ireq._ol_num; ol_i_id=ireq._ol_i_id; 
                 ol_supply_w_id=ireq._ol_supply_w_id; 
                 ol_amt=item.i_price * ireq._ol_qty;
                 ol_qty=ireq._ol_qty; ol_delivery_d=None} in
       let s_qty = if Int32.compare stk.s_qty 
                       (ireq._ol_qty + 10l) >= 0
                   then stk.s_qty - ireq._ol_qty
                   else stk.s_qty - ireq._ol_qty + 91l in
       Update.stock_table 
           (fun stock_key -> IdPair.compare stock_key
                 (ireq._ol_supply_w_id, ireq._ol_i_id))
           (fun s -> 
              { s with
                s_qty = s_qty;
                s_ytd = stk.s_ytd + ireq._ol_qty; 
                s_order_cnt = stk.s_order_cnt + 1l}) >>= fun () ->
        Insert.orderline_table ol) 
    (Txn.return ())
    ireqs

let payment_txn w_id d_id c_id h_amt = 
  let _ = printf "payment_txn\n" in
  let _ = flush_all () in
  let d_w_id = w_id in
  let c_w_id = w_id in
  let c_d_id = d_id in
  Select1.warehouse_table (w_id) >>= fun w ->
  Select1.district_table (d_w_id, d_id) >>= fun d ->
  Select1.customer_table (c_w_id, c_d_id, c_id) >>= fun c ->
  let h_item = {h_c_id = c_id; h_c_d_id = c_d_id; h_c_w_id = c_w_id; 
                h_d_id = d_id; h_w_id = w_id; h_amt = h_amt} in
  Update.warehouse_table
      (fun wh_id -> Id.compare wh_id w_id)
      (fun wh -> {wh with w_ytd = wh.w_ytd + h_amt}) >>= fun () ->
  Update.district_table
    (fun dist_key -> 
       IdPair.compare dist_key (d_w_id,d_id))
    (fun dist -> {dist with 
                    d_ytd = dist.d_ytd + h_amt}) >>= fun () ->
  Update.customer_table
    (fun ckey -> IdTriple.compare ckey
                  (c_w_id, (c_d_id, c_id)))
    (fun c -> 
       {c with c_bal = c.c_bal - h_amt;
               c_ytd_payment = c.c_ytd_payment + h_amt;
               c_payment_cnt = c.c_payment_cnt + 1l}) >>= fun () ->
  Insert.hist_table h_item


let delivery_txn w_id =
  let _ = printf "delivery_txn\n" in
  let _ = flush_all () in
  Select.district_table 
    (fun (dist_w_id,_) -> 
       Id.compare dist_w_id w_id) >>= fun dists ->
  List.fold_left 
    (fun pre d -> 
       pre >>= fun () ->
       Select.neworder_table
         (fun (no_w_id, (no_d_id,_)) -> 
            IdPair.compare 
              (no_w_id, no_d_id) (w_id, d.d_id)) >>= fun nords ->
       let nop = minf (fun no -> no.no_o_id) nords in
       match nop with 
         | Some no ->
           begin 
             (* delete the no entry *)
             Delete.neworder_table
               (no.no_w_id, no.no_d_id, no.no_o_id) >>= fun () ->
             Select1.order_table (w_id, d.d_id, no.no_o_id) >>= fun o ->
             Update.order_table
               (fun okey -> IdTriple.compare okey
                               (w_id, (d.d_id, no.no_o_id))) 
               (fun o -> {o with o_carrier_id = true}) >>= fun () ->
             Update.orderline_table
               (fun (k_w_id, (k_d_id, (k_o_id, _))) -> 
                  IdTriple.compare (k_w_id, (k_d_id, k_o_id)) 
                          (o.o_w_id, (o.o_d_id, o.o_id)))
               (fun ol -> 
                  {ol with ol_delivery_d = 
                             Some (Unix.time ())}) >>= fun () ->
             Select.orderline_table
              (fun (k_w_id, (k_d_id, (k_o_id, _))) -> 
                  IdTriple.compare (k_w_id, (k_d_id, k_o_id)) 
                          (o.o_w_id, (o.o_d_id, o.o_id))) >>= fun ols ->
             let amt = List.fold_left (fun acc ol -> 
                                        acc + ol.ol_amt) 0l ols in
             Update.customer_table
              (fun ckey -> IdTriple.compare ckey
                              (w_id, (d.d_id, o.o_c_id)))
              (fun c -> 
                 {c with c_bal = c.c_bal + amt;
                         c_delivery_cnt = c.c_delivery_cnt + 1l})
           end
         | None -> Txn.return ()) 
    (Txn.return ()) dists
