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

(* Item contains the item id, 
   item description, item minimum price/ initial price and the maximum bid price for the time *)
module Item = struct
  type t = {item_id : id; item_desc : string; item_initialprice : int64; item_maxbid : int64}

 let merge ~ancestor:{item_id; item_desc; item_initialprice = q;  item_maxbid = y} 
                     {item_initialprice=q1; item_maxbid=y1}
                     {item_initialprice=q2; item_maxbid=y2} =  
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    {item_id; item_desc; item_initialprice=q'; item_maxbid=y'}
end

(* BuyerWallet consists of user id and the balance it has in its wallet *)
module BuyerWallet = struct 
 type t = {buy_id : id; buyer_balance : int64}

  let merge ~ancestor:{buy_id; buyer_balance = q} 
                     {buyer_balance = q1}
                     {buyer_balance = q2} =  
    let q' = counter_merge q q1 q2 in
    {buy_id; buyer_balance = q'}
end 

(* SellerWallet consists of seller id, item the seller is interested in selling 
   and the balance it has in its wallet *)
module SellerWallet = struct 
 type t = {sell_id : id; sell_i_id : id; seller_balance : int64}

  let merge ~ancestor:{sell_id; sell_i_id; seller_balance = q} 
                     {seller_balance = q1}
                     {seller_balance = q2} =  
    let q' = counter_merge q q1 q2 in
    {sell_id; sell_i_id; seller_balance = q'}
end 

(* Every bid is stored in the bid table, which includes the 
   information about the seller which is represented by w_b_id, the bid is the bid id 
   and the item id for which this bid is and bid_amount is the 
   bid amount *)
module Bid = struct 
 type t = {b_id : id; b_buy_id : id; b_i_id : id; bid_amount : int64}

   let merge ~ancestor:lca v1 v2  =  
    failwith "bid is immutable"
end

module ItemBid = struct 
 type t = {i_id : id; i_buy_id : id}

 let merge ~ancestor:lca v1 v2  =  
    failwith "item bid is immutable"
end

module WalletBid = struct 
 type t = {w_buy_id : id; w_b_id : id; timestamp : float option} 
     let merge ~ancestor:lca v1 v2  =  
    failwith "walletbid is immutable"
 end 

module WalletItem = struct 
 type t = {w_id : id; w_i_id : id}
        let merge ~ancestor:lca v1 v2  =  
    failwith "walletitem is immutable"
end 

module Penality = struct 
 type t = {u_p_id : id; penality_amount : int64}
      let merge ~ancestor:lca v1 v2  =  
    failwith "penality is immutable"
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

module ItemTable = Rbmap.Make(Id)(Item)

module BuyerWalletTable = Rbmap.Make(Id)(BuyerWallet)

module SellerWalletTable = Rbmap.Make(IdPair)(SellerWallet)

module BidTable = Rbmap.Make(IdTriple)(Bid)

module ItemBidTable = Rbmap.Make(IdPair)(ItemBid)

module WalletBidTable = Rbmap.Make(IdPair)(WalletBid)

module WalletItemTable = Rbmap.Make(IdPair)(WalletItem)

module PenalityTable = Rbmap.Make(Id)(Penality)

type db = {item_table: ItemTable.t; 
           buyerwallet_table: BuyerWalletTable.t;
           sellerwallet_table : SellerWalletTable.t;
           bid_table: BidTable.t;
           itembid_table: ItemBidTable.t;
           walletitem_table : WalletItemTable.t;
           walletbid_table: WalletBidTable.t;
           penality_table: PenalityTable.t}

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
  let item_table o db = 
    let open Item in
    let t = db.item_table in
    let t'= ItemTable.insert (o.item_id) o t in
        ((),{db with item_table=t'})

  let buyerwallet_table o db = 
    let open BuyerWallet in
    let t = db.buyerwallet_table in
    let t'= BuyerWalletTable.insert (o.buy_id) o t in
        ((),{db with buyerwallet_table=t'})

  let sellerwallet_table o db = 
    let open SellerWallet in
    let t = db.sellerwallet_table in
    let t'= SellerWalletTable.insert (o.sell_id, o.sell_i_id) o t in
        ((),{db with sellerwallet_table=t'})

  let bid_table o db = 
    let open Bid in
    let t = db.bid_table in
    let t'= BidTable.insert (o.b_id, (o.b_buy_id, o.b_i_id)) o t in
        ((),{db with bid_table=t'})

  let itembid_table o db = 
    let open ItemBid in
    let t = db.itembid_table in
    let t'= ItemBidTable.insert (o.i_id, o.i_buy_id) o t in
        ((),{db with itembid_table=t'})

  let walletbid_table o db = 
    let open WalletBid in
    let t = db.walletbid_table in
    let t'= WalletBidTable.insert (o.w_buy_id, o.w_b_id) o t in
        ((),{db with walletbid_table=t'})

  let walletitem_table o db = 
    let open WalletItem in
    let t = db.walletitem_table in
    let t'= WalletItemTable.insert (o.w_id, o.w_i_id) o t in
        ((),{db with walletitem_table=t'})

  let penality_table o db = 
    let open Penality in
    let t = db.penality_table in
    let t'= PenalityTable.insert (o.u_p_id) o t in
        ((),{db with penality_table=t'})
end

module Update = struct 
  let item_table sigf updf db = 
    let t = db.item_table in
    let t' = ItemTable.update sigf updf t in
    ((), {db with item_table=t'})

  let buyerwallet_table sigf updf db = 
    let t = db.buyerwallet_table in
    let t' = BuyerWalletTable.update sigf updf t in
    ((), {db with buyerwallet_table=t'})

  let sellerwallet_table sigf updf db = 
    let t = db.sellerwallet_table in
    let t' = SellerWalletTable.update sigf updf t in
    ((), {db with sellerwallet_table=t'})

  let bid_table sigf updf db = 
    let t = db.bid_table in
    let t' = BidTable.update sigf updf t in
    ((), {db with bid_table=t'})

  let itembid_table sigf updf db = 
    let t = db.itembid_table in
    let t' = ItemBidTable.update sigf updf t in
    ((), {db with itembid_table=t'})

  let walletbid_table sigf updf db = 
    let t = db.walletbid_table in
    let t' = WalletBidTable.update sigf updf t in
    ((), {db with walletbid_table=t'})

  let walletitem_table sigf updf db = 
    let t = db.walletitem_table in
    let t' = WalletItemTable.update sigf updf t in
    ((), {db with walletitem_table=t'})

  let penality_table sigf updf db = 
    let t = db.penality_table in
    let t' = PenalityTable.update sigf updf t in
    ((), {db with penality_table=t'})
end

module Delete = struct

end

module Select1 = struct
  open Printf

  let item_table x db =
    try
      let t = db.item_table in
      let res = ItemTable.find x t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "item<%s> not found"
        (Id.to_string x)

  let buyerwallet_table (x) db =
    try
      let t = db.buyerwallet_table in
      let res = BuyerWalletTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "buyerwallet<%s> not found"
        (Id.to_string (x))

  let sellerwallet_table (x,y) db =
    try
      let t = db.sellerwallet_table in
      let res = SellerWalletTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "sellerwallet<%s> not found"
        (IdPair.to_string (x, y))

  let bid_table (x, y, z) db =
    try
      let t = db.bid_table in
      let res = BidTable.find (x, (y,z)) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Bid<%s> not found"
        (IdTriple.to_string (x, (y,z)))

  let itembid_table (x,y) db =
    try
      let t = db.itembid_table in
      let res = ItemBidTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Itembid<%s> not found"
        (IdPair.to_string (x,y))

  let walletbid_table (x,y) db =
    try
      let t = db.walletbid_table in
      let res = WalletBidTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "WalletBid<%s> not found"
        (IdPair.to_string (x,y))

  let walletitem_table (x,y) db =
    try
      let t = db.walletitem_table in
      let res = WalletItemTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Walletitem<%s> not found"
        (IdPair.to_string (x,(y)))

  let penality_table (x) db =
    try
      let t = db.penality_table in
      let res = PenalityTable.find (x) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "penality<%s> not found"
        (Id.to_string (x))
end

module Select = struct
  let item_table sigf db =
    let t = db.item_table in
    let res = ItemTable.select sigf t in
    (res, db)

  let buyerwallet_table sigf db =
    let t = db.buyerwallet_table in
    let res = BuyerWalletTable.select sigf t in
    (res, db)

  let sellerwallet_table sigf db =
    let t = db.sellerwallet_table in
    let res = SellerWalletTable.select sigf t in
    (res, db)

  let bid_table sigf db =
    let t = db.bid_table in
    let res = BidTable.select sigf t in
    (res, db)

  let itembid_table sigf db =
    let t = db.itembid_table in
    let res = ItemBidTable.select sigf t in
    (res, db)

  let walletbid_table sigf db =
    let t = db.walletbid_table in
    let res = WalletBidTable.select sigf t in
    (res, db)

  let walletitem_table sigf db =
    let t = db.walletitem_table in
    let res = WalletItemTable.select sigf t in
    (res, db)

  let penality_table sigf db =
    let t = db.walletbid_table in
    let res = WalletBidTable.select sigf t in
    (res, db)
end
 

module Temp = struct
  let (+) = Int64.add

  let (-) = Int64.sub

  let ( * ) = Int64.mul

  let (>>=) = Txn.bind
end
open Temp

open Item
open BuyerWallet
open SellerWallet
open Bid
open ItemBid
open WalletBid
open WalletItem
open Penality

open Printf

let rec get_bid_amounts bids = match bids with 
 | [] -> [Int64.of_int 0]
 | x :: xs -> x.bid_amount :: get_bid_amounts xs 

let get_max_bid bids = match bids with 
 | [] -> Int64.of_int 0
 | x :: xs -> List.fold_left max (x.bid_amount) (get_bid_amounts xs)

let rec get_max_bid_users biamt bids = match bids with 
 | [] -> failwith "no such buyer"
 | x :: xs -> if x.bid_amount = biamt then x.b_buy_id :: get_max_bid_users biamt xs 
              else get_max_bid_users biamt xs

let rec check_balance_users bids = match bids with 
 | [] -> failwith "no buyer"
 | x :: xs -> if x.buyer_balance < Int64.of_int 0 then x :: check_balance_users xs 
              else check_balance_users xs 

let deposit_to_wallet_txn wid amt = 
   Update.buyerwallet_table (fun waid -> Id.compare waid wid) 
                       (fun w -> {w with buyer_balance = w.buyer_balance + amt})

let new_bid_txn bid wid iid amt = 
   let nb = {b_id = bid; b_buy_id = wid; b_i_id = iid; bid_amount = amt} in
   let inb = {i_id = iid; i_buy_id = bid} in 
   let wnb = {w_buy_id = wid; w_b_id = bid; timestamp = Some (Unix.time())} in 
   let wni = {w_id = wid; w_i_id = iid} in 
   Insert.bid_table nb >>= fun _ ->
   Insert.itembid_table inb >>= fun _ ->
   Insert.walletbid_table wnb >>= fun _ ->
   Insert.walletitem_table wni 

let get_maxbid_for_an_item_txn iid = 
   Select.itembid_table (fun (x,_) -> Id.compare x iid) >>= fun itbs ->
   Select.walletitem_table (fun (_, y) -> Id.compare y iid) >>= fun wids ->
   Select.bid_table (fun (_,(_,z)) -> Id.compare z iid) >>= fun bids ->
   let bamt = get_max_bid bids in 
   Update.item_table (fun x -> Id.compare x iid)
                     (fun i -> {i with item_maxbid = i.item_maxbid + (bamt - i.item_maxbid)}) 

let auction_txn sid iid =
   Select.itembid_table (fun (x,_) -> Id.compare x iid) >>= fun itbs ->
   Select.walletitem_table (fun (_, y) -> Id.compare y iid) >>= fun wids ->
   Select.bid_table (fun (_,(_,z)) -> Id.compare z iid) >>= fun bids ->
   let bamt = get_max_bid bids in 
   Update.item_table (fun x -> Id.compare x iid)
                     (fun i -> {i with item_maxbid = i.item_maxbid + (bamt - i.item_maxbid)}) >>= fun _ ->
   let buyids = get_max_bid_users bamt bids in 
   Update.sellerwallet_table (fun (s, z) -> IdPair.compare (s,z) (sid, iid))
                             (fun sl -> {sl with seller_balance = sl.seller_balance + bamt}) >>= fun _ ->
   List.fold_left (fun pre x ->
                   pre >>= fun () ->
                   Update.buyerwallet_table (fun i -> Id.compare i x)
                                            (fun b -> {b with buyer_balance = b.buyer_balance - bamt})) 
                   (Txn.return ()) buyids

let update_penality_for_buyers_txn bid = 
  Select1.buyerwallet_table (bid) >>= fun lbuyer ->  
  let lbalance = lbuyer.buyer_balance in 
  let _ = if (lbalance < Int64.of_int 0) 
  then (Update.penality_table 
              (fun x -> Id.compare x bid)
              (fun p -> {p with penality_amount = p.penality_amount + Int64.of_int (-Int64.to_int (lbuyer.buyer_balance))}))
                               else (Update.penality_table (fun x -> Id.compare x bid)
                                                          (fun p -> {p with penality_amount = p.penality_amount})) in 
  Update.buyerwallet_table (fun x -> Id.compare x bid)
                           (fun b -> {b with buyer_balance = Int64.of_int (- Int64.to_int (b.buyer_balance))})








