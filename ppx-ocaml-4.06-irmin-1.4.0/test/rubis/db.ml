open Printf
open Rubis
module U = Utils

module List = struct 
  include List

 	let init ~f n =
    let rec aux i = if i = n then [] else f i :: aux (i+1) in
		aux 0 
end


let seed = 80032723

let _ = Random.init seed

let (>>=) = Txn.bind


let load_item i = 
  let open Item in 
  let item_id = Id.of_int i in
  let item_desc = sprintf "item %d" i in 
  let item_initialprice = 1000L in 
  let item_maxbid = 0L in 
  let item = {item_id; item_desc; item_initialprice; item_maxbid} in
  Insert.item_table item

let load_buyerwallet i =
  let open BuyerWallet in
  let buy_id = Id.of_int i in
  let buyer_balance = 300000L in 
  Insert.buyerwallet_table {buy_id; buyer_balance}

let load_sellerwallet i j =
  let open SellerWallet in
  let sell_id = Id.of_int i in
  let sell_i_id = Id.of_int j in 
  let seller_balance = 300000L in 
  Insert.sellerwallet_table {sell_id; sell_i_id; seller_balance}

let load_bid i j k =
  let open Bid in
  let b_id = Id.of_int i in
  let b_buy_id = Id.of_int j in
  let b_i_id = Id.of_int k in 
  let bid_amount = U.bounded_random_int64 1000L 2000L in 
  Insert.bid_table {b_id; b_buy_id; b_i_id; bid_amount}

let load_itembid i j =
  let open ItemBid in
  let i_id = Id.of_int i in
  let i_buy_id = Id.of_int j in
  Insert.itembid_table {i_id; i_buy_id}

let load_walletbid i j =
  let open WalletBid in
  let w_buy_id = Id.of_int i in
  let w_b_id = Id.of_int j in
  let timestamp = Some (Unix.time()) in 
  Insert.walletbid_table {w_buy_id; w_b_id; timestamp}

let load_walletitem i j =
  let open WalletItem in
  let w_id = Id.of_int i in
  let w_i_id = Id.of_int j in
  Insert.walletitem_table {w_id; w_i_id}

let load_penality i =
  let open Penality in
  let u_p_id = Id.of_int i in
  let penality_amount = 0L in
  Insert.penality_table {u_p_id; penality_amount}

let load_item () =
  U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_item i)
     300 (Txn.return ())

let load_buyerwallet () =
  U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_buyerwallet i)
      10 (Txn.return ())

let load_sellerwallet () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_sellerwallet i j)
      300 pre1)
    1 (Txn.return ())

let load_bid () = 
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 -> 
      U.fold (fun k pre3 ->
          pre3 >>= fun () -> load_bid i j k)
        300 pre2)
      10 pre1)
    10 (Txn.return ())

let load_itembid () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_itembid i j)
      10 pre1)
    300 (Txn.return ())

let load_walletbid () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_walletbid i j)
      10 pre1)
    10 (Txn.return ())

let load_walletitem () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_walletitem i j)
      300 pre1)
    10 (Txn.return ())

let load_penality () =
  U.fold (fun i pre1 -> 
        pre1 >>= fun () -> load_penality i)
      10 (Txn.return ())

let populate () = 
  begin 
    load_item() >>= fun _ ->
    load_buyerwallet() >>= fun _ ->
    load_sellerwallet() >>= fun _ ->
    load_bid() >>= fun _ ->
    load_itembid() >>= fun _ ->
    load_walletbid() >>= fun _ ->
    load_walletitem() >>= fun _ ->
    load_penality ()
  end

let empty () =
  {item_table= ItemTable.empty; 
   buyerwallet_table= BuyerWalletTable.empty;
   sellerwallet_table= SellerWalletTable.empty;
   bid_table= BidTable.empty;
   itembid_table= ItemBidTable.empty;
   walletbid_table= WalletBidTable.empty;
   walletitem_table= WalletItemTable.empty;
   penality_table= PenalityTable.empty}

let do_deposit_to_wallet_txn db =
  let wid = Id.random 10 in 
  let amt = U.bounded_random_int64 1000L 20000L in 
  snd @@ deposit_to_wallet_txn wid amt db  


let do_get_maxbid_for_an_item_txn db = 
  let iid = Id.random 300 in 
  snd @@ get_maxbid_for_an_item_txn iid db 

let do_auction_txn db = 
  let sid = Id.random 1 in 
  let iid = Id.random 300 in 
  snd @@ auction_txn sid iid db 

let do_update_penality_for_buyers_txn db = 
  let bid = Id.random 10 in 
  snd @@ update_penality_for_buyers_txn bid db 


let do_new_bid_txn db = 
  let bid = Id.random 10 in 
  let wid = Id.random 10 in 
  let iid = Id.random 300 in 
  let amt = U.bounded_random_int64 1000L 2000L in 
  snd @@ new_bid_txn bid wid iid amt db 

let dump_keys db = 
  let fp = open_out "keys.db" in
  let dump1 k v = 
    fprintf fp "%s\n" @@ Id.to_string k in
  let dump2 k v = 
    fprintf fp "%s\n" @@ IdPair.to_string k in
  let dump3 k v = 
    fprintf fp "%s\n" @@ IdTriple.to_string k in
  begin 
    fprintf fp "\n> Item\n";
    ItemTable.iter dump1 db.item_table;
    fprintf fp "\n> BuyerWallet\n";
    BuyerWalletTable.iter dump1 db.buyerwallet_table;
    fprintf fp "\n> SellerWallet\n";
    SellerWalletTable.iter dump2 db.sellerwallet_table;
    fprintf fp "\n> Bid\n";
    BidTable.iter dump3 db.bid_table;
    fprintf fp "\n> ItemBid\n";
    ItemBidTable.iter dump2 db.itembid_table;
    fprintf fp "\n> WalletBid\n";
    WalletBidTable.iter dump2 db.walletbid_table;
    fprintf fp "\n> WalletItem\n";
    WalletItemTable.iter dump2 db.walletitem_table;
    fprintf fp "\n> Penality\n";
    PenalityTable.iter dump1 db.penality_table;
    close_out fp;
    printf "Dumped keys in keys.db\n";
  end
  
