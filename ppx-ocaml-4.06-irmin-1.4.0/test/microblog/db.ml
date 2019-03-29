open Printf
open Microblog
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


let load_tweet i = 
  let open Tweet in 
  let tweet_id = Id.of_int i in
  let author_name = sprintf "tweet %d" i in
  let content = sprintf "content %d" i in 
  let tweet = {tweet_id; author_name; content} in
  Insert.tweet_table tweet

let load_timeline i j = 
  let open Timeline in 
  let user_timeline_id = Id.of_int i in
  let user_tweeted_id = Id.of_int j in
  let tweet_content = sprintf "tweet %d" j in
  let timeline = {user_timeline_id; user_tweeted_id; tweet_content} in
  Insert.timeline_table timeline

let load_user i = 
  let open User in 
  let user_id = Id.of_int i in
  let user_name = sprintf "tweet %d" i in
  let num_followers = 25L in 
  let num_following = 25L in 
  let user = {user_id; user_name; num_followers; num_following} in
  Insert.user_table user

let load_follower i j =
  let open Follower in
  let u_f_id = Id.of_int i in
  let follower_id = Id.of_int j in
  Insert.follower_table {u_f_id; follower_id}

let load_following i j =
  let open Following in
  let u_fl_id = Id.of_int i in
  let following_id = Id.of_int j in
  Insert.following_table {u_fl_id; following_id}

let load_tweet () = 
  U.fold (fun i pre -> pre >>= fun () -> 
                      load_tweet i)
    400 (Txn.return ())

let load_timeline () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_timeline i j)
      40 pre1)
    40 (Txn.return ())

let load_user () = 
  U.fold (fun i pre -> pre >>= fun () -> 
                      load_user i)
    40 (Txn.return ())

let load_follower () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_follower i j)
      40 pre1)
    40 (Txn.return ())

let load_following () =
  U.fold (fun i pre1 -> 
    U.fold (fun j pre2 ->
        pre2 >>= fun () -> load_following i j)
      40 pre1)
    40 (Txn.return ())

let populate () = 
  begin 
    load_tweet() >>= fun _ ->
    load_timeline() >>= fun _ ->
    load_user() >>= fun _ ->
    load_follower() >>= fun _ ->
    load_following() 
  end

let empty () =
  {tweet_table= TweetTable.empty; 
   timeline_table= TimelineTable.empty;
   user_table= UserTable.empty;
   follower_table= FollowerTable.empty;
   following_table= FollowingTable.empty}

let do_new_tweet_txn db = 
  let tweet_id = Id.random 40 in 
  let author_name = sprintf "tweet %d" (Int64.to_int tweet_id) in 
  let content = sprintf "tweet %d" (Int64.to_int tweet_id) in 
  snd @@ new_tweet_txn tweet_id author_name content db 

let do_delete_follower_txn db =
  let fid = Id.random 40 in 
  let uid = Id.random 40 in 
  snd @@ delete_follower_txn fid uid db 

let do_follow_txn db =
  let uid = Id.random 40 in 
  let fid = Id.random 40 in 
  snd @@ delete_follower_txn uid fid db 

let do_unfollow_txn db =
  let uid = Id.random 40 in 
  let fid = Id.random 40 in 
  snd @@ delete_follower_txn uid fid db 

let dump_keys db = 
  let fp = open_out "keys.db" in
  let dump1 k v = 
    fprintf fp "%s\n" @@ Id.to_string k in
  let dump2 k v = 
    fprintf fp "%s\n" @@ IdPair.to_string k in
  begin 
    fprintf fp "\n> Tweet\n";
    TweetTable.iter dump1 db.tweet_table;
    fprintf fp "\n> Timeline\n";
    TimelineTable.iter dump2 db.timeline_table;
    fprintf fp "\n> User\n";
    UserTable.iter dump1 db.user_table;
    fprintf fp "\n> Follower\n";
    FollowerTable.iter dump2 db.follower_table;
    fprintf fp "\n> Following\n";
    FollowingTable.iter dump2 db.following_table;
    close_out fp;
    printf "Dumped keys in keys.db\n";
  end
  
