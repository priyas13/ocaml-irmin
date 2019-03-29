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

(* Tweet contains the id of the person who tweeted, name of the person and the content *)
module Tweet = struct
  type t = {tweet_id: id; author_name: string; content : string}

  let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
end

(* Timeline contains the user to which the timeline belongs, tweet id and the content *)
module Timeline = struct 
 type t = {user_timeline_id : id; user_tweeted_id : id ; tweet_content : string}
   let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
end

(* user_id is the identifier for the user 
   user_name is the name of the user *)
module User = struct 
 type t = {user_id : id; user_name : string; num_followers : int64; num_following : int64}
 let merge ~ancestor:{user_id; user_name; num_followers = q;  num_following=y} 
                     {num_followers=q1; num_following = y1}
                     {num_followers= q2; num_following = y2} =  
    let q' = counter_merge q q1 q2 in
    let y' = counter_merge y y1 y2 in
    {user_id; user_name; num_followers =q'; num_following=y'}
end 

(* u_f_id is the user id to which the 
   follower with the follower_id belongs *)
module Follower = struct 
type t = {u_f_id : id; follower_id : id}
   let merge ~ancestor:lca v1 v2  =  
    failwith "new_order is immutable"
end 

(* u_f_id is the user id to which the 
   following with the following_id belongs *)
module Following = struct 
type t = {u_fl_id: id; following_id : id}
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

module TweetTable = Rbmap.Make(Id)(Tweet)

module TimelineTable = Rbmap.Make(IdPair)(Timeline)

module UserTable = Rbmap.Make(Id)(User)

module FollowerTable = Rbmap.Make(IdPair)(Follower)

module FollowingTable = Rbmap.Make(IdPair)(Following)


type db = {tweet_table: TweetTable.t; 
           timeline_table: TimelineTable.t;
           user_table: UserTable.t;
           follower_table: FollowerTable.t;
           following_table: FollowingTable.t}

module Txn = struct
  type 'a t = db -> 'a*db

  let bind m f = fun db ->
    let (a,db') = m db in
    f a db'

  let return a = fun db -> (a,db)
end

module Insert = struct
  let tweet_table o db = 
    let open Tweet in
    let t = db.tweet_table in
    let t'= TweetTable.insert (o.tweet_id) o t in
        ((),{db with tweet_table=t'})

  let timeline_table no db = 
    let open Timeline in
    let t = db.timeline_table in
    let t'= TimelineTable.insert 
              (no.user_timeline_id, no.user_tweeted_id) no t in
        ((),{db with timeline_table=t'})

  let user_table r db = 
    let open User in
    let t = db.user_table in
    let t' = UserTable.insert
       (r.user_id) r t in
    ((),{db with user_table=t'})

  let follower_table r db = 
    let open Follower in
    let t = db.follower_table in
    let t' = FollowerTable.insert
       (r.u_f_id, r.follower_id) r t in
    ((),{db with follower_table=t'})

  let following_table r db = 
    let open Following in
    let t = db.following_table in
    let t' = FollowingTable.insert
       (r.u_fl_id, r.following_id) r t in
    ((),{db with following_table=t'})

end

module Update = struct 
  let tweet_table sigf updf db = 
    let t = db.tweet_table in
    let t' = TweetTable.update sigf updf t in
    ((), {db with tweet_table=t'})

  let timeline_table sigf updf db = 
    let t = db.timeline_table in
    let t' = TimelineTable.update sigf updf t in
    ((), {db with timeline_table=t'})

  let user_table sigf updf db = 
    let t = db.user_table in
    let t' = UserTable.update sigf updf t in
    ((), {db with user_table=t'})

  let follower_table sigf updf db = 
    let t = db.follower_table in
    let t' = FollowerTable.update sigf updf t in
    ((), {db with follower_table=t'})

  let following_table sigf updf db = 
    let t = db.following_table in
    let t' = FollowingTable.update sigf updf t in
    ((), {db with following_table=t'})

end

module Select1 = struct
  open Printf

  let tweet_table x db =
    try
      let t = db.tweet_table in
      let res = TweetTable.find x t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Tweet<%s> not found"
        (Id.to_string x)

  let timeline_table (x,y) db =
    try
      let t = db.timeline_table in
      let res = TimelineTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Timeline<%s> not found"
        (IdPair.to_string (x,y))

  let user_table x db =
    try
      let t = db.user_table in
      let res = UserTable.find x t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "User<%s> not found"
        (Id.to_string x)

  let follower_table (x,y) db =
    try
      let t = db.follower_table in
      let res = FollowerTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Follower<%s> not found"
        (IdPair.to_string (x,y))

  let following_table (x,y) db =
    try
      let t = db.following_table in
      let res = FollowingTable.find (x,y) t in
      (res, db)
    with Not_found ->
      failwith @@ sprintf "Following<%s> not found"
        (IdPair.to_string (x,y))
end

module Select = struct
  let tweet_table sigf db =
    let t = db.tweet_table in
    let res = TweetTable.select sigf t in
    (res, db)

  let user_table sigf db =
    let t = db.user_table in
    let res = UserTable.select sigf t in
    (res, db)

  let timeline_table sigf db =
    let t = db.timeline_table in
    let res = TimelineTable.select sigf t in
    (res, db)

  let follower_table sigf db =
    let t = db.follower_table in
    let res = FollowerTable.select sigf t in
    (res, db)

  let following_table sigf db =
    let t = db.following_table in
    let res = FollowingTable.select sigf t in
    (res, db)

end

module Delete = struct
  let follower_table (uid, fid) db =
    let t = db.follower_table in
    let t' = FollowerTable.remove (uid, fid) t in
    ((), {db with follower_table=t'})

  let following_table (uid, fid) db =
    let t = db.following_table in
    let t' = FollowingTable.remove (uid, fid) t in
    ((), {db with following_table=t'})
end

module Temp = struct
  let (+) = Int64.add

  let (-) = Int64.sub

  let ( * ) = Int64.mul

  let (>>=) = Txn.bind
end
open Temp

open Tweet
open Timeline
open User
open Follower
open Following


open Printf

let rec get_all_followers_id ft = match ft with 
 | [] -> []
 | x :: xl -> x.follower_id :: get_all_followers_id xl


let new_tweet_txn uid uname tcontent = 
  let _ = printf "new_tweet_txn\n" in 
  let _ = flush_all () in 
  Select.follower_table (fun (ufid, _) -> 
                          Id.compare ufid uid) >>= fun fsuid ->
  let fsuidid = get_all_followers_id fsuid in 
  let new_tweet = {tweet_id = uid; author_name = uname; content = tcontent} in 
  Insert.tweet_table new_tweet >>= fun _ ->
  List.fold_left (fun pre x ->
                      pre >>= fun () ->
                      let tt = {user_timeline_id = x; user_tweeted_id = uid; tweet_content = tcontent} in 
                      Insert.timeline_table tt) (Txn.return()) fsuidid


let delete_follower_txn fid uid = 
  let _ = printf "delete_follower\n" in 
  let _ = flush_all () in 
  Delete.follower_table (uid, fid) >>= fun _ ->
  Delete.following_table (fid, uid) >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (uid))
                    (fun cu -> {cu with num_followers = cu.num_followers - (Int64.of_int 1);
                                        num_following = cu.num_following}) >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (fid))
                    (fun cu -> {cu with num_followers = cu.num_followers ;
                                        num_following = cu.num_following - (Int64.of_int 1)})                 

let follow_txn uid fid = 
  let _ = printf "follow\n" in 
  let _ = flush_all () in 
  let f = {u_f_id = fid; follower_id = uid} in
  let f' = {u_fl_id = uid; following_id = fid} in 
  Insert.following_table f' >>= fun _ ->
  Insert.follower_table f >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (fid))
                    (fun cu -> {cu with num_followers = cu.num_followers + (Int64.of_int 1);
                                        num_following = cu.num_following}) >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (uid))
                    (fun cu -> {cu with num_followers = cu.num_followers;
                                        num_following = cu.num_following + (Int64.of_int 1)})

let unfollow_txn uid fid = 
  let _ = printf "unfollow\n" in 
  let _ = flush_all () in  
  Delete.follower_table (fid, uid) >>= fun _ ->
  Delete.following_table (uid, fid) >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (fid))
                    (fun cu -> {cu with num_followers = cu.num_followers - (Int64.of_int 1);
                                        num_following = cu.num_following}) >>= fun _ ->
  Update.user_table (fun c -> Id.compare c (uid))
                    (fun cu -> {cu with num_followers = cu.num_followers;
                                        num_following = cu.num_following - (Int64.of_int 1)})






