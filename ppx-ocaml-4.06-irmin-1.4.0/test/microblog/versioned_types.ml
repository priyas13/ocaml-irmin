open Printf
open Msigs
open Lwt.Infix
module Id = Microblog.Id

type id = Id.t

let id = Id.t

let counter_merge = Microblog.counter_merge

let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."

module Serialization(S:sig 
                        type t 
                        val t: t Irmin.Type.t 
                      end) = struct 
  open S

  let pp = Irmin.Type.pp_json ~minify:false t

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    let res = try Irmin.Type.decode_json t decoder 
              with Invalid_argument s -> 
                (failwith @@ sprintf "AO_Value.of_string:\
                  \ Invalid_argument: %s" s) in
    let _ = match res with
      | Ok _ -> ()
      | Error (`Msg str) -> 
        (printf "Decoding error: %s\n" str;
        printf "While decoding: %s\n" s) in 
    res
end

module MakeVersionedDS(Config:CONFIG)
                      (OM:MERGEABLE)
                      (AO_value:Irmin.Contents.Conv with type t = OM.t) =
struct
  module S = Irmin_git.AO(Git_unix.FS)(AO_value)

  include AO_value

  type adt = OM.t

  let create config =
    let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
        "level" Irmin.Private.Conf.(some int) None
    in
    let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
    let level = Irmin.Private.Conf.get config level in
    Git_unix.FS.create ?root ?level ()

  (* Creates a Git backend *)
  let create () = create @@ Irmin_git.config Config.root

  module type MY_TREE = TAG_TREE with type value=AO_value.t

  let add_and_link: type a. (module MY_TREE with type t=a) 
                    -> S.t -> AO_value.t -> a -> (K.t*a) Lwt.t =
    fun (module T) t v tree ->
      (S.add t v) >>= fun k ->
      let tag = T.tag_of_hash k in
      T.add tree tag v >>= fun tree' ->
          Lwt.return (k,tree')
     
  let of_adt : type a. (module MY_TREE with type t=a) ->
                    adt -> a -> (t*a) Lwt.t = 
    fun (module T) adt tr ->
      begin
        create () >>= fun ao_store ->
        add_and_link (module T) 
                ao_store adt tr >>= fun (_,tr') ->
        Lwt.return (adt,tr')
      end

  let to_adt (t:t) : adt Lwt.t =
    Lwt.return t

  let merge ~(old:t Irmin.Merge.promise) (v1:t) (v2:t) =
    if v1=v2 then Irmin.Merge.ok v1
    else begin 
      let open Irmin.Merge.Infix in
      old () >>=* fun old ->
      let old = from_just old "merge" in
      let v = OM.merge ~ancestor:old v1 v2 in
      Irmin.Merge.ok v
    end
      
  let merge = Irmin.Merge.(option (v t merge))

end

module Make(Config:CONFIG) = 
struct
  
  module ITweet : IRMIN_DATA_STRUCTURE 
    with type adt = Microblog.Tweet.t = 
  struct

    module OM = Microblog.Tweet 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Microblog.Tweet in
        record "t" (fun tweet_id author_name content -> {tweet_id; author_name; content})
        |+ field "tweet_id" id (fun t -> t.tweet_id)
        |+ field "author_name" string (fun t -> t.author_name)
        |+ field "content" string (fun t -> t.content)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

  module ITimeline : IRMIN_DATA_STRUCTURE 
    with type adt = Microblog.Timeline.t = 
  struct

    module OM = Microblog.Timeline

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Microblog.Timeline in
        record "t" (fun user_timeline_id user_tweeted_id tweet_content -> {user_timeline_id; user_tweeted_id; tweet_content})
        |+ field "user_timeline_id" id (fun t -> t.user_timeline_id)
        |+ field "user_tweeted_id" id (fun t -> t.user_tweeted_id)
        |+ field "tweet_content" string (fun t -> t.tweet_content)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

    module IUser : IRMIN_DATA_STRUCTURE 
    with type adt = Microblog.User.t = 
  struct

    module OM = Microblog.User 

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Microblog.User in
        record "t" (fun user_id user_name num_followers num_following -> 
                      {user_id; user_name; num_followers; num_following})
        |+ field "user_id" id (fun t -> t.user_id)
        |+ field "user_name" string (fun t -> t.user_name)
        |+ field "num_followers" int64 (fun t -> t.num_followers)
        |+ field "num_following" int64 (fun t -> t.num_following)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

      module IFollower : IRMIN_DATA_STRUCTURE 
    with type adt = Microblog.Follower.t = 
  struct

    module OM = Microblog.Follower

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Microblog.Follower in
        record "t" (fun u_f_id follower_id -> 
                      {u_f_id; follower_id})
        |+ field "u_f_id" id (fun t -> t.u_f_id)
        |+ field "follower_id" id (fun t -> t.follower_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

        module IFollowing : IRMIN_DATA_STRUCTURE 
    with type adt = Microblog.Following.t = 
  struct

    module OM = Microblog.Following

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t =
        let open Irmin.Type in 
        let open Microblog.Following in
        record "t" (fun u_fl_id following_id -> 
                      {u_fl_id; following_id})
        |+ field "u_fl_id" id (fun t -> t.u_fl_id)
        |+ field "following_id" id (fun t -> t.following_id)
        |> sealr

      include Serialization(struct 
                              type t = adt
                              let t = t 
                            end)
    end

    include MakeVersionedDS(Config)(OM)(AO_value)
  end

end 