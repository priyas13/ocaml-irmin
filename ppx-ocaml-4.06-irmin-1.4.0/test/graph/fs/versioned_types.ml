open Printf
open Msigs
open Lwt.Infix


let from_just op msg = match op with
  | Some x -> x
  | None -> failwith @@ msg^": Expected Some. Got None."



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
      let v = OM.merge old v1 v2 in
      Irmin.Merge.ok v
    end
      
  let merge = Irmin.Merge.(option (v t merge))

end

module Make(Config:CONFIG) = 
struct
  
  module IIN_EDGE : IRMIN_DATA_STRUCTURE 
    with type adt = Graph_imp.Edge_set.t = 
  struct

    module OM = Graph_imp.Edge_set 
open OM 
module ISet = Iset_imp.MakeVersioned(Config)(Graph_imp.Edge_type)
open ISet

    module AO_value : Irmin.Contents.Conv with type t = OM.t = 
    struct
      type adt = OM.t
      type t = OM.t
       
      let t = let open Irmin.Type in 
              let open Graph_imp.Edge_set in 
              ISet.AO_value.t

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

    include MakeVersionedDS(Config)(OM)(AO_value)
  end
end