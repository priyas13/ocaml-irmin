(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type ATOM = sig
  type t
  val t: t Irmin.Type.t
  val to_string : t -> string   
  val of_string: string -> t
  include Msigs.RESOLVEABLE with type t := t
end

module List (Atom: ATOM)  =
struct
    type atom = Atom.t[@@derive ezjsonm]
    type t = atom list[@@derive versioned]

    let empty = []

    let length (x:t) = List.length x 

    let split_at l i =
      if List.length l > i then
        let rec aux xs i acc =
          if i >= 0 then aux (List.tl xs) (i - 1) ((List.hd xs) :: acc)
          else acc, xs
        in
        aux l i []
      else
        raise (Invalid_argument "out of bound index")

    let insert l i a =
      if List.length l = i then
        List.append l [a]
      else
        let rxs, ys = split_at l i in
        let ixs = match rxs with
        | [] -> [a]
        | x::[] -> [x; a]
        | x::y -> x::a::y in
        List.rev_append ixs ys

    let get l i =
      List.nth l i

    let set l i a =
      let rxs, ys = split_at l i in
      let sxs = a :: (List.tl rxs) in
      List.rev_append sxs ys

    let delete l i =
      let rxs, ys = split_at l i in
      let dxs = List.tl rxs in
      List.rev_append dxs ys
type edit =
    | Ins of int * atom
    | Del of int * atom
    | Rep of int * atom * atom

  type patch = edit list

  let edit_to_string atom_to_string = function
  | Ins (i, a) -> Printf.sprintf "Ins (%i, %s)" i (atom_to_string a)
  | Del (i, a) -> Printf.sprintf "Del (%i, %s)" i (atom_to_string a)
  | Rep (i, a, b) -> Printf.sprintf "Rep (%i, %s, %s)" i (atom_to_string a) (atom_to_string b)

  let op_diff xs ys =
    let cache = Array.init (length xs+1)
        (fun _ -> Array.make (length ys+1) None)
    in
    let rec loop i j =
      let cache_i = Array.unsafe_get cache i in
      let min3 x y z =
        let m' (a,al) (b,bl) = if a < b then (a,al) else (b,bl) in
        m' (m' x y) z
      in
      match Array.unsafe_get cache_i j with
      | Some v -> v
      | None ->
        let res =
          begin match i,j with
            | 0,0 -> (0, [])
            | 0, j ->
              let d,e = loop 0 (j-1) in
              (d+1, (Ins (i,get ys (j-1))::e))
            | i, 0 ->
              let d,e = loop (i-1) 0 in
              (d+1, (Del(i-1,get xs (i-1))::e))
            | _ ->
              let xsim1 = get xs (i-1) in
              let ysim1 = get ys (j-1) in
              let d,e = loop (i-1) j in
              let r1 = (d+1, Del (i-1,xsim1)::e) in
              let d,e = loop i (j-1) in
              let r2 = (d+1, Ins (i,ysim1)::e) in
              let d,e = loop (i-1) (j-1) in
              let r3 =
                if xsim1 = ysim1 then d,e
                else (d+1, (Rep (i-1, xsim1, ysim1)::e))
              in
              min3 r1 r2 r3
          end
        in
        Array.unsafe_set cache_i j (Some res);
        res
    in
    let _,e = loop (length xs) (length ys) in
    List.rev e

  let index = function
    | Ins (i,_) -> i
    | Del (i,_) -> i
    | Rep (i,_,_) -> i

  let shift_edit o = function
    | Ins(i,x) -> Ins(i+o,x)
    | Del(i,x) -> Del(i+o,x)
    | Rep(i,x,x') -> Rep(i+o,x,x')

  let rec shift_patch acc o = function
    | [] -> List.rev acc
    | e::tl -> shift_patch (shift_edit o e::acc) o tl

  let offset = function
    | Ins _ -> 1
    | Del _ -> -1
    | Rep _ -> 0

  let op_transform p q =
    let cons2 (x,y) (xs,ys) = (x::xs, y::ys) in
    let rec go xs a ys b =
      match xs, a, ys, b with
      | [], _, [], _ -> ([], [])
      | xs, a, [], _ -> (shift_patch [] a xs, [])
      | [], _, ys, b -> ([], shift_patch [] b ys)
      | x::xs, a, y::ys, b ->
        if index x < index y then
          let p',q' = go xs a (y::ys) (b + offset x) in
          (shift_edit a x::p',q')
        else if index x > index y then
          let p',q' = go (x::xs) (a + offset y) ys b in
          (p',shift_edit b y::q')
        else begin
          match x,y with
          | _ when x = y -> go xs (a + offset y) ys (b + offset x)
          | Ins (i,nx), Ins (_, ny) ->
            let n = Atom.resolve nx ny in
            cons2 (Rep (i+a,ny,n), Rep (i+b,nx,n)) (go xs (a + offset y) ys (b + offset x))
          | Rep (i, anc, nx), Rep (_, _, ny) ->
            let n = Atom.merge3 ~ancestor:anc nx ny in
            cons2 (Rep (i + a, ny, n), Rep (i + b, nx, n)) (go xs a ys b)
          | Ins _, _ ->
            let p',q' = go xs a (y::ys) (b + offset x) in
            (shift_edit a x::p',q')
          | _, Ins _ ->
            let p',q' = go (x::xs) (a + offset y) ys b in
            (p', shift_edit b y::q')
          | Rep (i,_,nx), Del _ ->
            let p',q' = go xs (a + offset y) ys b in
            (p', Del (i+b, nx)::q')
          | Del _, Rep (i, _, ny) ->
            let p',q' = go xs a ys (b + offset x) in
            (Del (i+a,ny)::p',q')
          | Del _, Del _ -> go xs (a + offset y) ys (b + offset x)
        end
    in
    go p 0 q 0

  let rec apply off s = function
    | [] -> s
    | Ins(pos,c)::tl ->
      let s' = insert s (pos+off) c in
      apply (off + 1) s' tl
    | Rep(pos, _, x')::tl ->
      let s' = set s (pos + off) x' in
      apply off s' tl
    | Del(pos, _)::tl ->
      let s' = delete s (pos + off) in
      apply (off - 1) s' tl

  let apply s =
    try apply 0 s
    with Invalid_argument _ ->
      raise (Invalid_argument "incompatible patch")

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'

  end[@@derive_versioned]
  open Lwt.Infix
open Irmin_unix

(* Config module has three functions root, shared and init. *)
module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

(* MakeVersioned is a functor which takes Config and Atom as arguments *)
(* Config is defined above *)
(* Mvector_list contains the ATOM module which is passed to the functor *)
(* It also includes two other modules OM and K *)
(* OM includes the operations, types defined in the implementation if the vector list module Make *)
(* K is the module which containes functions for hashing the values and generating the key for the values stored in the Irmin store *)
(* Irmin.Hash provides user defined hash functions to digest serialized contents *)
module MakeVersioned (Config: Config) (Atom: Mvector_list.ATOM) = struct
  module OM = Mvector_list.Make(Atom)
  module K = Irmin.Hash.SHA1

  let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

  
  type vt = OM.atom list
  
  (* This module M defines the custom type for Irmin which is build using OCaml list data type *)
  (* To create a custom type we need type t, vaue t of type Irmin.TYpe.t, function pp for formatting t, of_string for 
     converting from string to t and a merger function *)
  module M = struct
    module AO_value  = struct
      
      type t = vt 

      let t = Irmin.Type.(list Atom.t)

      let pp = Irmin.Type.pp_json t

      let of_string s =
      let decoder = Jsonm.decoder (`String s) in
      Irmin.Type.decode_json (t) decoder
    end
    
    (* storage backhend: Append-only store *)
    module AO_store = struct
      (* Immutable collection of all versionedt *)
      module S = Irmin_git.AO(Git_unix.FS)(AO_value)
      include S

      let create config =
        let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
            "level" Irmin.Private.Conf.(some int) None
        in
        let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
        let level = Irmin.Private.Conf.get config level in
        Git_unix.FS.create ?root ?level ()

      (* Somehow pulls the config set by Store.init *)
      (* And creates a Git backend *)
      let create () = create @@ Irmin_git.config Config.shared
    end

    type t = K.t
     
   (* of_adt takes ocaml type and gives Irmin key *)
   (* A backend store called aostore is created using the function create *)
   (* Using the add function of the append-only store, the value is added to the store *)
   let of_adt (a:OM.t) : t Lwt.t  =
      let aostore = AO_store.create () in 
      let aostore_add value = 
          aostore >>= (fun ao_store -> AO_store.add ao_store value) in 
          aostore_add =<< Lwt.return (a)
   
   (* to_adt gives the raw value that was associated with the key k in the store *)
   let rec to_adt (k:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      AO_store.find ao_store k >>= fun t ->
      let t = from_just t in
      Lwt.return (t)
    
    (* Value t of type Irmin.Type.t *)
    let t = K.t

    let pp = K.pp

    let of_string = K.of_string
 
    (* merge function merges old, v1_k and v2_k *)
    (* Irmin.Merge.promise t is a promise containing a value of type t *)
    (* using the to_adt, old_k, v1_k and v2_k is converted to the OCaml data type *)
    (* Then the merge fucntion from the OM module is being called on oldv, v1 and v2 *)
    let rec merge ~(old:t Irmin.Merge.promise) v1_k v2_k =
      let open Irmin.Merge.Infix in
      old () >>=* fun old_k ->
      let old_k = from_just old_k in
      to_adt old_k >>= fun oldv  ->
      to_adt v1_k >>= fun v1  ->
      to_adt v2_k >>= fun v2 ->
      let v = OM.merge3 oldv v1 v2 in
      of_adt v >>= fun merged_k ->
      Irmin.Merge.ok merged_k

    let merge = Irmin.Merge.(option (v t merge))
  end

  (* Store is defined as follows which is a module *)
  (* M is the module which is passed as content to the Irmin store *)
  module BC_store = struct
    module Store = Irmin_unix.Git.FS.KV(M)
    module Sync = Irmin.Sync(Store)

    type t = Store.t

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config
    
    (* A persistent store based on master branch *)
    (* repo is the repository containing the type t *)
    let master (repo:Store.repo) = Store.master repo

    (* clones the source to the destination branch *)
    let clone t name = Store.clone t name
    
    (* Gets the store based on the branch name *)
    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    let update t k v = Store.set t k v

    let read t k = Store.find t k
  end

(* Vpst is a module which consist of type store, st and 'a t *)
  module Vpst : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val with_init_version_do: OM.t -> 'a t -> 'a
  val fork_version : 'a t -> unit t
  val get_latest_version: unit -> OM.t t
  val sync_next_version: ?v:OM.t -> OM.t t
  val liftLwt : 'a Lwt.t -> 'a t
end = struct
    (* store is a type which is basically of type BC_store.t *)
    type store = BC_store.t
    (* st is a record type with fields as master, local, name and next_id *)
    type st = {master   : store;
               local    : store;
               name     : string; 
               next_id  : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')

    let with_init_version_do (v: OM.t) (m: 'a t) =
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          M.of_adt v >>= fun k ->
          let cinfo = info "creating state of master" in
          BC_store.update m_br path k ~info:cinfo >>= fun () ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

    let with_init_forked_do (m: 'a t) = 
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun m_br ->
      BC_store.clone m_br "1_local" >>= fun t_br ->
      let st = {master=m_br; local=t_br; name="1"; next_id=1} in
      m st >>= fun (a, _) -> Lwt.return a

    let fork_version (m: 'a t) : unit t = fun (st: st) ->
      let thread_f () = 
        let child_name = st.name^"_"^(string_of_int st.next_id) in
        let parent_m_br = st.master in
        (* Ideally, the following has to happen: *)
        (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
        (* But, we currently default to an SC mode. Master is global. *)
        let m_br = parent_m_br in
        BC_store.clone m_br (child_name^"_local") >>= fun t_br ->
        let new_st = {master = m_br; local  = t_br; name = child_name; next_id = 1} in
        m new_st in
      begin
        Lwt.async thread_f;
        Lwt.return ((), {st with next_id=st.next_id+1})
      end

    let get_latest_version () : OM.t t = fun (st: st) ->
      BC_store.read st.local path >>= fun k ->
      M.to_adt @@ from_just k >>= fun td ->
      Lwt.return (td,st)

    let sync_remote_version remote_uri ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2.. Pull from remote to master *)
      let cinfo = info (Printf.sprintf "Merging remote: %s" remote_uri) in
      BC_store.Sync.pull st.master (Irmin.remote_uri remote_uri) (`Merge  cinfo) >>= fun _ ->
      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let sync_next_version ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)
end 
end









(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)