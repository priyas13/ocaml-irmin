module Make  = struct 

(* pixel is a record type which consist of parameters r, g and b where all of them are of char type *)
type pixel = {r:char; g:char; b:char}

(* default_pixel is a variable which represents the default pixel value *)
let default_pixel = {r=Char.chr 255; g=Char.chr 255; b=Char.chr 255}

(* type t is defines as follows which consist of two constructors
N is a pixel which represents the leaf node 
B is a tree of quadrants where each part is of type t *)
type node = {tl_t: t; tr_t: t; 
          bl_t: t; br_t: t} and 
t = 
  | N of pixel 
  | B of node (* 4 quadrants *)

(* canvas is a record type  -----*)
type canvas = {max_x:int; max_y:int; t:t}

(* loc represents the location in a canvas which is basically a record type with two entries *)
type loc = {x:int; y:int}

(* blank is a leaf node with default_pixel *)
let blank = N default_pixel

(* plain px is a leaf node with pixel px *)
let plain px = N px

(* new_convas takes two argument max_x and max_y and produces a canvas where the canvas is blank *)
let new_canvas max_x max_y = 
  {max_x=max_x; max_y=max_y; t=blank}

(* set_px is a function which sets the canvas at location loc with pixel px *)
(* If the max_x and max_y is less than the loc where we want to set pixel then we return a leaf node with pixel px *)
(* This is a recursive function *)
let rec set_px canvas loc px = 
  if canvas.max_x<=loc.x && canvas.max_y<=loc.y 
  then N px
  else 
    let mid_x = canvas.max_x/2 in
    let mid_y = canvas.max_y/2 in 
      match (loc.x <= mid_x, loc.y <= mid_y) with
        | (true,true) -> (* top-left quadrant *)
            let tl_t = match canvas.t with 
              | N px -> N px | B {tl_t} -> tl_t in
            let tl_c = {max_x=mid_x; max_y=mid_y; t=tl_t} in
            let tl_t' = set_px tl_c loc px in
            let t' = match canvas.t with
              | N px -> B {tl_t=tl_t'; tr_t=N px; 
                           bl_t=N px; br_t=N px}
              | B y -> B {y with tl_t=tl_t'} in
              t'
        | (false,true) -> (* top-right quadrant *)
            let tr_t = match canvas.t with 
              | N px -> N px | B {tr_t} -> tr_t in
            let tr_c = {max_x=canvas.max_x - mid_x; 
                        max_y=mid_y; t=tr_t} in
            let loc' = {loc with x=loc.x - mid_x} in
            let tr_t' = set_px tr_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=tr_t'; 
                           bl_t=N px; br_t=N px}
              | B y -> B {y with tr_t=tr_t'} in
              t'
        | (true,false) -> (* bottom-left quadrant *)
            let bl_t = match canvas.t with 
              | N px -> N px | B {bl_t} -> bl_t in
            let bl_c = {max_x=mid_x; 
                        max_y=canvas.max_y - mid_y; 
                        t=bl_t} in
            let loc' = {loc with y=loc.y - mid_y} in
            let bl_t' = set_px bl_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=N px; 
                           bl_t=bl_t'; br_t=N px}
              | B y -> B {y with bl_t=bl_t'} in
              t'
        | (false,false) -> (* bottom-right quadrant *)
            let br_t = match canvas.t with 
              | N px -> N px | B {br_t} -> br_t in
            let br_c = {max_x=canvas.max_x - mid_x;
                        max_y=canvas.max_y - mid_y; 
                        t=br_t} in
            let loc' = {x=loc.x-mid_x; y=loc.y-mid_y} in
            let br_t' = set_px br_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=N px; 
                           bl_t=N px; br_t=br_t'}
              | B y -> B {y with br_t=br_t'} in
              t'
(* This uses the recursive function defined above *)
let set_px canvas loc px = 
  if loc.x > canvas.max_x || loc.y > canvas.max_y then
    failwith "set_px: location out of canvas bounds"
  else
    let t' = set_px canvas loc px in
      {canvas with t=t'}

(* get_px is the recursive function which is defined to get the pixel at location loc in the canvas *)
let rec get_px canvas loc = match canvas.t with
  | N px -> px
  | B y -> 
    let mid_x = canvas.max_x/2 in
    let mid_y = canvas.max_y/2 in 
      match (loc.x <= mid_x, loc.y <= mid_y) with
        | (true,true) ->
            let tl_t = match canvas.t with 
              | N px -> N px | B {tl_t} -> tl_t in
            let tl_c = {max_x=mid_x; max_y=mid_y; t=tl_t} in
              get_px tl_c loc
        | (false,true) -> 
            let tr_t = match canvas.t with 
              | N px -> N px | B {tr_t} -> tr_t in
            let tr_c = {max_x=canvas.max_x - mid_x; 
                        max_y=mid_y; t=tr_t} in
            let loc' = {loc with x=loc.x - mid_x} in
              get_px tr_c loc'
        | (true,false) ->
            let bl_t = match canvas.t with 
              | N px -> N px | B {bl_t} -> bl_t in
            let bl_c = {max_x=mid_x; 
                        max_y=canvas.max_y - mid_y; 
                        t=bl_t} in
            let loc' = {loc with y=loc.y - mid_y} in
              get_px bl_c loc'
        | (false,false) -> 
            let br_t = match canvas.t with 
              | N px -> N px | B {br_t} -> br_t in
            let br_c = {max_x=canvas.max_x - mid_x;
                        max_y=canvas.max_y - mid_y; 
                        t=br_t} in
            let loc' = {x=loc.x-mid_x; y=loc.y-mid_y} in
              get_px br_c loc'

(*
 * RGB color mixing algorithm.
 *)
(* There are cases where users update the same location in that case we need to mix the pixels and resolve the conflict*)
let color_mix px1 px2 : pixel = 
  let f = Char.code in
  let h x y = Char.chr @@ (x + y)/ 2 in
  let (r1,g1,b1) = (f px1.r, f px1.g, f px1.b) in
  let (r2,g2,b2) = (f px2.r, f px2.g, f px2.b) in
  let (r,g,b) = (h r1 r2, h g1 g2, h b1 b2) in
    {r=r; g=g; b=b}

let b_of_n px = 
  B {tl_t=N px; tr_t=N px; bl_t=N px; br_t=N px}
    
let make_b (tl,tr,bl,br) = 
  B {tl_t=tl; tr_t=tr; bl_t=bl; br_t=br}

let rgb px = {r=px; g=px; b=px}

(* merge is a recursive function which takes three arguments old, v1 and v2 *)
let rec merge old v1 v2 = 
  if v1=v2 then v1
  else if v1=old then v2
  else if v2=old then v1
  else match (old,v1,v2) with
    (*
     * The first three rules isomorphize old, v1 and v2.
     *)
    | (_, B _, N px2) -> merge old v1 @@ b_of_n px2
    | (_, N px1, B _) -> merge old (b_of_n px1) v2
    | (N px, B _, B _) -> merge (b_of_n px) v1 v2
    | (B x, B x1, B x2) ->
        let tl_t' = merge x.tl_t x1.tl_t x2.tl_t in
        let tr_t' = merge x.tr_t x1.tr_t x2.tr_t in
        let bl_t' = merge x.bl_t x1.bl_t x2.bl_t in
        let br_t' = merge x.br_t x1.br_t x2.br_t in
          B {tl_t=tl_t'; tr_t=tr_t'; bl_t=bl_t'; br_t=br_t'}
    | (_, N px1, N px2) -> 
        (* pixels are merged by mixing colors *)
        let px' = color_mix px1 px2 in N px'

let rec print min_x min_y max_x max_y t = 
  if min_x > max_x || min_y > max_y then ()
  else match t with 
    | N px when not (px = default_pixel) -> 
        if min_x = max_x && min_y = max_y 
        then Printf.printf "<%d,%d>: (%d,%d,%d)\n" min_x min_y 
               (Char.code px.r) (Char.code px.g) (Char.code px.b)
        else Printf.printf "<%d,%d> to <%d,%d>: (%d,%d,%d)\n"
                min_x min_y max_x max_y (Char.code px.r)
                (Char.code px.g) (Char.code px.b) 
    | N px -> ()
    | B {tl_t; tr_t; bl_t; br_t} -> 
        let (mid_x, mid_y) = (min_x + (max_x - min_x + 1)/2, 
                              min_y + (max_y - min_y + 1)/2) in
        begin
          print min_x min_y mid_x mid_y tl_t;
          print (mid_x+1) min_y max_x mid_y tr_t;
          print min_x (mid_y+1) mid_x max_y bl_t;
          print (mid_x+1) (mid_y+1) max_x max_y br_t;
        end 

let print {max_x; max_y; t} = print 0 0 max_x max_y t

let print c = 
  for x=1 to c.max_x do
    for y=1 to c.max_y do
      let px = get_px c {x=x; y=y} in
        if not (px = default_pixel)
        then Printf.printf "<%d,%d>: (%d,%d,%d)\n" x y 
               (Char.code px.r) (Char.code px.g) (Char.code px.b)
        else ()
    done
  done

end[@@derive_versioned]
module ICanvas = 
struct
open Lwt.Infix
open Irmin_unix
(* Config module has three functions root, shared and init. *)
module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

(* MakeVersioned is a functor which takes Config and Atom as arguments *)
module MakeVersioned (Config: Config)  = struct
  module OM = Canvas.Make
  module K = Irmin.Hash.SHA1

  let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

  (* vpixel is a record type which consist of parameters r, g and b where all of them are of char type *)
  type vpixel = {r:char; g:char; b:char}

  type vnode = {tl_t:K.t; tr_t:K.t; bl_t:K.t; br_t:K.t}

  (* vt is a type *)
  type vt = 
   | N of vpixel 
   | B of vnode

  type vcanvas = {max_x:int64; max_y: int64; vt:vt}

  type vloc = {x:int64; y:int64}
  

  module M = struct
  (* AO_value represents the value that will be passed to the Irmin store *)
    module AO_value = struct
      type t = vt
    
      let vpixel = 
        let open Irmin.Type in
        record "vpixel" (fun r g b -> {r; g; b})
        |+ field "r" Irmin.Type.char (fun t -> t.r)
        |+ field "g" Irmin.Type.char (fun t -> t.g)
        |+ field "b" Irmin.Type.char (fun t -> t.b)
        |> sealr


      let vnode = 
        let open Irmin.Type in
        record "vnode" (fun tl_t tr_t bl_t br_t -> {tl_t;tr_t;bl_t;br_t})
        |+ field "tl_t" K.t (fun t -> t.tl_t)
        |+ field "tr_t" K.t (fun t -> t.tr_t)
        |+ field "bl_t" K.t (fun t -> t.bl_t)
        |+ field "br_t" K.t (fun t -> t.br_t)
        |> sealr


      let t =
        let open Irmin.Type in
        variant "t" (fun vp np -> function
            | N v -> vp v  
            | B n -> np n)
        |~ case1 "N" vpixel (fun x -> N x)
        |~ case1 "B" vnode (fun x -> B x)
        |> sealv


      let vcanvas = 
       let open Irmin.Type in 
       record "vcanvas" (fun max_x max_y vt -> {max_x; max_y; vt})
       |+ field "max_x" int64 (fun t -> t.max_x)
       |+ field "max_y" int64 (fun t -> t.max_y)
       |+ field "vt" t (fun t -> t.vt)
       |> sealr

      let vloc = 
       let open Irmin.Type in 
       record "vloc" (fun x y -> {x ; y})
       |+ field "x" int64 (fun t -> t.x)
       |+ field "y" int64 (fun t -> t.y)
       |> sealr

    let pp = Irmin.Type.pp_json t
    
     let of_string s =
        let decoder = Jsonm.decoder (`String s) in
        Irmin.Type.decode_json t decoder

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
     
    (* canvas functions *)

  let default_pixel = {r=Char.chr (255); g=Char.chr (255); b=Char.chr (255)}   
  let blank = N default_pixel

  let rec of_adt (a:OM.t) : t Lwt.t  =
      let aostore = AO_store.create () in
      let aostore_add value =
        aostore >>= (fun ao_store -> AO_store.add ao_store value) in
      aostore_add =<<
      (match a with
       | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
       | OM.B {tl_t;tr_t;bl_t;br_t} -> 
         (of_adt tl_t >>= fun tl_t' ->
          of_adt tr_t >>= fun tr_t' ->
          of_adt bl_t >>= fun bl_t' ->
          of_adt br_t >>= fun br_t' ->
          Lwt.return {tl_t=tl_t'; tr_t=tr_t'; bl_t=bl_t'; br_t=br_t'})
         >>= ((fun n -> Lwt.return @@ (B n))))

  let rec to_adt (k:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      AO_store.find ao_store k >>= fun t ->
      let t = from_just t in
      (match t with
      | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
      | B {tl_t;tr_t;bl_t;br_t} ->
        (to_adt tl_t >>= fun tl_t' ->
         to_adt tr_t >>= fun tr_t' ->
         to_adt bl_t >>= fun bl_t' ->
         to_adt br_t >>= fun br_t' ->
         Lwt.return {OM.tl_t=tl_t'; OM.tr_t=tr_t'; OM.bl_t=bl_t'; OM.br_t=br_t'})
        >>= ((fun n -> Lwt.return @@ (OM.B n))))

    let t = K.t

    let pp = K.pp

    let of_string = K.of_string
 
    (* merge function merges old, v1_k and v2_k *)
    (* Irmin.Merge.promise t is a promise containing a value of type t *)
    (* using the to_adt, old_k, v1_k and v2_k is converted to the OCaml data type *)
    let rec merge ~(old:t Irmin.Merge.promise) v1_k v2_k =
      let open Irmin.Merge.Infix in
      old () >>=* fun old_k ->
      let old_k = from_just old_k in
      to_adt old_k >>= fun oldv  ->
      to_adt v1_k >>= fun v1  ->
      to_adt v2_k >>= fun v2 ->
      let v = OM.merge oldv v1 v2 in
      of_adt v >>= fun merged_k ->
      Irmin.Merge.ok merged_k

    let merge = Irmin.Merge.(option (v t merge))
  end

  (* Store is defined as follows which is a module *)
  module BC_store = struct
    module Store = Irmin_unix.Git.FS.KV(M)
    module Sync = Irmin.Sync(Store)

    type t = Store.t

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

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
end  
