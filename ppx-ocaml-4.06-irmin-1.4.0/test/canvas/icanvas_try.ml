   module Make  = struct 




  (* pixel is a record type which consist of parameters r, g and b where all of them are of char type *)
  type pixel = {r:char; g:char; b:char} [@@derive ezjsonm]

  (* default_pixel is a variable which represents the default pixel value *)
  (*let default_pixel = {r=Char.chr 255; g=Char.chr 255; b=Char.chr
   * 255}*)
  let default_pixel = {r=Char.chr 1; g=Char.chr 1; b=Char.chr 1}   

  (* type t is defines as follows which consist of two constructors
  N is a pixel which represents the leaf node 
  B is a tree of quadrants where each part is of type t *)
  type node = {tl_t: t; tr_t: t; 
            bl_t: t; br_t: t} and 
  t = 
    | N of pixel 
    | B of node (* 4 quadrants *)[@@derive versioned]

      (* loc represents the location in a canvas which is basically a record type with two entries *)
  type loc = {x:int; y:int}[@@derive ezjsonm]

  (* canvas is a record type  -----*)
  type canvas = {max_x:int; max_y:int; t:t}

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

(* main is a fucntion which calls several other functions like 
c is the variable which defines the canvas which consist of paramter 128 and 128, these both are the maximum x and y coordinates 
loc is the variable which stores two cordinates for x and y
c' is a variable that stores the return value of the function set_px, as we know set_px sets the location loc in the canvas c with the pixel 
then function _ prints c' 
px is a variable that stores the return value of the function get_px, as we know the get_px gets the pixel at the location loc in the canvas c'
and so on the rest of the functions defined in main *)
(*let main () =
  let c = new_canvas 128 128 in
  let loc = {x=93; y=127} in
  let c' = set_px c loc @@ rgb @@ Char.chr 23 in
  let _ = print c' in
  let px = get_px c' loc  in
  let _ = Printf.printf "px(%d,%d)=(%d,%d,%d)\n" loc.x loc.y
          (Char.code px.r) (Char.code px.g) (Char.code px.b) in
  let loc' = {x=45; y=78} in
  let px' = get_px c loc' in
  let _ = Printf.printf "px(%d,%d)=(%d,%d,%d)\n" loc'.x loc'.y
          (Char.code px'.r) (Char.code px'.g) (Char.code px'.b) in
  let c' = set_px c {x=98;y=17} @@ rgb @@ Char.chr 23 in
  let _ = print c' in
    ();;*)

(* main ();; *)
 module Icanvas = struct 
    open Lwt.Infix
    open Irmin_unix
    module OM = Canvas.Make
    module K = Irmin.Hash.SHA1
    module G = Git_unix.FS
    module type Config  =
      sig val root : string val shared : string val init : unit -> unit end
    let from_just op msg =
      match op with
      | Some x -> x
      | None -> failwith @@ (msg ^ ": Expected Some. Got None.")
    module MakeVersioned(Config:Config) =
      struct
        type node = {
          tl_t: K.t ;
          tr_t: K.t ;
          bl_t: K.t ;
          br_t: K.t }
        and madt =
          | B of node 
          | N of pixel 
        module IrminConvert =
          struct
            let pixel =
              let open Irmin.Type in
                ((((record "pixel" (fun r -> fun g -> fun b -> { r; g; b }))
                     |+ (field "r" Irmin.Type.char (fun t -> t.r)))
                    |+ (field "g" Irmin.Type.char (fun t -> t.g)))
                   |+ (field "b" Irmin.Type.char (fun t -> t.b)))
                  |> sealr
            let mknode t =
              let open Irmin.Type in
                (((((record "node"
                       (fun tl_t ->
                          fun tr_t ->
                            fun bl_t ->
                              fun br_t -> { tl_t; tr_t; bl_t; br_t }))
                      |+ (field "tl_t" K.t (fun t -> t.tl_t)))
                     |+ (field "tr_t" K.t (fun t -> t.tr_t)))
                    |+ (field "bl_t" K.t (fun t -> t.bl_t)))
                   |+ (field "br_t" K.t (fun t -> t.br_t)))
                  |> sealr
            and mkmadt node =
              let open Irmin.Type in
                (((variant "madt"
                     (fun b ->
                        fun n -> function | B a0 -> b a0 | N a0 -> n a0))
                    |~ (case1 "B" node (fun x -> B x)))
                   |~ (case1 "N" pixel (fun x -> N x)))
                  |> sealv
          end
        module IrminConvertTie =
          struct
            let () = ()
            let () = ()
            and (node, madt) =
              let open Irmin.Type in
                mu2
                  (fun node ->
                     fun madt ->
                       ((IrminConvert.mknode madt),
                         (IrminConvert.mkmadt node)))
          end
        module AO_value =
          (struct
             type t = madt
             let t = IrminConvertTie.madt
             let pp = Irmin.Type.pp_json ~minify:false t
             let of_string s =
               let decoder = Jsonm.decoder (`String s) in
               let res =
                 try Irmin.Type.decode_json t decoder
                 with
                 | Invalid_argument s ->
                     failwith @@
                       (Printf.sprintf
                          "AO_Value.of_string: Invalid_argument: %s" s) in
               res
           end : (Irmin.Contents.Conv with type  t =  madt))
        module AO_store =
          struct
            module S = ((Irmin_git.AO)(Git_unix.FS))(AO_value)
            include S
            let create config =
              let level =
                Irmin.Private.Conf.key ~doc:"The Zlib compression level."
                  "level" (let open Irmin.Private.Conf in some int) None in
              let root =
                Irmin.Private.Conf.get config Irmin.Private.Conf.root in
              let level = Irmin.Private.Conf.get config level in
              G.create ?root ?level ()
            let create () = create @@ (Irmin_git.config Config.root)
            let on_add = ref (fun k -> fun v -> Lwt.return ())
            let add t v =
              (S.add t v) >>=
                (fun k -> ((!on_add) k v) >>= (fun _ -> Lwt.return k))
       let rec add_adt t (a : OM.t) =
              (add t) =<<
                 (match a with
                  | OM.B a0 ->
                      (match a0 with
                       | { tl_t; tr_t; bl_t; br_t;_} ->
                           (add_adt t tl_t) >>=
                             ((fun tl_t' ->
                                 (add_adt t tr_t) >>=
                                   (fun tr_t' ->
                                      (add_adt t bl_t) >>=
                                        (fun bl_t' ->
                                           (add_adt t br_t) >>=
                                             (fun br_t' ->
                                                Lwt.return @@
                                                  {
                                                    tl_t = tl_t';
                                                    tr_t = tr_t';
                                                    bl_t = bl_t';
                                                    br_t = br_t'
                                                  }))))))
                        >>= ((fun a0' -> Lwt.return @@ (B a0')))
                  | OM.N a0 -> Lwt.return @@ (N a0)) 

  let rec read_adt t (k:K.t) : OM.t Lwt.t =
    find t k >>= fun aop ->
    let a = from_just aop "to_adt" in
    match a with
      | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
      | B {tl_t;tr_t;bl_t;br_t} ->
        (read_adt t tl_t >>= fun tl_t' ->
         read_adt t tr_t >>= fun tr_t' ->
         read_adt t bl_t >>= fun bl_t' ->
         read_adt t br_t >>= fun br_t' ->
         Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                             OM.bl_t=bl_t'; OM.br_t=br_t'})
          end
        module type IRMIN_STORE_VALUE  =
          sig
            include Irmin.Contents.S
            val of_adt : OM.t -> t Lwt.t
            val to_adt : t -> OM.t Lwt.t
          end
        module BC_value =
          (struct
             include AO_value
             let of_adt (a:OM.t) : t Lwt.t  =
      AO_store.create () >>= fun ao_store -> 
      let aostore_add adt =
        AO_store.add_adt ao_store adt in
      match a with
       | OM.N {r;g;b} -> Lwt.return @@ N {r;g;b}
       | OM.B {tl_t;tr_t;bl_t;br_t} -> 
         (aostore_add tl_t >>= fun tl_t' ->
          aostore_add tr_t >>= fun tr_t' ->
          aostore_add bl_t >>= fun bl_t' ->
          aostore_add br_t >>= fun br_t' ->
          Lwt.return @@ B {tl_t=tl_t'; tr_t=tr_t'; 
                           bl_t=bl_t'; br_t=br_t'})

    let to_adt (t:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      let aostore_read k =
        AO_store.read_adt ao_store k in
      match t with
        | N {r;g;b} -> Lwt.return @@ OM.N {r;g;b}
        | B {tl_t;tr_t;bl_t;br_t} ->
          (aostore_read tl_t >>= fun tl_t' ->
           aostore_read tr_t >>= fun tr_t' ->
           aostore_read bl_t >>= fun bl_t' ->
           aostore_read br_t >>= fun br_t' ->
           Lwt.return @@ OM.B {OM.tl_t=tl_t'; OM.tr_t=tr_t'; 
                               OM.bl_t=bl_t'; OM.br_t=br_t'})

             let rec merge ~old:(old : t Irmin.Merge.promise)  (v1 : t)
               (v2 : t) =
               let open Irmin.Merge.Infix in
                 (old ()) >>=*
                   (fun old ->
                      (to_adt (from_just old "merge")) >>=
                        (fun oldv ->
                           (to_adt v1) >>=
                             (fun v1 ->
                                (to_adt v2) >>=
                                  (fun v2 ->
                                     let v = OM.merge oldv v1 v2 in
                                     (of_adt v) >>=
                                       (fun merged_v ->
                                          Irmin.Merge.ok merged_v)))))
             let merge = let open Irmin.Merge in option (v t merge)
           end : (IRMIN_STORE_VALUE with type  t =  madt))
        module BC_store =
          struct
            module Store = (Irmin_unix.Git.FS.KV)(BC_value)
            module Sync = (Irmin.Sync)(Store)
            type t = Store.t
            type path = string list
            let init ?root  ?bare  () =
              let config = Irmin_git.config Config.root in
              Store.Repo.v config
            let master (repo : Store.repo) = Store.master repo
            let clone t name = Store.clone t name
            let get_branch r ~branch_name  = Store.of_branch r branch_name
            let merge s ~into  = Store.merge s ~into
            let read t (p : path) = Store.find t p
            let string_of_path p = String.concat "/" p
            let info s = Irmin_unix.info "[repo %s] %s" Config.root s
            let rec update ?msg  t (p : path) (v : BC_value.t) =
              let msg =
                match msg with
                | Some s -> s
                | None -> "Setting " ^ (string_of_path p) in
              let fname_of_hash hsh =
                String.sub (Fmt.to_to_string Irmin.Hash.SHA1.pp hsh) 0 7 in
              let link_to_tree k =
                (AO_store.create ()) >>=
                  (fun ao_store ->
                     (AO_store.find ao_store k) >>=
                       (fun vop ->
                          let v_k = from_just vop "BC_store.update" in
                          let path_k = [fname_of_hash k] in
                          update t path_k v_k)) in
              (match v with
               | B a0 ->
                   (match a0 with
                    | { tl_t; tr_t; bl_t; br_t;_} ->
                        List.fold_left
                          (fun m -> fun k -> m >>= (fun () -> link_to_tree k))
                          (Lwt.return()) [tl_t; tr_t; bl_t; br_t])
                     >>= ((fun a0' -> Lwt.return()))
               | N a0 -> Lwt.return()) >>=
                (fun () -> Store.set t p v ~info:(info msg))
          end
        module Vpst :
          sig
            type 'a t
            val return : 'a -> 'a t
            val bind : 'a t -> ('a -> 'b t) -> 'b t
            val with_init_version_do : OM.t -> 'a t -> 'a
            val with_remote_version_do : string -> 'a t -> 'a
            val fork_version : 'a t -> unit t
            val get_latest_version : unit -> OM.t t
            val sync_next_version : ?v:OM.t -> OM.t t
            val liftLwt : 'a Lwt.t -> 'a t
            val pull_remote : string -> unit t
          end =
          struct
            type store = BC_store.t
            type st =
              {
              master: store ;
              local: store ;
              name: string ;
              next_id: int }
            type 'a t = st -> ('a * st) Lwt.t
            let info s = Irmin_unix.info "[repo %s] %s" Config.root s
            let path = ["state"]
            let return (x : 'a) = (fun st -> Lwt.return (x, st) : 'a t)
            let bind (m1 : 'a t) (f : 'a -> 'b t) =
              (fun st -> (m1 st) >>= (fun (a, st') -> f a st') : 'b t)
            let with_init_version_do (v : OM.t) (m : 'a t) =
              Lwt_main.run
                ((BC_store.init ()) >>=
                   (fun repo ->
                      (BC_store.master repo) >>=
                        (fun m_br ->
                           (BC_value.of_adt v) >>=
                             (fun (v' : BC_value.t) ->
                                (BC_store.update ~msg:"initial version" m_br
                                   path v')
                                  >>=
                                  (fun () ->
                                     (BC_store.clone m_br "1_local") >>=
                                       (fun t_br ->
                                          let st =
                                            {
                                              master = m_br;
                                              local = t_br;
                                              name = "1";
                                              next_id = 1
                                            } in
                                          (m st) >>=
                                            (fun (a, _) -> Lwt.return a)))))))
            let fork_version (m : 'a t) =
              (fun (st : st) ->
                 let thread_f () =
                   let child_name =
                     st.name ^ ("_" ^ (string_of_int st.next_id)) in
                   let parent_m_br = st.master in
                   let m_br = parent_m_br in
                   (BC_store.clone m_br (child_name ^ "_local")) >>=
                     (fun t_br ->
                        let new_st =
                          {
                            master = m_br;
                            local = t_br;
                            name = child_name;
                            next_id = 1
                          } in
                        m new_st) in
                 Lwt.async thread_f;
                 Lwt.return ((), { st with next_id = (st.next_id + 1) }) : 
              unit t)
            let get_latest_version () =
              fun (st : st) ->
                 (BC_store.read st.local path) >>=
                   (fun (vop : BC_value.t option) ->
                      let v = from_just vop "get_latest_version" in
                      (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) 
            let pull_remote remote_uri (st : st) =
              let cinfo =
                info
                  (Printf.sprintf "Merging remote(%s) to local master"
                     remote_uri) in
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote (`Merge cinfo)) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
            let with_remote_version_do remote_uri m =
              Lwt_main.run
                ((BC_store.init ()) >>=
                   (fun repo ->
                      (BC_store.master repo) >>=
                        (fun m_br ->
                           let remote = Irmin.remote_uri remote_uri in
                           (BC_store.Sync.pull m_br remote `Set) >>=
                             (fun res ->
                                (match res with
                                 | Ok _ -> Lwt.return ()
                                 | Error _ ->
                                     failwith
                                       "Error while pulling the remote")
                                  >>=
                                  (fun _ ->
                                     (BC_store.clone m_br "1_local") >>=
                                       (fun t_br ->
                                          let st =
                                            {
                                              master = m_br;
                                              local = t_br;
                                              name = "1";
                                              next_id = 1
                                            } in
                                          (m st) >>=
                                            (fun (a, _) -> Lwt.return a)))))))
            let sync_next_version ?v  =
              (fun (st : st) ->
                 (match v with
                  | None -> Lwt.return ()
                  | Some v ->
                      (BC_value.of_adt v) >>=
                        ((fun v' ->
                            BC_store.update ~msg:"Committing local state"
                              st.local path v')))
                   >>=
                   (fun () ->
                      let cinfo = info "Merging master into local" in
                      (BC_store.merge st.master ~into:(st.local) ~info:cinfo)
                        >>=
                        (fun _ ->
                           let cinfo = info "Merging local into master" in
                           (BC_store.merge st.local ~into:(st.master)
                              ~info:cinfo)
                             >>= (fun _ -> get_latest_version () st))) : 
              OM.t t)
            let liftLwt (m : 'a Lwt.t) =
              (fun st -> m >>= (fun a -> Lwt.return (a, st)) : 'a t)
          end 
      end
    end 