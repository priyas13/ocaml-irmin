module Make =
  struct
    type pixel = {
      r: char ;
      g: char ;
      b: char }[@@derive ezjsonm]
    let default_pixel =
      { r = (Char.chr 255); g = (Char.chr 255); b = (Char.chr 255) }
    type node = {
      tl_t: t ;
      tr_t: t ;
      bl_t: t ;
      br_t: t }
    and t =
      | N of pixel 
      | B of node [@@derive versioned]
    type canvas = {
      max_x: int ;
      max_y: int ;
      t: t }
    type loc = {
      x: int ;
      y: int }
    let blank = N default_pixel
    let plain px = N px
    let new_canvas max_x max_y = { max_x; max_y; t = blank }
    let rec set_px canvas loc px =
      if (canvas.max_x <= loc.x) && (canvas.max_y <= loc.y)
      then N px
      else
        (let mid_x = canvas.max_x / 2 in
         let mid_y = canvas.max_y / 2 in
         match ((loc.x <= mid_x), (loc.y <= mid_y)) with
         | (true, true) ->
             let tl_t =
               match canvas.t with | N px -> N px | B { tl_t } -> tl_t in
             let tl_c = { max_x = mid_x; max_y = mid_y; t = tl_t } in
             let tl_t' = set_px tl_c loc px in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = tl_t';
                       tr_t = (N px);
                       bl_t = (N px);
                       br_t = (N px)
                     }
               | B y -> B { y with tl_t = tl_t' } in
             t'
         | (false, true) ->
             let tr_t =
               match canvas.t with | N px -> N px | B { tr_t } -> tr_t in
             let tr_c =
               { max_x = (canvas.max_x - mid_x); max_y = mid_y; t = tr_t } in
             let loc' = { loc with x = (loc.x - mid_x) } in
             let tr_t' = set_px tr_c loc' px in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = tr_t';
                       bl_t = (N px);
                       br_t = (N px)
                     }
               | B y -> B { y with tr_t = tr_t' } in
             t'
         | (true, false) ->
             let bl_t =
               match canvas.t with | N px -> N px | B { bl_t } -> bl_t in
             let bl_c =
               { max_x = mid_x; max_y = (canvas.max_y - mid_y); t = bl_t } in
             let loc' = { loc with y = (loc.y - mid_y) } in
             let bl_t' = set_px bl_c loc' px in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = (N px);
                       bl_t = bl_t';
                       br_t = (N px)
                     }
               | B y -> B { y with bl_t = bl_t' } in
             t'
         | (false, false) ->
             let br_t =
               match canvas.t with | N px -> N px | B { br_t } -> br_t in
             let br_c =
               {
                 max_x = (canvas.max_x - mid_x);
                 max_y = (canvas.max_y - mid_y);
                 t = br_t
               } in
             let loc' = { x = (loc.x - mid_x); y = (loc.y - mid_y) } in
             let br_t' = set_px br_c loc' px in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = (N px);
                       bl_t = (N px);
                       br_t = br_t'
                     }
               | B y -> B { y with br_t = br_t' } in
             t')
    let set_px canvas loc px =
      if (loc.x > canvas.max_x) || (loc.y > canvas.max_y)
      then failwith "set_px: location out of canvas bounds"
      else (let t' = set_px canvas loc px in { canvas with t = t' })
    let rec get_px canvas loc =
      match canvas.t with
      | N px -> px
      | B y ->
          let mid_x = canvas.max_x / 2 in
          let mid_y = canvas.max_y / 2 in
          (match ((loc.x <= mid_x), (loc.y <= mid_y)) with
           | (true, true) ->
               let tl_t =
                 match canvas.t with | N px -> N px | B { tl_t } -> tl_t in
               let tl_c = { max_x = mid_x; max_y = mid_y; t = tl_t } in
               get_px tl_c loc
           | (false, true) ->
               let tr_t =
                 match canvas.t with | N px -> N px | B { tr_t } -> tr_t in
               let tr_c =
                 { max_x = (canvas.max_x - mid_x); max_y = mid_y; t = tr_t } in
               let loc' = { loc with x = (loc.x - mid_x) } in
               get_px tr_c loc'
           | (true, false) ->
               let bl_t =
                 match canvas.t with | N px -> N px | B { bl_t } -> bl_t in
               let bl_c =
                 { max_x = mid_x; max_y = (canvas.max_y - mid_y); t = bl_t } in
               let loc' = { loc with y = (loc.y - mid_y) } in
               get_px bl_c loc'
           | (false, false) ->
               let br_t =
                 match canvas.t with | N px -> N px | B { br_t } -> br_t in
               let br_c =
                 {
                   max_x = (canvas.max_x - mid_x);
                   max_y = (canvas.max_y - mid_y);
                   t = br_t
                 } in
               let loc' = { x = (loc.x - mid_x); y = (loc.y - mid_y) } in
               get_px br_c loc')
    let color_mix px1 px2 =
      (let f = Char.code in
       let h x y = Char.chr @@ ((x + y) / 2) in
       let (r1, g1, b1) = ((f px1.r), (f px1.g), (f px1.b)) in
       let (r2, g2, b2) = ((f px2.r), (f px2.g), (f px2.b)) in
       let (r, g, b) = ((h r1 r2), (h g1 g2), (h b1 b2)) in { r; g; b } : 
      pixel)
    let b_of_n px =
      B { tl_t = (N px); tr_t = (N px); bl_t = (N px); br_t = (N px) }
    let make_b (tl, tr, bl, br) =
      B { tl_t = tl; tr_t = tr; bl_t = bl; br_t = br }
    let rgb px = { r = px; g = px; b = px }
    let rec merge old v1 v2 =
      if v1 = v2
      then v1
      else
        if v1 = old
        then v2
        else
          if v2 = old
          then v1
          else
            (match (old, v1, v2) with
             | (_, B _, N px2) -> (merge old v1) @@ (b_of_n px2)
             | (_, N px1, B _) -> merge old (b_of_n px1) v2
             | (N px, B _, B _) -> merge (b_of_n px) v1 v2
             | (B x, B x1, B x2) ->
                 let tl_t' = merge x.tl_t x1.tl_t x2.tl_t in
                 let tr_t' = merge x.tr_t x1.tr_t x2.tr_t in
                 let bl_t' = merge x.bl_t x1.bl_t x2.bl_t in
                 let br_t' = merge x.br_t x1.br_t x2.br_t in
                 B { tl_t = tl_t'; tr_t = tr_t'; bl_t = bl_t'; br_t = br_t' }
             | (_, N px1, N px2) -> let px' = color_mix px1 px2 in N px')
    let rec print min_x min_y max_x max_y t =
      if (min_x > max_x) || (min_y > max_y)
      then ()
      else
        (match t with
         | N px when not (px = default_pixel) ->
             if (min_x = max_x) && (min_y = max_y)
             then
               Printf.printf "<%d,%d>: (%d,%d,%d)\n" min_x min_y
                 (Char.code px.r) (Char.code px.g) (Char.code px.b)
             else
               Printf.printf "<%d,%d> to <%d,%d>: (%d,%d,%d)\n" min_x min_y
                 max_x max_y (Char.code px.r) (Char.code px.g)
                 (Char.code px.b)
         | N px -> ()
         | B { tl_t; tr_t; bl_t; br_t } ->
             let (mid_x, mid_y) =
               ((min_x + (((max_x - min_x) + 1) / 2)),
                 (min_y + (((max_y - min_y) + 1) / 2))) in
             (print min_x min_y mid_x mid_y tl_t;
              print (mid_x + 1) min_y max_x mid_y tr_t;
              print min_x (mid_y + 1) mid_x max_y bl_t;
              print (mid_x + 1) (mid_y + 1) max_x max_y br_t))
    let print { max_x; max_y; t } = print 0 0 max_x max_y t
    let print c =
      for x = 1 to c.max_x do
        for y = 1 to c.max_y do
          let px = get_px c { x; y } in
          if not (px = default_pixel)
          then
            Printf.printf "<%d,%d>: (%d,%d,%d)\n" x y (Char.code px.r)
              (Char.code px.g) (Char.code px.b)
          else ()
        done
      done
  end[@@derive_versioned ]
module IMake =
  struct
    open Lwt.Infix
    open Irmin_unix
    module OM = Make
    open OM
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
          | N of pixel 
          | B of node 
        module AO_value =
          (struct
             type madt = t
             type t = madt
             [%%dali_irmin_convert ]
             let pp = Irmin.Type.pp_json ~minify:false t
             let of_string s =
               let decoder = Jsonm.decoder (`String s) in
               let res =
                 try Irmin.Type.decode_json t decoder
                 with
                 | Invalid_argument s ->
                     failwith @@
                       (sprintf "AO_Value.of_string: Invalid_argument: %s" s) in
               res
           end : (Irmin.Contents.Conv with type  t =  t))
        module AO_store =
          struct
            module S = (((Irmin_git.AO)(G))(K))(AO_value)
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
            let rec add_adt t (a : Make.t) =
              ((add t) ==<<
                 (match a with
                  | Make.N a0 -> Lwt.return @@ (N a0)
                  | Make.B a0 ->
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
                        >>= ((fun a0' -> Lwt.return @@ (B a0')))) : K.t Lwt.t)
            let rec read_adt t (k : K.t) =
              ((find t k) >>=
                 (fun aop ->
                    let a = from_just aop "to_adt" in
                    match a with
                    | N a0 -> Lwt.return @@ (Make.N a0)
                    | B a0 ->
                        (match a0 with
                         | { tl_t; tr_t; bl_t; br_t;_} ->
                             (read_adt t tl_t) >>=
                               ((fun tl_t' ->
                                   (read_adt t tr_t) >>=
                                     (fun tr_t' ->
                                        (read_adt t bl_t) >>=
                                          (fun bl_t' ->
                                             (read_adt t br_t) >>=
                                               (fun br_t' ->
                                                  Lwt.return @@
                                                    {
                                                      Make.tl_t = tl_t';
                                                      Make.tr_t = tr_t';
                                                      Make.bl_t = bl_t';
                                                      Make.br_t = br_t'
                                                    }))))))
                          >>= ((fun a0' -> Lwt.return @@ (Make.B a0')))) : 
              Make.t Lwt.t)
          end
        module type IRMIN_STORE_VALUE_MAKE  =
          sig
            include Irmin.Contents.S
            val of_adt : Make.t -> t Lwt.t
            val to_adt : t -> Make.t Lwt.t
          end
        module BC_value =
          (struct
             include AO_value
             let of_adt (a : OM.t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_add adt = AO_store.add_adt ao_store adt in
                     match a with
                     | Make.N a0 -> Lwt.return @@ (N a0)
                     | Make.B a0 ->
                         (match a0 with
                          | { tl_t; tr_t; bl_t; br_t;_} ->
                              (aostore_adt tl_t) >>=
                                ((fun tl_t' ->
                                    (aostore_adt tr_t) >>=
                                      (fun tr_t' ->
                                         (aostore_adt bl_t) >>=
                                           (fun bl_t' ->
                                              (aostore_adt br_t) >>=
                                                (fun br_t' ->
                                                   Lwt.return @@
                                                     {
                                                       tl_t = tl_t';
                                                       tr_t = tr_t';
                                                       bl_t = bl_t';
                                                       br_t = br_t'
                                                     }))))))
                           >>= ((fun a0' -> Lwt.return @@ (B a0')))) : 
               t Lwt.t)
             let to_adt (t : t) =
               ((AO_store.create ()) >>=
                  (fun ao_store ->
                     let aostore_read k = AO_store.read_adt ao_store k in
                     match t with
                     | N a0 -> Lwt.return @@ (Make.N a0)
                     | B a0 ->
                         (match a0 with
                          | { tl_t; tr_t; bl_t; br_t;_} ->
                              (aostore_read tl_t) >>=
                                ((fun tl_t' ->
                                    (aostore_read tr_t) >>=
                                      (fun tr_t' ->
                                         (aostore_read bl_t) >>=
                                           (fun bl_t' ->
                                              (aostore_read br_t) >>=
                                                (fun br_t' ->
                                                   Lwt.return @@
                                                     {
                                                       Make.tl_t = tl_t';
                                                       Make.tr_t = tr_t';
                                                       Make.bl_t = bl_t';
                                                       Make.br_t = br_t'
                                                     }))))))
                           >>= ((fun a0' -> Lwt.return @@ (Make.B a0')))) : 
               OM.t Lwt.t)
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
           end : (IRMIN_STORE_VALUE_MAKE with type  t =  t))
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
          end
        module Vpst :
          sig
            type 'a t
            val return : 'a -> 'a t
            val bind : 'a t -> ('a -> 'b t) -> 'b t
            val with_init_version_do : Make.t -> 'a t -> 'a
            val fork_version : 'a t -> unit t
            val get_latest_version : unit -> Make.t t
            val sync_next_version : ?v:Make.t -> Make.t t
            val liftLwt : 'a Lwt.t -> 'a t
            val pull_remote : string -> unit t
            val fork_remote : string -> unit t
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
              (fun (st : st) ->
                 (BC_store.read st.local path) >>=
                   (fun (vop : BC_value.t option) ->
                      let v = from_just vop "get_latest_version" in
                      (BC_value.to_adt v) >>= (fun td -> Lwt.return (td, st))) : 
              OM.t t)
            let pull_remote remote_uri (st : st) =
              let cinfo =
                info
                  (sprintf "Merging remote(%s) to local master" remote_uri) in
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote (`Merge cinfo)) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
            let fork_remote remote_uri (st : st) =
              let remote = Irmin.remote_uri remote_uri in
              (BC_store.Sync.pull st.master remote `Set) >>=
                (fun res ->
                   match res with
                   | Ok _ -> Lwt.return ((), st)
                   | Error _ -> failwith "Error while pulling the remote")
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
