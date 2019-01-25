

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Canvas *)
let _ =
  U.print_header "Canvas";
let module MkConfig (Vars: sig val root: string end) : Icanvas.ICanvas.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Icanvas.ICanvas.MakeVersioned(CInit) in 
let module M = Canvas.Canvas in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let mk t = {MInit.OM.max_x=(128); MInit.OM.max_y=(128); t=t} in 

let bob_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  let loc = {MInit.OM.x=(98);MInit.OM.y=(17)} in
  let c0 = mk t0 in
  (* 
   * Bob sets the rgb value of the pixel at (98,17) to
   * (23,23,23).
   *)
  let c0' = MInit.OM.set_px c0 loc @@ MInit.OM.rgb @@ Char.chr 23 in
  (* Bob syncs with Alice. Observes no changes. *)
  Vpst.sync_next_version ~v:c0'.M.t >>= fun t1 ->
  let loc = {MInit.OM.x=(45); MInit.OM.y=(78)} in
  let c1 = mk t1 in
  (*
   * Bob now colors (45,78) pixel with an rgb value of
   * (111,111,111).
   *)
  let c1' = MInit.OM.set_px c1 loc @@ MInit.OM.rgb @@ Char.chr 111 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* 
   * Bob syncs with Alice. Gets latest updates. 
   * Since Alice has also colored (45,78) pixel, but with an 
   * rgb value of (17,17,17), the resultant color of the pixel 
   * is the overlap of both colors.
   *)
  Vpst.sync_next_version ~v:c1'.MInit.OM.t >>= fun t2 ->
  let _ = Printf.printf "Bob: \n" in
  let c2 = mk t2 in 
  let _ = MInit.OM.print @@ c2 in
  Vpst.return () in 
  
let alice_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Alice invites Bob for collaboration.
   *)
  Vpst.fork_version bob_f >>= fun () ->
  let loc = {MInit.OM.x=93;MInit.OM.y=127} in
  let c0 = mk t0 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* 
   * Alice sets the rgb value of the pixel at (93,127) to
   * (23,23,23).
   *)
  let c0' = MInit.OM.set_px c0 loc @@ MInit.OM.rgb @@ Char.chr 23 in
  (* Alice syncs with Bob. Sees Bob's coloring at (98,17) pixel. *)
  Vpst.sync_next_version ~v:c0'.MInit.OM.t >>= fun t1 ->
  let loc = {MInit.OM.x=45; MInit.OM.y=78} in
  let c1 = mk t1 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->

  (*
   * Alice now sets the color of (45,78) pixel to 17.
   *)
  let c1' = MInit.OM.set_px c1 loc @@ MInit.OM.rgb @@ Char.chr 17 in
  (* 
   * Alice syncs with Bob. Gets latest updates. 
   * Since Bob has also colored (45,78) pixel, the resultant
   * color of the pixel is the overlap of Alice's and Bob's.
   *)
  Vpst.sync_next_version ~v:c1'.MInit.OM.t >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = MInit.OM.print @@ mk t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return () in 

let main () =
  Vpst.with_init_version_do (MInit.OM.N {r=Char.chr (255); g=Char.chr (255); b=Char.chr (255)}) alice_f in 


main ();;
