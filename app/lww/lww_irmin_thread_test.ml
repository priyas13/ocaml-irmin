

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "LWW";
let module MkConfig (Vars: sig val root: string end) : Ilww_app.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module IntAtom = struct
  type t = int64
  let compare = Pervasives.compare
  let t = Irmin.Type.int64
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Ilww_app.MakeVersioned(CInit)(IntAtom) in 
let module M = Lww_app.Make(IntAtom) in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 adds 40 and 60 *)
  let c0' = MInit.OM.{time = (2) ; value = (Int64.of_int 2)} in
  (* Thread2 syncs with master. *)
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 adds 10. *)
  let c1' = M.update c0' (Int64.of_int 7) (4)  in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ =  print_int (Int64.to_int c2.value) in 
  Vpst.return ()  in 


 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Adds 4, 3 and 2 *)
  let c0' = M.{time = (2); value = (Int64.of_int 1)} in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Adds 1 *)
  let c1' = M.update c0' (Int64.of_int 3) (8) in
  (* Syncs with the master again. *)
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c2 >>= fun c3->
  let _ = print_int (Int64.to_int c3.value) in 
  Vpst.return ()   in 

  let main () =
   (*
    * thread1 starts with the following set denoted by original.
    *)
   let original = M.{time = 1; value = (Int64.of_int 0)} in
   Vpst.with_init_version_do original thread1_f in 

main ();;





