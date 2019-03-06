

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end 

(* Set - AVL Tree *)
let _ =
  U.print_header "BANKAPP";
let module MkConfig (Vars: sig val root: string end) : Ibank_app.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end in 

let module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) in 
let module MInit = Ibank_app.MakeVersioned(CInit) in 
let module M = Bank_app.Make in 
let module Vpst = MInit.Vpst in 

let (>>=) = Vpst.bind  in


let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 adds 40 and 60 *)
  let c0' = MInit.OM.{acct_id = 1 ; value = 10} in
  (* Thread2 syncs with master. *)
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 adds 10. *)
  let c1' = MInit.OM.deposit c0' 5  in 
  Vpst.sync_next_version ~v:c1' >>= fun c2 ->
  let _ =  print_int (c1'.value) in 
  Vpst.return ()  in 


 let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Adds 4, 3 and 2 *)
  let c0' = MInit.OM.{acct_id = (1); value = (10)} in
  Vpst.sync_next_version ~v:c0' >>= fun c1 ->
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Adds 1 *)
  let c1' = MInit.OM.deposit c0' 5 in
  (* Syncs with the master again. *)
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version ~v:c1' >>= fun c3 ->
  let _ = print_int (c1'.value) in 
  Vpst.return ()   in 

  let main () =
   (*
    * thread1 starts with the following set denoted by original.
    *)
   let original = MInit.OM.{acct_id = 1; value = (10)} in
   Vpst.with_init_version_do original thread1_f in 

main ();;




