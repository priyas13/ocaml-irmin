open Printf

(* Utility functions *)
(* U is a module with two functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"

  let print_header h = 
    begin 
      Printf.printf "%s\n" h;
      flush_all();
    end

  let (>>=) = Lwt.Infix.(>>=)

  let rec loop_until_y (msg:string) : unit Lwt.t = 
    Lwt_io.printf "%s" msg >>= fun _ ->
    Lwt_io.read_line Lwt_io.stdin >>= fun str ->
    if str="y" then Lwt.return ()
    else loop_until_y msg

  let fold f n b = 
    let rec fold_aux f i b = 
      if i >= n then b 
      else fold_aux f (i+1) @@ f i b in
    fold_aux f 0 b
end 

(* Canvas *)
let _ =
  U.print_header "Red-Black Set"

module MkConfig (Vars: sig val root: string end) : Igraph.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module Atom = struct
  type t = int32
  let t = Irmin.Type.int32
  let compare x y = Int32.to_int @@ Int32.sub x y
  let to_string = Int32.to_string
  let of_string = Int32.of_string
end
(*
 * --------------------------------------------------------
 * Note: this alice monkey is setup to do diff experiments.
 * For more general experiments, change the repo root path, and
 * uncomment bob_url
 * --------------------------------------------------------
 *)
module CInit = MkConfig(struct let root = "/tmp/repos/graph_imp2.git" end)
module OG = Graph_imp.Make
module OS = OG.OS
module ISet = Iset_imp.MakeVersioned(CInit)(Graph_imp.Edge_type)
module IGRaph =Igraph.MakeVersioned(CInit)(Graph_imp.Edge_type)
(ISet.BC_value)
module GR = Graph_imp.Make
module GRaph = GR
module Vpst = IGRaph.Vpst

let uris = [(*"git+ssh://opam@172.18.0.3/tmp/repos/rbset.git";
            "git+ssh://opam@172.18.0.4/tmp/repos/rbset.git"*)]

let seed = 564294298

let _ = Random.init seed

let (>>=) = Vpst.bind

let loop_until_y msg = Vpst.liftLwt @@ U.loop_until_y msg

let do_an_insert_node t = 
  let n = (Random.int64 9000000L) in 
  let l = sprintf "item %d" (Int64.to_int n) in 
  GR.insert_node n l t

let do_an_insert_edge t = 
  let v = (Random.int64 9000000L) in 
  let w = (Random.int64 9000000L) in 
  let l = sprintf "item %d" (Int64.to_int v) in 
  GR.insert_edge v w l t

let do_delete_node t = 
  let n = (Random.int64 9000000L) in 
  GR.delete_node n t

let do_delete_edge t = 
  let v = (Random.int64 9000000L) in 
  let w = (Random.int64 9000000L) in 
  let l = sprintf "item %d" (Int64.to_int v) in 
  GR.delete_edge v w l t

(* do_an_oper performs the operation either insert or remove
   : if input choosen is random number 0 or 1 then it performs insert else it performs remove *)
let do_an_oper t = 
  match Random.int 10 with
    | 0 -> do_delete_edge t
    | 1 -> do_delete_node t 
    | 2|3|4 -> do_an_insert_node t
    | _ -> do_an_insert_edge t

let comp_time = ref 0.0

let sync_time = ref 0.0

let avg_tree_size = ref 0

let _n_ops_per_round = ref 30

let _n_rounds = ref 10

let loop_iter i (pre: GRaph.t Vpst.t) : GRaph.t Vpst.t = 
  pre >>= fun t ->
  let t1 = Sys.time() in
  let t' = U.fold (fun _ c -> do_an_oper c) !_n_ops_per_round (t) in
  let t2 = Sys.time() in
  Vpst.sync_next_version ~v:t' >>= fun v ->
  let t3 = Sys.time () in
  let _ = flush_all() in
  let size = GRaph.graph_length t' in
  begin 
    comp_time := !comp_time +. (t2 -. t1);
    sync_time := !sync_time +. (t3 -. t2);
    avg_tree_size := ((!avg_tree_size * i)+size)/( i+1);
    printf "Round %d\n" i;
    flush_all();
    Vpst.return v
  end

let work_loop () : GRaph.t Vpst.t = 
  U.fold loop_iter !_n_rounds (Vpst.get_latest_version ())

let experiment_f ((*fp: out_channel*)) : unit =
  begin
    CInit.init ();
    Vpst.with_init_version_do GRaph.empty
      begin 
        (*Vpst.fork_version (work_loop ()) >>= fun br1 ->
        Vpst.fork_version ~parent:br1 (work_loop ()) >>= fun br2 ->
        Vpst.set_parent br2 >>= fun () ->*)
        (work_loop ()) >>= fun _ ->
        Vpst.liftLwt @@ Lwt_unix.sleep 5.0
      end;
    printf "Done\n";
    let kb = (!avg_tree_size * 141)/1024 in
    printf "%d\n" kb;
    (*
    let mtime = !IRBSet.merge_time in
    let ctime = !comp_time in
    let stime = !sync_time in
    let mcount = !IRBSet.merge_count in
    let real_mtime = !IRBSet.real_merge_time in
    let total_rounds = 3 * !_n_rounds in
    let ctime_per_round = ctime/.(float total_rounds) in
    let mdivisor = if mcount > total_rounds then mcount 
                   else total_rounds in
    let avg_mtime = real_mtime/.(float mdivisor) in
    fprintf fp "%d,%d,%fs,%fs,%fs,%d,%fs,%fs,%fs\n" 
                !_n_rounds !_n_ops_per_round 
                mtime ctime stime mcount real_mtime 
                ctime_per_round avg_mtime;
    reset ()
     *)
  end

let main () =
  begin
    Logs.set_reporter @@ Logs.format_reporter ();
    Logs.set_level @@ Some Logs.Error;
    _n_rounds := int_of_string @@ Sys.argv.(1);
    _n_ops_per_round := int_of_string @@ Sys.argv.(2);
    if Array.length Sys.argv > 3 then
      Random.init @@ int_of_string @@ Sys.argv.(3)
    else Random.self_init ();
    (*let fp = open_out_gen [Open_append; Open_creat] 
              0o777 "results.csv" in*)
    experiment_f ()
  end;;

main ();;