open Lwt.Infix
open Irmin_unix

module Make = struct 

type t = {acct_id : int; value : int}

let get_balance {acct_id = i; value = v} = v

let withdraw {acct_id = i; value = v} amt = if amt < v then {acct_id = i; value = v - amt} else {acct_id = i; value = v}

let deposit {acct_id = i; value = v} amt = {acct_id = i; value = v + amt}

let get_id {acct_id = i; value = v} = i

type edit = 
  | Withdraw of int
  | Deposit of int  

type patch = edit list 

let edit_to_string atom_to_string = function
    | Withdraw (a) -> Printf.sprintf "Withdraw (%s)" (atom_to_string a)
    | Deposit (a) -> Printf.sprintf "Deposit (%s)" (atom_to_string a)

let get_edit p = match p with 
      | Withdraw x -> x
      | Deposit y -> y


let op_diff b1 b2 = let c = Pervasives.compare (get_balance b1) (get_balance b2) in 
                    if c > 0 then [Withdraw (get_balance b1 - get_balance b2)]
                    else if c < 0 then [Deposit (get_balance b2 - get_balance b1)]
                    else [] 

let rec op_transform b p q = match p, q with
                      | [],[] -> [], []
                      | x, [] -> x, []
                      | [], y -> [], y
                      | [Withdraw x], [Withdraw y] -> if (x + y) > get_balance b then if x > y then [], [Deposit (x-y)]
                                                                                               else if x < y then [Deposit (y-x)], []
                                                                                                             else [], []
                                                                                 else [Withdraw y], [Withdraw x]
                      | [Deposit x], [Deposit y] -> [Deposit x], [Deposit y]
                      | [Deposit x], [Withdraw y] -> [Deposit x], [Withdraw y] 
                      | x, y -> op_transform b p q
                      


let apply s = function
    | [] -> s
    | [Withdraw x] -> withdraw s x 
    | [Deposit x] -> deposit s x 


let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let p', q' = op_transform ancestor p q in 
    apply l q'

end 
