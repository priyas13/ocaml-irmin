open Lwt.Infix
open Irmin_unix

module Make = struct 

type item = {item_name : int; quantity : int}

type cart = {cart_name : int; list item} 

let remove_items_from_cart 