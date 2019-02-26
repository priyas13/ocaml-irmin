open Lwt.Infix
open Irmin_unix

module type ATOM = sig
  type t
  val t: t Irmin.Type.t
  (*val to_string : t -> string   
  val of_string: string -> t*)
  include Msigs.RESOLVEABLE with type t := t
end

module Atom = 
 struct 
  module OC = Counter.Make 
  type t = {item_id : string; item_quantity : OC.t}

  let t = let open Irmin.Type in
                (((record "madt"
                     (fun item_id -> fun item_quantity -> { item_id; item_quantity }))
                    |+
                    (field "item_id" Irmin.Type.string (fun t -> t.item_id)))
                    |+ (field "item_quantity" Irmin.Type.int64 (fun t -> (t.item_quantity))))
                  |> sealr

  let compare = Pervasives.compare

  let get_quantity_of_item i = i.item_quantity

  let merge3 ~ancestor v1 v2 = if ((ancestor.item_id = v1.item_id) && (v1.item_id = v2.item_id) && (ancestor.item_id = v2.item_id)) 
                              then {item_id = ancestor.item_id ; item_quantity = Int64.of_int (OC.merge (ancestor.item_quantity) 
                                                                                          (v1.item_quantity) 
                                                                                          (v2.item_quantity))} 
                              else failwith "Merge not possible"

  let resolve x y = {item_id = x.item_id ; item_quantity = Int64.of_int 0}

  let get_item_id item = item.item_id

  let get_item_quantity item = item.item_quantity

  let print_item item = print_string "{"; print_string "item_id" ; print_string "=" ; 
                        print_string (get_item_id item); print_string ";" ; 
                        print_string "item_quantity" ; print_string "=" ; print_int (Int64.to_int (get_item_quantity item)); print_string "}"
end

module Shoppingcart_app = 
 struct 
 module OI = Atom
 module OC = Counter.Make 
 module OS = Mvector_list.List(Atom)
 type t = {cart_id : string; items : Atom.t list}[@@derive versioned]

 let check_item_in_items item is = OS.mem item is 

 let check_item_in_cart item cart = check_item_in_items item (cart.items)

 let rec check_item_in_items_with_item_id item_id is = match is with 
                       | [] -> false
                       | c :: cl -> if OI.get_item_id c = item_id then true else check_item_in_items_with_item_id item_id cl


 let check_item_in_cart_with_item_id item_id cart = check_item_in_items_with_item_id item_id (cart.items) 

 let rec get_index_of_item_in_cart item cart = match cart.items with 
                                           | [] -> failwith "Empty cart"
                                           | c :: cl -> if OI.get_item_id c = OI.get_item_id item then 0 
                                                       else (get_index_of_item_in_cart item {cart_id = cart.cart_id ; items = cl} + 1)

 let rec add_item_to_cart item cart = 
  if (check_item_in_cart_with_item_id (OI.get_item_id item) cart) 
  then {cart_id = cart.cart_id ; 
        items = (OS.set cart.items (get_index_of_item_in_cart item cart) 
         {item_id = OI.get_item_id item; 
          item_quantity = Int64.of_int (OC.inc (OI.get_item_quantity item) (OI.get_item_quantity (OS.get cart.items (get_index_of_item_in_cart item cart))))})}
  else {cart_id = cart.cart_id ; items = List.append cart.items [item]}

  let rec remove_item_to_cart item cart = if (check_item_in_cart_with_item_id (OI.get_item_id item) cart) then 
  {cart_id = cart.cart_id ; items = (OS.set cart.items (get_index_of_item_in_cart item cart) 
          {item_id = OI.get_item_id item; 
           item_quantity = Int64.of_int (OC.dec 
                           (OI.get_item_quantity (OS.get cart.items (get_index_of_item_in_cart item cart)))
                           (OI.get_item_quantity item))})}
  else failwith "Not found"

  let rec merge ~ancestor v1 v2 = if (ancestor.cart_id = v1.cart_id && ancestor.cart_id = v2.cart_id && v1.cart_id = v2.cart_id) then
                                    {cart_id = ancestor.cart_id; items = OS.merge3 ancestor.items v1.items v2.items}
                                  else failwith "Merge not possible"

  let rec print_items items = match items with 
                          | [] -> ()
                          | i :: il -> OI.print_item i; print_string ""; print_items il

  let print_cart cart = print_string "{" ; print_string "cart_id"; print_string "="; print_string cart.cart_id; print_string ";"; 
                                            print_string "items" ; print_string "="; print_items cart.items; print_string "}"



end[@@derive_versioned]
