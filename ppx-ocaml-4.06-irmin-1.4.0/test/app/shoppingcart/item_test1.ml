 let module M = Shoppingcart_app.Item in 
 let original = {M.item_id = "apple" ; M.item_quantity = 4} in
 let v1 = {M.item_id = "apple"; M.item_quantity = 2} in 
 let v2 = {M.item_id = "apple"; M.item_quantity = 8} in 
 let m = M.merge3 original v1 v2 in 
 print_int m.item_quantity 