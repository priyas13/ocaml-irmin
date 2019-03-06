 let module I = Shoppingcart_app.Item in 
 let module M = Shoppingcart_app.Cart in 
 let original = {M.cart_id = "cart1" ; M.items = [{I.item_id = "apple"; I.item_quantity = 2}; {I.item_id = "orange" ; I.item_quantity = 4};
                                                  {I.item_id = "cherry" ; I.item_quantity = 4}]} in
 let original' = 
 (M.add_item_to_cart {I.item_id = "orange"; I.item_quantity = 1} (M.add_item_to_cart {I.item_id = "apple"; I.item_quantity = 2} original)) in 
 let original'' = 
 (M.add_item_to_cart {I.item_id = "apple"; I.item_quantity = 3} (M.add_item_to_cart {I.item_id = "apple"; I.item_quantity = 3} original)) in 
 M.print_cart (M.merge original original' original'') ;
 print_newline() 

