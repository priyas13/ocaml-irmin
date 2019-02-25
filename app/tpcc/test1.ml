 let module U = Tpcc.User_id_type in 
 let module W = Tpcc.Warehouse in
 let module D = Tpcc.District in 
 let module C = Tpcc.Customer in 
 let module O = Tpcc.Order in 
 let module OL = Tpcc.Orderline in 
 let module NO = Tpcc.Neworder in 
 let module S = Tpcc.Stock in 
 let module I = Tpcc.Item in 
 let module IR = Tpcc.Itemreq in 
 let module H = Tpcc.History in 
 let module M = Tpcc.Tpcc in 
 let original = {M.wt = [{W.w_id = 'w'; W.w_name = "Warehouse1"; W.w_state = "Indiana"; W.w_ytd = 100}; {W.w_id = 'w'; W.w_name = "Warehouse1"; W.w_state = "Indiana"; W.w_ytd = 100}]; 
                 M.dt = [{D.d_id = 'd'; D.d_name = "District1"; D.d_w_id = 'w'; D.d_ytd = 500; D.d_next_o_id = 2}];
                 M.ct = [{C.c_id = 'c'; C.c_d_id = 'd'; C.c_w_id = 'w'; C.c_ytd = 0; C.c_payment_ct = 0; C.c_bal = 0; C.c_delivery_ct = 0}];
                 M.ot = [{O.o_id = 1; O.o_c_id = 'c'; O.o_d_id = 'd'; O.o_w_id = 'w'; O.o_ol_ct = 1}];
                 M.olt = [{OL.ol_o_id = 1; OL.ol_d_id = 'd'; OL.ol_w_id = 'w'; OL.ol_num = 1; OL.ol_amt = 0; OL.ol_i_id = 'o'; OL.ol_supply_w_id = 'w'; OL.ol_qty = 20}];
                 M.ont = [{NO.no_o_id = 2; NO.no_d_id = 'd'; NO.no_w_id = 'w'}];
                 M.st = [{S.s_i_id = 's'; S.s_w_id = 'w'; S.s_qty = 90; S.s_ytd = 100; S.s_order_cnt = 0}];
                 M.it = [{I.i_id = 'i'; I.i_name = "Item1"; I.i_price = 10}];
                 M.irt = [{IR.ir_num = 1; IR.ir_i_id = 'i'; IR.ir_supply_w_id = 'w'; IR.ir_qty = 5}];
                 M.ht = [{H.h_c_id = 'c'; H.h_c_d_id = 'd'; H.h_c_w_id = 'w'; H.h_d_id = 'd'; H.h_w_id = 'w'; H.h_amt = 0}]} in 
                 M.print_warehouse_table original.wt

(* {M.cart_id = "cart1" ; M.items = [{I.item_id = "apple"; I.item_quantity = 2}; {I.item_id = "orange" ; I.item_quantity = 4};
                                                  {I.item_id = "cherry" ; I.item_quantity = 4}]} in
 let original' = {M.cart_id = "cart1"; M.items = []}in 
 let original'' = 
 (M.remove_item_to_cart {I.item_id = "apple"; I.item_quantity = 2} 
 (M.add_item_to_cart {I.item_id = "watermelon"; I.item_quantity = 3} (M.add_item_to_cart {I.item_id = "apple"; I.item_quantity = 3} original))) in 
 M.print_cart (M.merge original original' original'') ;
 print_newline()*)

