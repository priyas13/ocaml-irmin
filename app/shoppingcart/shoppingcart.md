# Shopping cart
In the shopping cart example, there are two kinds of data types. One is the items and the other is the cart.

Cart is a list of items. Hence a list data data type will be used to represent the shopping cart.

Item is a record datatype with fields as the name of the item and the number representing how many items of that type. Here the number of items in the cart can be a counter data type. 

## Merging carts in a three-way merge fashion 
For merging the shopping cart we should use the merge operation supported by list data type and counter data type. Why? 
Because shopping cart data type is a composition of list data type and counter data type

##Compile and run 
ocamlfind ocamlopt counter.ml msigs.ml  mvector_list.ml  shoppingcart_app.ml cart_test1.ml cart_test2.ml cart_test3.ml cart_test4.ml -o sc -package ezjsonm,irmin-unix,irmin-http,lwt.unix -linkpkg -thread

./sc


