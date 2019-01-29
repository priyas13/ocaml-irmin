# Compatible with OCaml 4.06 and Irmin 1.4 (I guess it will also work for OCaml 4.07 (the latest))

## How to compile and create the native code of the ppx rewriter 
Go to the root of the directory 

ocamlbuild -use-ocamlfind ppx/new_ppx_dali.native 

## How to run the rewriter on the example code
Go the root of the directory

ocamlfind ppx_tools/rewriter ./new_ppx_dali.native canvas.ml > derived.ml 

The transformed code will be present in derived.ml
