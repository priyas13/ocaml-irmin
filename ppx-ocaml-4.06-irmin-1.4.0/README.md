# Compatible with OCaml 4.06 and Irmin 1.4 (I guess it will also work for OCaml 4.07 (the latest))

## How to compile and create the native code of the ppx rewriter 
Go to the root of the directory 

### Module not functor
ocamlbuild -use-ocamlfind ppx/new_ppx_dali.native 

- This is used for modules not functors

- The directory contains new_ppx_dali.native which can be directly used without doing the above step

### Functor
ocamlbuild -use-ocamlfind ppx/new_ppx_dali_with_atom.native

- This is used for functors which takes only one argument and its names as Atom and whose type is ATOM
- The directory contains new_ppx_dali_with_atom.native which can be directly used without doing the above step


## How to run the rewriter on the example code
Go the root of the directory

### The source_file.ml should be equipped with extension nodes 

- Add [@@derive versioned] after type with name as t 

- Add [@@derive ezjsonm] for all the other type 

- Add [@@derive_versioned] just after the end of the module 

#### For examples consisting of module not functor 
ocamlfind ppx_tools/rewriter ./new_ppx_dali.native source_file.ml > derived_file.ml

#### For examples consisting of functor 
ocamlfind ppx_tools/rewriter ./new_ppx_dali_with_atom.native source_file.ml > derived_file.ml

The transformed code will be present in derived_file.ml which has both the source and derived_file in it. So no need to include the source file while compiling or running the derived version.

## Drawbacks:
(1) Support of only one argument in functor which is named as Atom and whose type is named as ATOM. Restriction on argument and its naming.

(2) The module or functor name should be same as the file name.

(3) The work has been tested on (compilation with OCaml 4.06 and Irmin 1.4):

    - set 
    
    - list
    
    - heap
    
    - rbtree 
    
    - counter 
    
    - canvas
    
(4) There could be other flaws but I could correct them easily if any example fails and also trying to fix drawback 1. I     tested with the types presented in above examples. But working on testing more. 

(5) Some of the design decisions are also dependend on Irmin. For example: The following type t should be broken down into two types.

    
    type t = | Node of {r : int; l : char}
    
    type node = {r : int; l : char}
    
    type t = Node of node

Irmin verison creates some issue when we define it in the above form.

(6) Use int32 or int64 instead of int because Irmin supports only these. 

(7) For using Atom.t please use it directly instead of naming it as elt or atom or anything else.
    
## I will keep updating the ppx so please use the most recent version on [my github](https://github.com/priyas13/ocaml-irmin/tree/master/ppx-ocaml-4.06-irmin-1.4.0).
