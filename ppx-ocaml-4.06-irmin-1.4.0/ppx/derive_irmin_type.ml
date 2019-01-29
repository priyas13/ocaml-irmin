open Ast_helper
open Ast_convenience
open Asttypes
open Parsetree
open Irmin

(* find_some_type is trying to find the type declaration with type name tn *)
(* here l is the list of type declaration *)
(* if l is empty then None 
      i is of form x :: y then we start from the head
                               if the ptype_name is tn then x else we call the same function on the tail of the list *)
let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn

(* mkaststr takes str (string) as argument and returns a record type with extension node name and location as fields *)
let mkaststr str = {txt=str; loc = !Ast_helper.default_loc}

  let get_core_list c' = match c' with
                               | Pcstr_tuple ct' -> ct'
                               | Pcstr_record rt' -> invalid_arg "Derive.get_core_list" 


let derive_to_irmin_type (tds: type_declaration list) = 
	let open Irmin.Type in 
	(* here t is a type declaration *)
	(* the name is same as the type name defined in the functor module above *)
	let mk_to_irmin_name t = 
    (* ptype_name field of t is to_json *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json *)
    t.ptype_name.txt  in
    (* here ctd is the type declaration record *)
  let rec kind_mapper ctd =
  (* here tt is the core_type record which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let type_to_to_irmin_type_value tt a =
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by yusing E.apply *)
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = String.concat "." (Longident.flatten n) ^ "." in
        app (evar ("Irmin.Type" ^ "." ^ prefix)) [(evar a)]
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Irmin.Type.int64"
          | "string" -> "Irmin.Type.string"
          | "char" -> "Irmin.Type.char"
          | x -> x ^ "_") in
            app (evar ("Irmin.Type" ^ "." ^ prefix)) [(evar a)]      
      | Ptyp_var n -> (* TODO *) assert false 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
      (match ctd.ptype_kind with 
       | Ptype_record l ->
       (* here l is the list of label declarations which is basically fields in the record *)
         let plhs = precord ~closed:Closed @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in 
         let prhs = 
           let ttt x = tuple [ (str x.pld_name.txt); (type_to_to_irmin_type_value x.pld_type x.pld_name.txt)] in 
           let cc = List.map ttt l in 
            Exp.variant ("|+") (Some (Ast_convenience.list cc)) in 
            Vb.mk (pvar @@ mk_to_irmin_name ctd) (func [(plhs, prhs)])
       | Ptype_abstract -> (* TODO *) assert false
    | Ptype_open -> assert false 
      | Ptype_variant l -> assert false) in 
      Str.value Recursive (List.map kind_mapper tds)




