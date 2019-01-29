open Ast_helper
open Ast_convenience
open Asttypes
open Parsetree

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

(* derive_to_json takes a list of type declarations as argument *)
(* it is a function for producing the AST of the json value of the type *)
(* t is any type *)
let derive_to_json (tds:type_declaration list) =
  (* t is here a type declaration *)
  let mk_to_json_name t = 
    (* ptype_name field of t is the type name *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json. The function name is like t_to_json *)
    t.ptype_name.txt ^ "_to_json" in
    (* here ctd is the type declaration *)
  let rec kind_mapper ctd =
  (* here tt is the type expression which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let type_to_to_json_expr tt a =
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by yusing E.apply *)
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = String.concat "." (Longident.flatten n) ^ "." in
        app (evar (prefix ^ "to_json")) [(evar a)]
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Tc.Int."
          | "string" -> "Tc.String."
          | x -> x ^ "_") in
          app (evar (prefix ^ "to_json")) [(evar a)]
      | Ptyp_var n -> (* TODO *) assert false 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
    (match ctd.ptype_kind with
      (* type_kind of the type declaration ctd is tackled here *)
      (* type can be a variant type, abstract type, record type etc *)
      (* First case is if the type is a variant kind where l is the list of constructor_declarations *)
      (* c is the constructor_declaration *)
      (* As l is a variant type so it will be made of bunch of constructors *)
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      (* Here c is the constructor declaration  which is passed as argument to the Ptype_variant *)
      (* A variant type can have multiple constructors, here c is one of the constructors *)
      let pat_mapper c =
        let name = c.pcd_name.txt in
        (* Pat.var is a function defined in AST_helper file which is used to create a pattern which consist of ppat_desc, ppat_loc and ppat_attributes *)
        (* Pat.var is being applied to a string (where string is created by converting the int i to string) *)
        let plhs = pconstr name ((List.mapi (fun i e -> (Pat.var @@ mkaststr (mkarg i)))) (get_core_list (c.pcd_args))) in
        let prhs =
          (* Exp.variant creates a expression with pexp_desc as Pexp_variant *)
          let cc = Exp.variant ("String") (Some (str name)) in
          let ca = List.mapi (fun i e -> type_to_to_json_expr e (mkarg i)) (get_core_list (c.pcd_args)) in 
          Exp.variant ("A") (Some (Ast_convenience.list (cc :: ca))) in
        (plhs, prhs) in
      Vb.mk (pvar @@ mk_to_json_name ctd) (func (List.map pat_mapper l))
      (* l is the list of label_declarations because record is made of fields *)
    | Ptype_record l ->
      let plhs = precord ~closed:Closed @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs =
        let ttt x = tuple [ (str x.pld_name.txt); (type_to_to_json_expr x.pld_type x.pld_name.txt)] in
        let cc = List.map ttt l in
        Exp.variant ("O") (Some (Ast_convenience.list cc)) in
      Vb.mk (pvar @@ mk_to_json_name ctd) (func [(plhs, prhs)])
    | Ptype_abstract -> (* TODO *) assert false
    | Ptype_open -> assert false) in
  (* Here we are using List.map to map the function kind_mapper to each element of the tds (type declarations) *)
  (* As we saw before the kind_mapper takes a type declaration as an argument *)
  Str.value Recursive (List.map kind_mapper tds)

let derive_of_json (tds:type_declaration list) =
  let mk_of_json_name t = 
    t.ptype_name.txt ^ "_of_json" in
  let fallback_pattern e =
    pvar "j", app (evar "Ezjsonm.parse_error") [evar "j"; str @@ mk_of_json_name e] in  
  let rec kind_mapper ctd =
    let type_to_of_json_expr tt a =
      (match tt.ptyp_desc with
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = String.concat "." (Longident.flatten n) ^ "." in
        app (evar (prefix ^ "of_json")) [(evar a)]
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Tc.Int."
          | "string" -> "Tc.String."
          | x -> x ^ "_") in
          app (evar (prefix ^ "of_json")) [(evar a)]
      | Ptyp_var n -> (* TODO *) assert false 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
    (match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let pat_mapper c =
        let name = c.pcd_name.txt in
        let plhs =
          let cc = Pat.variant ("String") (Some (pstr name)) in
          let ca = List.mapi (fun i e -> pvar (mkarg i)) (get_core_list (c.pcd_args)) in 
          Pat.variant ("A") (Some (plist (cc :: ca))) in
        let prhs = constr name (List.mapi (fun i e -> type_to_of_json_expr e (mkarg i)) (get_core_list (c.pcd_args))) in
        (plhs, prhs) in
      let pat_list = (List.map pat_mapper l) @ [(fallback_pattern ctd)] in
      Vb.mk (pvar @@ mk_of_json_name ctd) (func pat_list)
    | Ptype_record l ->
      let plhs =
        let ttt x = ptuple [ (pstr x.pld_name.txt); (pvar x.pld_name.txt)] in
        let cc = List.map ttt l in
        Pat.variant ("O") (Some (plist cc)) in
      let prhs = record @@ List.map (fun e -> e.pld_name.txt, type_to_of_json_expr e.pld_type e.pld_name.txt) l in
      let pat_list = [(plhs, prhs)] @ [(fallback_pattern ctd)] in
      Vb.mk (pvar @@ mk_of_json_name ctd) (func pat_list)
    | Ptype_abstract -> (* TODO *) assert false
    | Ptype_open -> assert false) in
  Str.value Recursive (List.map kind_mapper tds)
