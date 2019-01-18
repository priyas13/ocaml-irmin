open Ast_helper
open Ast_convenience
open Asttypes
open Parsetree

(** ERRORS **)
(* File "ppx/derive_ezjsonm.ml", line 46, characters 90-100:
Error: This expression has type Parsetree.constructor_arguments
       but an expression was expected of type 'a list *)
(* File "ppx/derive_ezjsonm.ml", line 82, characters 31-56:
Error: This expression has type Ast_convenience.Label.t = Asttypes.arg_label
       but an expression was expected of type Asttypes.label = string *)

let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn

(* mkaststr takes str (string) as argument and returns a record type with extension node name and location as fields *)
let mkaststr str = {txt=str; loc = !Ast_helper.default_loc}

(* derive_to_json takes a list of type declarations as argument *)
let derive_to_json (tds:type_declaration list) =
  (* t is here a type declaration *)
  let mk_to_json_name t = 
    (* ptype_name field of t is to_json *)
    (* ^ concatenates the name with _to_jason *)
    t.ptype_name.txt ^ "_to_json" in
    (* here ctd is the type declaration record *)
  let rec kind_mapper ctd =
  (* here tt is the core_type record which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
    let type_to_to_json_expr tt a =
      (match tt.ptyp_desc with
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
      (* type_kind of the type declaration ctd  is tackled here *)
      (* First case is if the type is a variant kind where l is the list of constructor_declarations *)
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      (* Here c is the constructor declaration  whihc is passed as argument to the Ptype_variant *)
      let pat_mapper c =
        let name = c.pcd_name.txt in
        let plhs = pconstr name (( (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)))) c.pcd_args) in
        let prhs =
          let cc = Exp.variant (Label.labelled "String") (Some (str name)) in
          let ca = List.mapi (fun i e -> type_to_to_json_expr e (mkarg i)) c.pcd_args in 
          Exp.variant (Label.labelled "A") (Some (Ast_convenience.list (cc :: ca))) in
        (plhs, prhs) in
      Vb.mk (pvar @@ mk_to_json_name ctd) (func (List.map pat_mapper l))
    | Ptype_record l ->
      let plhs = precord ~closed:Closed @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs =
        let ttt x = tuple [ (str x.pld_name.txt); (type_to_to_json_expr x.pld_type x.pld_name.txt)] in
        let cc = List.map ttt l in
        Exp.variant (Label.labelled "O") (Some (Ast_convenience.list cc)) in
      Vb.mk (pvar @@ mk_to_json_name ctd) (func [(plhs, prhs)])
    | Ptype_abstract -> (* TODO *) assert false
    | Ptype_open -> assert false) in
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
          let cc = Pat.variant (Label.labelled "String") (Some (pstr name)) in
          let ca = List.mapi (fun i e -> pvar (mkarg i)) c.pcd_args in 
          Pat.variant (Label.labelled "A") (Some (plist (cc :: ca))) in
        let prhs = constr name (List.mapi (fun i e -> type_to_of_json_expr e (mkarg i)) c.pcd_args) in
        (plhs, prhs) in
      let pat_list = (List.map pat_mapper l) @ [(fallback_pattern ctd)] in
      Vb.mk (pvar @@ mk_of_json_name ctd) (func pat_list)
    | Ptype_record l ->
      let plhs =
        let ttt x = ptuple [ (pstr x.pld_name.txt); (pvar x.pld_name.txt)] in
        let cc = List.map ttt l in
        Pat.variant (Label.labelled "O") (Some (plist cc)) in
      let prhs = record @@ List.map (fun e -> e.pld_name.txt, type_to_of_json_expr e.pld_type e.pld_name.txt) l in
      let pat_list = [(plhs, prhs)] @ [(fallback_pattern ctd)] in
      Vb.mk (pvar @@ mk_of_json_name ctd) (func pat_list)
    | Ptype_abstract -> (* TODO *) assert false
    | Ptype_open -> assert false) in
  Str.value Recursive (List.map kind_mapper tds)
