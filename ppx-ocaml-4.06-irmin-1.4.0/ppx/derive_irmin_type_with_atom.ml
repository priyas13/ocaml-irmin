open Ast_helper
open Ast_convenience
open Asttypes
open Parsetree
open Irmin
open Longident

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

let get_core = function
  | {pexp_desc=Pexp_ident{txt=id;_};_} ->
      (String.concat "." (Longident.flatten id))
  | _ -> failwith "error"

let rec remove_last_element l = match l with 
                            | [] -> failwith "No last element"
                            | [x] -> []
                            | [x;y] -> [x]
                            | x :: xl -> x :: remove_last_element xl 

let rec last_element l = match l with 
                     | [] -> failwith "No last element"
                     | [x] -> x 
                     | [x;y] -> y
                     | x :: xl -> last_element xl

let get_core_from_core_option ctd = match ctd.ptype_manifest with 
                             | Some x -> x
                             | None -> failwith "No such core type"



(* derive_to_json takes a list of type declarations as argument *)
(* it is a function for producing the AST of the json value of the type *)
(* t is any type *)
let derive_to_irmin (tds:type_declaration list) =
  (* t is here a type declaration *)
  let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json. The function name is like t_to_json *)
    (t.ptype_name.txt) in
  let rec kind_mapper ctd =
  (* here tt is the type expression which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let type_to_to_irmin_expr tt a =
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by using E.apply *)
        (* here the first case is for the recursive type *)
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = "K.t" in 
        evar (prefix) 
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Irmin.Type.int64"
          | "string" -> "Irmin.Type.string"
          | "char" -> "Irmin.Type.char"
          | "atom" -> "OM.atom"
          | x -> x) in
        evar (prefix)
      | Ptyp_var n -> (evar n) 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
    (match ctd.ptype_kind with
      (* type_kind of the type declaration ctd is tackled here *)
      (* type can be a variant type, abstract type, record type etc *)
      (* First case is if the type is a variant kind where l is the list of constructor_declarations *)
      (* c is the constructor_declaration *)
      (* As l is a variant type so it will be made of bunch of constructors *)
    | Ptype_variant l ->
         let mkarg x = "a" ^ string_of_int x in
         (* here c is the constructor declaration *)
         let pat_mapper c =
         let name = c.pcd_name.txt in
         let pl = pconstr name 
                  ((List.mapi (fun i e -> (Pat.var @@ mkaststr (mkarg i)))) 
                              (get_core_list (c.pcd_args))) in
         let pr =  constr (String.lowercase_ascii name) 
                   (List.mapi (fun i e -> (Exp.ident ({txt = (Lident (mkarg i)); 
                                                       loc = !Ast_helper.default_loc}))) 
                              (get_core_list (c.pcd_args))) in 
         (pl, pr) in
         let pat_list = (List.map pat_mapper l) in
         let pr' = List.fold_right (fun x y -> lam (pvar (String.lowercase_ascii x.pcd_name.txt)) y) 
                                   l (func (pat_list)) in  
         let prhs = 
          let ttt x = app (Exp.ident ({txt = (if (List.length(get_core_list (x.pcd_args))) = 0 
                      then Lident "case0" 
                      else Lident "case1"); loc = !Ast_helper.default_loc}))
                      [Exp.constant (Pconst_string (x.pcd_name.txt, None)); 
                       (type_to_to_irmin_expr 
                        (List.hd(get_core_list (x.pcd_args))) x.pcd_name.txt);                  
                      (Exp.fun_ Nolabel None (pvar "x") 
                       (Exp.construct ({txt = (Lident (x.pcd_name.txt)); 
                                        loc = !Ast_helper.default_loc}) 
                                       (Some (Exp.ident ({txt = (Lident "x"); 
                                                          loc = !Ast_helper.default_loc})))))] in
         let cc = List.map ttt l in
         let rrr = app (Exp.ident ({txt = (Lident "variant"); 
                                    loc = !Ast_helper.default_loc})) 
                       [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); pr'] in 
         let rrr' = List.fold_left 
                   (fun x y -> (app (Exp.ident ({txt = (Lident "|~"); 
                                                 loc = !Ast_helper.default_loc})) [x;y])) 
                   rrr cc in  
         let ty x = (type_to_to_irmin_expr (List.hd(get_core_list (x.pcd_args))) x.pcd_name.txt) in 
         let tys = List.map (fun x -> ty x) l in
                  (if (tys = []) 
                    then  (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                            loc = !Ast_helper.default_loc} 
                            (app (Exp.ident ({txt = (Lident "|>"); 
                                              loc = !Ast_helper.default_loc})) 
                                 [rrr'; 
                                 (Exp.ident ({txt = (Lident "sealv"); 
                                              loc = !Ast_helper.default_loc}))])) 
                    else  (lam (pvar (get_core (List.hd tys)))
                               (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                                 loc = !Ast_helper.default_loc} 
                                (app (Exp.ident ({txt = (Lident "|>"); 
                                                  loc = !Ast_helper.default_loc})) 
                                 [rrr'; 
                                 (Exp.ident ({txt = (Lident "sealv"); 
                                              loc = !Ast_helper.default_loc}))])))) in 
            Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) prhs 
   | Ptype_record l ->
        let rr = List.map (fun e -> e.pld_name.txt, evar e.pld_name.txt) l in 
        let ff = List.fold_right (fun x y -> lam (pvar x.pld_name.txt) y) l (record rr) in 
        let rrr = app (Exp.ident ({txt = (Lident "record"); 
                                   loc = !Ast_helper.default_loc})) 
                   [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); 
                    ff] in 
        let ttt x = app (Exp.ident ({txt = (Lident "field"); 
                                     loc = !Ast_helper.default_loc}))
                    [Exp.constant (Pconst_string (x.pld_name.txt, None)); 
                     (type_to_to_irmin_expr x.pld_type x.pld_name.txt); 
                    (Exp.fun_ Nolabel None (pvar "t") 
                    (Exp.field (Exp.ident ({txt = (Lident "t"); 
                                            loc = !Ast_helper.default_loc})) 
                               {txt = (Lident x.pld_name.txt); 
                                loc = !Ast_helper.default_loc}))] in
        let cc = List.map ttt l in 
        let rrr' = List.fold_left 
                    (fun x y -> (app (Exp.ident ({txt = (Lident "|+"); 
                                                  loc = !Ast_helper.default_loc})) [x;y])) 
                    rrr cc in
        let c_ty x = match x.pld_type.ptyp_desc with 
                     | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) -> true 
                     | _ -> false in  
        let ty = if (List.exists (fun x -> c_ty x) l) 
                  then (lam (pvar "t") (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                                         loc = !Ast_helper.default_loc} 
                        (app (Exp.ident ({txt = (Lident "|>"); 
                                          loc = !Ast_helper.default_loc})) 
                          [rrr'; 
                           (Exp.ident ({txt = (Lident "sealr"); 
                                        loc = !Ast_helper.default_loc}))]))) 
                  else (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                         loc = !Ast_helper.default_loc} 
                          (app (Exp.ident ({txt = (Lident "|>"); 
                                            loc = !Ast_helper.default_loc})) 
                             [rrr'; 
                              (Exp.ident ({txt = (Lident "sealr"); 
                                           loc = !Ast_helper.default_loc}))])) in 
        if (List.exists (fun x -> c_ty x) l) 
          then Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) ty 
          else Vb.mk (pvar @@ (mk_to_irmin_name ctd)) ty
    | Ptype_abstract ->  
       let exp_ty ctd = 
        (match ctd.ptype_manifest with 
         | Some x -> (match x.ptyp_desc with 
                      | Ptyp_tuple l -> 
                        (match l with 
                         | [] -> failwith "Nothing left"
                         | a :: al -> let mapper a = (match a.ptyp_desc with 
                                      | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                                      | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                                     
                                      | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                                      | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                        if n = ctd.ptype_name.txt then (constr ("K.t") []) 
                                                                 else (match n with 
                                                                      | "int64" -> (constr "int64" [])
                                                                      | "char" -> (constr "char" [])
                                                                      | "string" -> (constr "string" [])
                                                                      | "atom" -> (constr "OM.atom" [])
                                                                      | _ -> assert false)
                                      | Ptyp_constr ({txt = n}, x) -> let tn = String.concat "." (Longident.flatten n) in
                                                                       let prefix = (match tn with
                                                                          | "int" -> "int64"
                                                                          | "string" -> "string"
                                                                          | "char" -> "char"
                                                                          | "Atom.t" -> "Atom.t"
                                                                          | "atom" -> "OM.atom"
                                                                          | x -> x) in (constr prefix [])
                                      | _ -> assert false) in 
                                       (List.fold_right (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); loc = !Ast_helper.default_loc})) [x; y])) 
                                         (remove_last_element (List.map (fun x -> mapper x) l)) (last_element ((List.map (fun x -> mapper x) l))))) 
                    | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                    | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                                    if n = ctd.ptype_name.txt 
                                                    then (constr ("K.t") []) 
                                                     else (match n with 
                                                          | "int64" -> (constr "int64" [])
                                                          | "char" -> (constr "char" [])
                                                          | "string" -> (constr "string" [])
                                                          | "atom" -> (constr "OM.atom" [])
                                                          | "list" -> (match x with 
                                                                       | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                                                                         (constr "list Atom.t" [])
                                                                       | _ -> assert false)
                                                          | _ -> assert false)
                    | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
                    | _ -> (evar "Irmin.Type.int64"))
       | None -> assert false) in 
         let rec c_ty x = match x.ptyp_desc with 
                      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) -> true 
                      | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                                    if n = ctd.ptype_name.txt 
                                                    then true
                                                     else (match n with 
                                                          | "int64" -> false
                                                          | "char" -> false
                                                          | "string" -> false
                                                          | "atom" -> false
                                                          | "list" -> (match x with 
                                                                       | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                                                                         false
                                                                       | _ -> assert false)
                                                          | _ -> assert false)
                      | Ptyp_tuple l -> (List.exists (fun x -> c_ty x) l)
                      | _ -> false in 
         if (List.exists (fun x -> c_ty x) [(get_core_from_core_option ctd)]) then
         Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) (lam (pvar "t") 
          (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} (exp_ty ctd))) else
         Vb.mk (pvar @@ (mk_to_irmin_name ctd)) 
          (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} (exp_ty ctd))
         (*{pvb_pat = pvar (mk_to_irmin_name ctd) ; 
pvb_expr = (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type");loc = !Ast_helper.default_loc}
                        (app (Exp.ident ({txt = (Lident "list"); loc = !Ast_helper.default_loc}))
                             [(Exp.ident ({txt = Ldot (Lident "OM", "atom"); loc = !Ast_helper.default_loc}))]));
pvb_attributes = [];
pvb_loc = !Ast_helper.default_loc}*)
    | Ptype_open -> assert false) in
  Str.value Nonrecursive (List.map kind_mapper tds)

  (* derive_to_json takes a list of type declarations as argument *)
(* it is a function for producing the AST of the json value of the type *)
(* t is any type *)
let derive_to_irmin_tie (tds:type_declaration list) =
  (* t is here a type declaration *)
  let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json. The function name is like t_to_json *)
    (t.ptype_name.txt) in
  let rec kind_mapper ctd =
  (* here tt is the type expression which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let type_to_to_irmin_expr tt a =
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by using E.apply *)
        (* here the first case is for the recursive type *)
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = "K.t" in 
        evar (prefix) 
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Irmin.Type.int64"
          | "string" -> "Irmin.Type.string"
          | "char" -> "Irmin.Type.char"
          | "atom" -> "OM.atom"
          | x -> x) in
        evar (prefix)
      | Ptyp_var n -> (evar n) 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
    (match ctd.ptype_kind with
    | Ptype_variant l -> 
            let ty x = (type_to_to_irmin_expr (List.hd(get_core_list (x.pcd_args))) x.pcd_name.txt) in 
           let tys = List.map (fun x -> ty x) l in
           let prhs = (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} 
            (app (evar "mu2") [lam (pvar (get_core (List.hd tys))) (lam (pvar (mk_to_irmin_name ctd))
                                (tuple [constr ("IrminConvert" ^ "." ^ "mk" ^ (get_core (List.hd tys))) [(evar (mk_to_irmin_name ctd))] ;
                                        constr ("IrminConvert" ^ "." ^ "mk" ^ (mk_to_irmin_name ctd)) [(evar ((get_core (List.hd tys))))]]))])) in 
           if tys = [] then {pvb_pat = punit(); pvb_expr= unit(); pvb_attributes = []; pvb_loc = !Ast_helper.default_loc}
           else Vb.mk (ptuple [pvar (get_core (List.hd tys)); pvar (mk_to_irmin_name ctd)]) prhs  
    | Ptype_record l -> {pvb_pat = punit(); pvb_expr= unit(); pvb_attributes = []; pvb_loc = !Ast_helper.default_loc}
    | Ptype_abstract ->  let exp_ty ctd = 
        (match ctd.ptype_manifest with 
         | Some x -> (match x.ptyp_desc with 
                      | Ptyp_tuple l -> 
                        (match l with 
                         | [] -> failwith "Nothing left"
                         | a :: al -> let mapper a = (match a.ptyp_desc with 
                                      | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                                      | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                                     
                                      | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                                      | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                        if n = ctd.ptype_name.txt then (constr ("K.t") []) 
                                                                 else (match n with 
                                                                      | "int64" -> (constr "int64" [])
                                                                      | "char" -> (constr "char" [])
                                                                      | "string" -> (constr "string" [])
                                                                      | "atom" -> (constr "OM.atom" [])
                                                                      | _ -> assert false)
                                      | Ptyp_constr ({txt = n}, x) -> let tn = String.concat "." (Longident.flatten n) in
                                                                       let prefix = (match tn with
                                                                          | "int" -> "int64"
                                                                          | "string" -> "string"
                                                                          | "char" -> "char"
                                                                          | "Atom.t" -> "Atom.t"
                                                                          | "atom" -> "OM.atom"
                                                                          | x -> x) in (constr prefix [])
                                      | _ -> assert false) in 
                                       (List.fold_right (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); loc = !Ast_helper.default_loc})) [x; y])) 
                                         (remove_last_element (List.map (fun x -> mapper x) l)) (last_element ((List.map (fun x -> mapper x) l))))) 
                    | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                    | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                                    if n = ctd.ptype_name.txt 
                                                    then (constr ("K.t") []) 
                                                     else (match n with 
                                                          | "int64" -> (constr "int64" [])
                                                          | "char" -> (constr "char" [])
                                                          | "string" -> (constr "string" [])
                                                          | "atom" -> (constr "OM.atom" [])
                                                          | "list" -> (match x with 
                                                                       | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                                                                         (constr "list Atom.t" [])
                                                                       | _ -> assert false)
                                                          | _ -> assert false)
                    | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
                    | _ -> (evar "Irmin.Type.int64"))
       | None -> assert false) in 
         let rec c_ty x = match x.ptyp_desc with 
                      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) -> true 
                      | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                                    if n = ctd.ptype_name.txt 
                                                    then true
                                                     else (match n with 
                                                          | "int64" -> false
                                                          | "char" -> false
                                                          | "string" -> false
                                                          | "atom" -> false
                                                          | "list" -> (match x with 
                                                                       | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                                                                         false
                                                                       | _ -> assert false)
                                                          | _ -> assert false)
                      | Ptyp_tuple l -> (List.exists (fun x -> c_ty x) l)
                      | _ -> false in 
         if (List.exists (fun x -> c_ty x) [(get_core_from_core_option ctd)]) then
         Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) (lam (pvar "t") 
          (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} (exp_ty ctd))) else
         Vb.mk (pvar @@ (mk_to_irmin_name ctd)) 
          (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} (exp_ty ctd))
                         
    | Ptype_open -> assert false) in
      Str.value Nonrecursive (List.map kind_mapper tds)
    
 

