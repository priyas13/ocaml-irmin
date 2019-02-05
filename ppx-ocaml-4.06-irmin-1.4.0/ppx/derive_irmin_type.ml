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



(* derive_to_json takes a list of type declarations as argument *)
(* it is a function for producing the AST of the json value of the type *)
(* t is any type *)
let derive_to_irmin (tds:type_declaration list) =
  (* t is here a type declaration *)
  let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json. The function name is like t_to_json *)
    t.ptype_name.txt in
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
           let pl = pconstr name ((List.mapi (fun i e -> (Pat.var @@ mkaststr (mkarg i)))) (get_core_list (c.pcd_args))) in
           let pr =  constr (String.lowercase_ascii name) (List.mapi (fun i e -> (Exp.ident ({txt = (Lident (mkarg i)); loc = !Ast_helper.default_loc}))) (get_core_list (c.pcd_args))) in 
           (pl, pr) in
           let pat_list = (List.map pat_mapper l) in
           let pr' = List.fold_right (fun x y -> lam (pvar (String.lowercase_ascii x.pcd_name.txt)) y) l (func (pat_list)) in  
           let prhs = 
            let ttt x = app (Exp.ident ({txt = (if (List.length(get_core_list (x.pcd_args))) = 0 then Lident "case0" else Lident "case1"); loc = !Ast_helper.default_loc}))
                    [Exp.constant (Pconst_string (x.pcd_name.txt, None)); 
                     (type_to_to_irmin_expr (List.hd(get_core_list (x.pcd_args))) x.pcd_name.txt);                  
                    (Exp.fun_ Nolabel None (pvar "x") 
                     (Exp.construct ({txt = (Lident (x.pcd_name.txt)); loc = !Ast_helper.default_loc}) (Some (Exp.ident ({txt = (Lident "x"); loc = !Ast_helper.default_loc})))))] in
           let cc = List.map ttt l in
           let rrr = app (Exp.ident ({txt = (Lident "variant"); loc = !Ast_helper.default_loc})) 
            [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); pr'] in 
           let rrr' = List.fold_left (fun x y -> (app (Exp.ident ({txt = (Lident "|~"); loc = !Ast_helper.default_loc})) [x;y])) rrr cc in 
           Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} 
             (app (Exp.ident ({txt = (Lident "|>"); loc = !Ast_helper.default_loc})) 
              [rrr'; (Exp.ident ({txt = (Lident "sealv"); loc = !Ast_helper.default_loc}))]) in 
            Vb.mk (pvar @@ (mk_to_irmin_name ctd)) prhs
    | Ptype_record l ->
      let prhs = 
        let rr = List.map (fun e -> e.pld_name.txt, evar e.pld_name.txt) l in 
        let ff = List.fold_right (fun x y -> lam (pvar x.pld_name.txt) y) l (record rr) in 
        let rrr = app (Exp.ident ({txt = (Lident "record"); loc = !Ast_helper.default_loc})) 
        [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); ff] in 
        let ttt x = app (Exp.ident ({txt = (Lident "field"); loc = !Ast_helper.default_loc}))
                    [Exp.constant (Pconst_string (x.pld_name.txt, None)); 
                     (type_to_to_irmin_expr x.pld_type x.pld_name.txt); 
                    (Exp.fun_ Nolabel None (pvar "t") 
                    (Exp.field (Exp.ident ({txt = (Lident "t"); loc = !Ast_helper.default_loc})) 
                               {txt = (Lident x.pld_name.txt); loc = !Ast_helper.default_loc}))] in
        let cc = List.map ttt l in 
        let rrr' = List.fold_left (fun x y -> (app (Exp.ident ({txt = (Lident "|+"); loc = !Ast_helper.default_loc})) [x;y])) rrr cc in 
        Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} 
          (app (Exp.ident ({txt = (Lident "|>"); loc = !Ast_helper.default_loc})) 
             [rrr'; (Exp.ident ({txt = (Lident "sealr"); loc = !Ast_helper.default_loc}))]) in 
        Vb.mk (pvar @@ (mk_to_irmin_name ctd)) prhs
    | Ptype_abstract ->  {pvb_pat = {ppat_desc = Ppat_any; ppat_loc= !Ast_helper.default_loc; ppat_attributes = []}; pvb_expr = {pexp_desc = Pexp_unreachable; pexp_loc = !Ast_helper.default_loc; pexp_attributes = []}; pvb_attributes = [] ; pvb_loc = !Ast_helper.default_loc}
    | Ptype_open -> assert false) in
  Str.value Nonrecursive (List.map kind_mapper tds)

