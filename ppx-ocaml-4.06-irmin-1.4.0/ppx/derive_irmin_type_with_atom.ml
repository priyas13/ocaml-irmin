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

(* t is any type *)
let derive_to_irmin (tds:type_declaration list) = 
  match tds with 
  | [] -> Str.value Nonrecursive []
  | [x] -> Str.value Nonrecursive []
  (* t is here a type declaration *)
  | x :: y -> let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (t.ptype_name.txt) in
  let rec kind_mapper ctd =
  (* here tt is the core_type which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let type_to_to_irmin_expr tt a =
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by using E.apply *)
        (* here the first case is for the recursive type *)
      | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
      | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (constr ("K.t") []) 
              else 
               (match n with 
                | "int" -> (constr "int64" [])
                | "int32" -> (constr "int32" [])
                | "int64" -> (constr "int64" [])
                | "char" -> (constr "char" [])
                | "string" -> (constr "string" [])
                | "atom" -> (constr "OM.atom" [])
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (constr "list Atom.t" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (constr "list int32" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (constr "list char" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (constr "list string" [])
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "int" -> (constr "int64" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (constr prefix [])
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | [{ptyp_desc = Ptyp_constr ({txt = Lident x}, [])}] -> (constr x [])
                       | _ -> assert false)
                    | x -> (constr x []))
      | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
      | Ptyp_tuple l -> 
          (match l with 
           | [] -> failwith "Nothing left"
           | a :: al -> 
             let mapper a = 
               (match a.ptyp_desc with 
                  | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                  | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                    (constr ("Atom.t") [])
                  | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                  | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                    if n = ctd.ptype_name.txt 
                     then (constr ("K.t") []) 
                     else (match n with 
                          | "int64" -> (constr "int64" [])
                          | "int32" -> (constr "int32" [])
                          | "char" -> (constr "char" [])
                          | "string" -> (constr "string" [])
                          | "atom" -> (constr "OM.atom" [])
                          | _ -> assert false)
                  | Ptyp_constr ({txt = n}, x) -> 
                    let tn = String.concat "." (Longident.flatten n) in
                       let prefix = (match tn with
                          | "int" -> "int64"
                          | "int32" -> "int32"
                          | "string" -> "string"
                          | "char" -> "char"
                          | "Atom.t" -> "Atom.t"
                          | "atom" -> "OM.atom"
                          | x -> x) in (constr prefix [])
                  | _ -> assert false) in 
                   (List.fold_right 
                    (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                     (remove_last_element (List.map (fun x -> mapper x) l)) 
                     (last_element ((List.map (fun x -> mapper x) l))))) 
      | Ptyp_var n -> (evar n) 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
     let case_variant c = match c.pcd_args with 
                 | Pcstr_record l' -> assert false
                 | Pcstr_tuple l' -> 
                   match l' with 
                   | [] -> app (Exp.ident ({txt = Lident "case0"; 
                                            loc = !Ast_helper.default_loc}))
                           [Exp.constant (Pconst_string (c.pcd_name.txt, None));
                            evar (c.pcd_name.txt)]
                   | x :: xl -> app (Exp.ident ({txt = Lident "case1"; 
                                                 loc = !Ast_helper.default_loc}))
                                    [Exp.constant (Pconst_string (c.pcd_name.txt, None)); 
                                     (type_to_to_irmin_expr 
                                      (List.hd (get_core_list (c.pcd_args))) c.pcd_name.txt);                  
                                    (Exp.fun_ Nolabel None (pvar "x") 
                                     (Exp.construct ({txt = (Lident (c.pcd_name.txt)); 
                                                      loc = !Ast_helper.default_loc}) 
                                                     (Some (Exp.ident ({txt = (Lident "x"); 
                                                                        loc = !Ast_helper.default_loc})))))] in 
    let mkty c = match c.pcd_args with 
               | Pcstr_record l' -> assert false 
               | Pcstr_tuple l' -> 
                 match l' with 
                  | [] -> evar (c.pcd_name.txt)
                  | x :: xl -> 
               (match x.ptyp_desc with 
            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (evar ("Atom.t"))
            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (evar ("K.t")) 
              else 
               (match n with 
                | "int32" -> (evar "int32")
                | "int64" -> (evar "int64")
                | "char" -> (evar "char")
                | "string" -> (evar "string")
                | "atom" -> (evar "OM.atom")
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (evar "list Atom.t")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (evar "list int64")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (evar "list int32")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (evar "list int64")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (evar "list char")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (evar "list string")
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (evar ("K.t"))
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (evar ("Atom.t"))
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (evar ("K.t")) 
                                 else (match n with 
                                      | "int64" -> (evar "int64")
                                      | "int32" -> (evar "int32")
                                      | "int" -> (evar "int64")
                                      | "char" -> (evar "char")
                                      | "string" -> (evar "string")
                                      | "atom" -> (evar "OM.atom")
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (evar prefix)
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | [{ptyp_desc = Ptyp_constr ({txt = Lident x}, [])}] -> (evar x)
                       | _ -> assert false)
                    | x -> evar x)
                  | Ptyp_constr ({txt = t}, x) -> (evar ("K.t"))
                  | Ptyp_tuple l -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                           (match a.ptyp_desc with 
                              | Ptyp_constr ({txt = Lident "t"}, []) -> (evar ("K.t"))
                              | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                                (evar ("Atom.t"))
                              | Ptyp_constr ({txt = Lident "elt"}, []) -> (evar ("Atom.t"))
                              | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                if n = ctd.ptype_name.txt 
                                 then (evar ("K.t")) 
                                 else (match n with 
                                      | "int64" -> (evar "int64")
                                      | "int32" -> (evar "int32")
                                      | "char" -> (evar "char")
                                      | "string" -> (evar "string")
                                      | "atom" -> (evar "OM.atom")
                                      | _ -> assert false)
                              | Ptyp_constr ({txt = n}, x) -> 
                                let tn = String.concat "." (Longident.flatten n) in
                                   let prefix = (match tn with
                                      | "int" -> "int64"
                                      | "int32" -> "int32"
                                      | "string" -> "string"
                                      | "char" -> "char"
                                      | "Atom.t" -> "Atom.t"
                                      | "atom" -> "OM.atom"
                                      | x -> x) in (evar prefix)
                              | _ -> assert false) in 
                               (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l)) 
                                 (last_element ((List.map (fun x -> mapper x) l))))) 
                     | _ -> assert false) in
 let pat_mapper_tuple c = match c.pcd_args with 
                             | Pcstr_record l' -> assert false
                             | Pcstr_tuple l' -> 
                               match l' with 
                              | [] -> let name = c.pcd_name.txt in 
                                      let pl = pconstr name [] in 
                                      let pr = constr (String.lowercase_ascii name) [] in 
                                      (pl,pr) 
                              | x :: xl ->  let mkarg x = "a" ^ string_of_int x in
                                            let name = c.pcd_name.txt in
                                            let pl = pconstr name 
                                                     ((List.mapi (fun i e -> (Pat.var @@ mkaststr (mkarg i)))) 
                                                     (get_core_list (c.pcd_args))) in
                                            let pr =  constr (String.lowercase_ascii name) 
                                                      (List.mapi (fun i e -> (Exp.ident ({txt = (Lident (mkarg i)); 
                                                                     loc = !Ast_helper.default_loc}))) 
                                            (get_core_list (c.pcd_args))) in 
                               (pl, pr) in
    match ctd.ptype_kind with
      | Ptype_open -> assert false 
      | Ptype_abstract ->  let exp_ty ctd = 
        (match ctd.ptype_manifest with 
         | Some c -> 
           (match c.ptyp_desc with
            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (constr ("K.t") []) 
              else 
               (match n with 
                | "int32" -> (constr "int32" [])
                | "int64" -> (constr "int64" [])
                | "char" -> (constr "char" [])
                | "string" -> (constr "string" [])
                | "atom" -> (constr "OM.atom" [])
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (constr "list Atom.t" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (constr "list int32" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (constr "list char" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (constr "list string" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = Lident "t"}, [])}] -> (app (evar "list") [(evar "K.t")])
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "int" -> (constr "int64" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (constr prefix [])
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | [{ptyp_desc = Ptyp_constr ({txt = Lident x}, [])}] -> (app (evar "list") [(evar x)])
                       | _ -> assert false)
                    | x -> constr x [])
                  | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
                  | Ptyp_tuple l -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let rec mapper a = 
                           (match a.ptyp_desc with 
                              | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                              | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                                (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                if n = ctd.ptype_name.txt 
                                 then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                              | Ptyp_constr ({txt = n}, x) -> 
                                let tn = String.concat "." (Longident.flatten n) in
                                   let prefix = (match tn with
                                      | "int" -> "int64"
                                      | "int32" -> "int32"
                                      | "string" -> "string"
                                      | "char" -> "char"
                                      | "Atom.t" -> "Atom.t"
                                      | "atom" -> "OM.atom"
                                      | x -> x) in (constr prefix [])
                              | Ptyp_tuple l' -> (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l')) 
                                 (last_element ((List.map (fun x -> mapper x) l'))))
                              | _ -> assert false) in 
                               (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l)) 
                                 (last_element ((List.map (fun x -> mapper x) l))))) 
                     | _ -> assert false)
        | None -> assert false) in
            let prhs =  
            (lam (pvar @@ "t") 
           (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                             loc = !Ast_helper.default_loc} (exp_ty ctd))) in 
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
        let ty = (lam (pvar "t") 
                 (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                         loc = !Ast_helper.default_loc} 
                          (app (Exp.ident ({txt = (Lident "|>"); 
                                            loc = !Ast_helper.default_loc})) 
                             [rrr'; 
                              (Exp.ident ({txt = (Lident "sealr"); 
                                           loc = !Ast_helper.default_loc}))]))) in 
        Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) ty  
      | Ptype_variant l ->
        match l with 
        | [] -> failwith "no constructors"
        | x :: xl -> 
           match x.pcd_args with 
            | Pcstr_record l' -> assert false
            | Pcstr_tuple l' -> 
              let pat_list = List.map (pat_mapper_tuple) l in
              let pr' = List.fold_right (fun x y -> lam (pvar (String.lowercase_ascii x.pcd_name.txt)) y) 
                                             l (func (pat_list)) in 
              let prhs =
              let cc = List.map (case_variant) l in 
              let rrr = app (Exp.ident ({txt = (Lident "variant"); 
                                              loc = !Ast_helper.default_loc})) 
                        [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); pr'] in 
              let rrr' = List.fold_left 
                        (fun x y -> (app (Exp.ident ({txt = (Lident "|~"); 
                                                           loc = !Ast_helper.default_loc})) [x;y])) 
                             rrr cc in 
              let l' = List.map (mkty) (List.tl l) in 
              (lam (pvar (get_core (List.hd l'))) ((Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                                           loc = !Ast_helper.default_loc} 
                                          (app (Exp.ident ({txt = (Lident "|>"); 
                                                            loc = !Ast_helper.default_loc})) 
                                           [rrr'; 
                                           (Exp.ident ({txt = (Lident "sealv"); 
                                                        loc = !Ast_helper.default_loc}))])))) in 
              Vb.mk (pvar @@ ("mk" ^ mk_to_irmin_name ctd)) prhs in  
        Str.value Nonrecursive (List.map kind_mapper tds)

(* t is any type *)
let derive_to_irmin_tie (tds:type_declaration list) = 
  match tds with 
  | [] -> Str.value Nonrecursive []
  | [x] -> let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
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
      | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (constr ("K.t") []) 
              else 
               (match n with 
                | "int32" -> (constr "int32" [])
                | "int64" -> (constr "int64" [])
                | "char" -> (constr "char" [])
                | "string" -> (constr "string" [])
                | "atom" -> (constr "OM.atom" [])
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (constr "list Atom.t" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (constr "list int32" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (constr "list char" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (constr "list string" [])
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "int" -> (constr "int64" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (constr prefix [])
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | _ -> assert false)
                    | x -> (constr "x" []))
                  | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
                  | Ptyp_tuple l -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                           (match a.ptyp_desc with 
                              | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                              | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                                (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                if n = ctd.ptype_name.txt 
                                 then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                              | Ptyp_constr ({txt = n}, x) -> 
                                let tn = String.concat "." (Longident.flatten n) in
                                   let prefix = (match tn with
                                      | "int" -> "int64"
                                      | "int32" -> "int32"
                                      | "string" -> "string"
                                      | "char" -> "char"
                                      | "Atom.t" -> "Atom.t"
                                      | "atom" -> "OM.atom"
                                      | x -> x) in (constr prefix [])
                              | _ -> assert false) in 
                               (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l)) 
                                 (last_element ((List.map (fun x -> mapper x) l))))) 
      | Ptyp_var n -> (evar n) 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
        let case_variant c = match c.pcd_args with 
                 | Pcstr_record l' -> assert false
                 | Pcstr_tuple l' -> 
                   match l' with 
                   | [] -> app (Exp.ident ({txt = Lident "case0"; 
                                            loc = !Ast_helper.default_loc}))
                           [Exp.constant (Pconst_string (c.pcd_name.txt, None));
                            evar (c.pcd_name.txt)]
                   | x :: xl -> app (Exp.ident ({txt = Lident "case1"; 
                                                 loc = !Ast_helper.default_loc}))
                                    [Exp.constant (Pconst_string (c.pcd_name.txt, None)); 
                                     (type_to_to_irmin_expr 
                                      (List.hd (get_core_list (c.pcd_args))) c.pcd_name.txt);                  
                                    (Exp.fun_ Nolabel None (pvar "x") 
                                     (Exp.construct ({txt = (Lident (c.pcd_name.txt)); 
                                                      loc = !Ast_helper.default_loc}) 
                                                     (Some (Exp.ident ({txt = (Lident "x"); 
                                                                        loc = !Ast_helper.default_loc})))))] in 
    let pat_mapper_tuple c = match c.pcd_args with 
                             | Pcstr_record l' -> assert false
                             | Pcstr_tuple l' -> 
                               match l' with 
                              | [] -> let name = c.pcd_name.txt in 
                                      let pl = pconstr name [] in 
                                      let pr = constr (String.lowercase_ascii name) [] in 
                                      (pl,pr) 
                              | x :: xl ->  let mkarg x = "a" ^ string_of_int x in
                                            let name = c.pcd_name.txt in
                                            let pl = pconstr name 
                                                     ((List.mapi (fun i e -> (Pat.var @@ mkaststr (mkarg i)))) 
                                                     (get_core_list (c.pcd_args))) in
                                            let pr =  constr (String.lowercase_ascii name) 
                                                      (List.mapi (fun i e -> (Exp.ident ({txt = (Lident (mkarg i)); 
                                                                     loc = !Ast_helper.default_loc}))) 
                                            (get_core_list (c.pcd_args))) in 
                               (pl, pr) in
    match ctd.ptype_kind with
      | Ptype_open -> assert false 
      | Ptype_abstract ->  let exp_ty ctd = 
        (match ctd.ptype_manifest with 
         | Some c -> 
           (match c.ptyp_desc with
            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (constr ("K.t") []) 
              else 
               (match n with 
                | "int32" -> (constr "int32" [])
                | "int64" -> (constr "int64" [])
                | "char" -> (constr "char" [])
                | "string" -> (constr "string" [])
                | "atom" -> (constr "OM.atom" [])
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (constr "list Atom.t" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (constr "list int32" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (constr "list int64" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (constr "list char" [])
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (constr "list string" [])
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (constr ("Atom.t") [])
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "int" -> (constr "int64" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (constr prefix [])
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | _ -> assert false)
                    | _ -> assert false)
                  | Ptyp_constr ({txt = t}, x) -> (constr ("K.t") [])
                  | Ptyp_tuple l -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                           (match a.ptyp_desc with 
                              | Ptyp_constr ({txt = Lident "t"}, []) -> (constr ("K.t") [])
                              | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                                (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Lident "elt"}, []) -> (constr ("Atom.t") [])
                              | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                if n = ctd.ptype_name.txt 
                                 then (constr ("K.t") []) 
                                 else (match n with 
                                      | "int64" -> (constr "int64" [])
                                      | "int32" -> (constr "int32" [])
                                      | "char" -> (constr "char" [])
                                      | "string" -> (constr "string" [])
                                      | "atom" -> (constr "OM.atom" [])
                                      | _ -> assert false)
                              | Ptyp_constr ({txt = n}, x) -> 
                                let tn = String.concat "." (Longident.flatten n) in
                                   let prefix = (match tn with
                                      | "int" -> "int64"
                                      | "int32" -> "int32"
                                      | "string" -> "string"
                                      | "char" -> "char"
                                      | "Atom.t" -> "Atom.t"
                                      | "atom" -> "OM.atom"
                                      | x -> x) in (constr prefix [])
                              | _ -> assert false) in 
                               (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l)) 
                                 (last_element ((List.map (fun x -> mapper x) l))))) 
                     | _ -> assert false)
        | None -> assert false) in 
            Vb.mk (pvar @@ (mk_to_irmin_name ctd)) 
           (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                             loc = !Ast_helper.default_loc} (exp_ty ctd))
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
        let ty = (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                         loc = !Ast_helper.default_loc} 
                          (app (Exp.ident ({txt = (Lident "|>"); 
                                            loc = !Ast_helper.default_loc})) 
                             [rrr'; 
                              (Exp.ident ({txt = (Lident "sealr"); 
                                           loc = !Ast_helper.default_loc}))])) in 
        Vb.mk (pvar @@ (mk_to_irmin_name ctd)) ty  
      | Ptype_variant l ->
        match l with 
        | [] -> failwith "no constructors"
        | x :: xl -> 
           match x.pcd_args with 
            | Pcstr_record l' -> assert false
            | Pcstr_tuple l' -> 
              let pat_list = List.map (pat_mapper_tuple) l in
              let pr' = List.fold_right (fun x y -> lam (pvar (String.lowercase_ascii x.pcd_name.txt)) y) 
                                             l (func (pat_list)) in 
              let prhs =
              let cc = List.map (case_variant) l in 
              let rrr = app (Exp.ident ({txt = (Lident "variant"); 
                                              loc = !Ast_helper.default_loc})) 
                                 [Exp.constant (Pconst_string (mk_to_irmin_name ctd, None)); pr'] in 
                   let rrr' = List.fold_left 
                             (fun x y -> (app (Exp.ident ({txt = (Lident "|~"); 
                                                           loc = !Ast_helper.default_loc})) [x;y])) 
                             rrr cc in  
                   (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); 
                                                           loc = !Ast_helper.default_loc} 
                                          (app (Exp.ident ({txt = (Lident "|>"); 
                                                            loc = !Ast_helper.default_loc})) 
                                           [rrr'; 
                                           (Exp.ident ({txt = (Lident "sealv"); 
                                                        loc = !Ast_helper.default_loc}))])) in 
              Vb.mk (pvar @@ (mk_to_irmin_name ctd)) prhs in  
        Str.value Nonrecursive (List.map kind_mapper tds)
  (* t is here a type declaration *)
  | x :: y -> let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (t.ptype_name.txt) in
  let rec kind_mapper ctd =
  (* here tt is the type expression which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)

    let mkty c = match c.pcd_args with 
               | Pcstr_record l' -> assert false 
               | Pcstr_tuple l' -> 
                 match l' with 
                  | [] -> evar (c.pcd_name.txt)
                  | x :: xl -> 
               (match x.ptyp_desc with 
            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (evar ("Atom.t"))
            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
              if n = ctd.ptype_name.txt 
              then (evar ("K.t")) 
              else 
               (match n with 
                | "int32" -> (evar "int32")
                | "int64" -> (evar "int64")
                | "char" -> (evar "char")
                | "string" -> (evar "string")
                | "atom" -> (evar "OM.atom")
                | "list" -> 
                  (match x with 
                   | [{ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, [])}] -> 
                     (evar "list Atom.t")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int")}, [])}] -> 
                     (evar "list int64")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int32")}, [])}] -> 
                     (evar "list int32")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "int64")}, [])}] -> 
                     (evar "list int64")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "char")}, [])}] -> 
                     (evar "list char")
                   | [{ptyp_desc = Ptyp_constr ({txt = (Lident "string")}, [])}] -> 
                     (evar "list string")
                   | [{ptyp_desc = Ptyp_tuple l}] -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                          (match a.ptyp_desc with 
                            | Ptyp_constr ({txt = Lident "t"}, []) -> (evar ("K.t"))
                            | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> (evar ("Atom.t"))
                            | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                              if n = ctd.ptype_name.txt 
                                then (evar ("K.t")) 
                                 else (match n with 
                                      | "int64" -> (evar "int64")
                                      | "int32" -> (evar "int32")
                                      | "int" -> (evar "int64")
                                      | "char" -> (evar "char")
                                      | "string" -> (evar "string")
                                      | "atom" -> (evar "OM.atom")
                                      | _ -> assert false)
                            | Ptyp_constr ({txt = n}, x) -> 
                              let tn = String.concat "." (Longident.flatten n) in
                                 let prefix = (match tn with
                                    | "int" -> "int64"
                                    | "int32" -> "int32"
                                    | "int64" -> "int64"
                                    | "string" -> "string"
                                    | "char" -> "char"
                                    | "Atom.t" -> "Atom.t"
                                    | "atom" -> "OM.atom"
                                    | x -> x) in (evar prefix)
                            | _ -> assert false) in 
                             app (evar "list") [(List.fold_right 
                              (fun x y -> 
                                (app (Exp.ident ({txt = (Lident "pair"); 
                                                  loc = !Ast_helper.default_loc})) [x; y])) 
                               (remove_last_element (List.map (fun x -> mapper x) l)) 
                               (last_element ((List.map (fun x -> mapper x) l))))]) 
                       | _ -> assert false)
                    | x -> evar x)
                  | Ptyp_constr ({txt = t}, x) -> (evar ("K.t"))
                  | Ptyp_tuple l -> 
                      (match l with 
                       | [] -> failwith "Nothing left"
                       | a :: al -> 
                         let mapper a = 
                           (match a.ptyp_desc with 
                              | Ptyp_constr ({txt = Lident "t"}, []) -> (evar ("K.t"))
                              | Ptyp_constr ({txt = Ldot (Lident "Atom", "t")}, []) -> 
                                (evar ("Atom.t"))
                              | Ptyp_constr ({txt = Lident "elt"}, []) -> (evar ("Atom.t"))
                              | Ptyp_constr ({txt = Longident.Lident n}, x) -> 
                                if n = ctd.ptype_name.txt 
                                 then (evar ("K.t")) 
                                 else (match n with 
                                      | "int64" -> (evar "int64")
                                      | "int32" -> (evar "int32")
                                      | "char" -> (evar "char")
                                      | "string" -> (evar "string")
                                      | "atom" -> (evar "OM.atom")
                                      | _ -> assert false)
                              | Ptyp_constr ({txt = n}, x) -> 
                                let tn = String.concat "." (Longident.flatten n) in
                                   let prefix = (match tn with
                                      | "int" -> "int64"
                                      | "int32" -> "int32"
                                      | "string" -> "string"
                                      | "char" -> "char"
                                      | "Atom.t" -> "Atom.t"
                                      | "atom" -> "OM.atom"
                                      | x -> x) in (evar prefix)
                              | _ -> assert false) in 
                               (List.fold_right 
                                (fun x y -> (app (Exp.ident ({txt = (Lident "pair"); 
                                                              loc = !Ast_helper.default_loc})) [x; y])) 
                                 (remove_last_element (List.map (fun x -> mapper x) l)) 
                                 (last_element ((List.map (fun x -> mapper x) l))))) 
                     | _ -> assert false) in 
    match ctd.ptype_kind with
      | Ptype_open -> assert false 
      | Ptype_abstract -> {pvb_pat = punit(); pvb_expr= unit(); pvb_attributes = []; pvb_loc = !Ast_helper.default_loc} 
            (*let prhs = (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} 
            (app (evar "mu2") [lam (pvar @@ (mk_to_irmin_name x)) (lam (pvar (mk_to_irmin_name ctd))
                                (tuple [constr ("IrminConvert" ^ "." ^ "mk" ^ (mk_to_irmin_name x)) [(evar (mk_to_irmin_name ctd))] ;
                                        constr ("IrminConvert" ^ "." ^ "mk" ^ (mk_to_irmin_name ctd)) [(evar (mk_to_irmin_name x))]]))])) in 
           Vb.mk (ptuple [pvar (mk_to_irmin_name x); pvar (mk_to_irmin_name ctd)]) prhs*)
      | Ptype_record l -> {pvb_pat = punit(); pvb_expr= unit(); pvb_attributes = []; pvb_loc = !Ast_helper.default_loc} 
      | Ptype_variant l ->
        let l' = List.map (mkty) (List.tl l) in 
        let prhs = (Exp.open_ Fresh {txt = Ldot (Lident "Irmin", "Type"); loc = !Ast_helper.default_loc} 
            (app (evar "mu2") [lam (pvar (get_core (List.hd l'))) (lam (pvar (mk_to_irmin_name ctd))
                                (tuple [constr ("IrminConvert" ^ "." ^ "mk" ^ (get_core (List.hd l'))) [(evar (mk_to_irmin_name ctd))] ;
                                        constr ("IrminConvert" ^ "." ^ "mk" ^ (mk_to_irmin_name ctd)) [(evar (get_core (List.hd l')))]]))])) in 
           Vb.mk (ptuple [pvar (get_core (List.hd l')); pvar (mk_to_irmin_name ctd)]) prhs in  
        Str.value Nonrecursive (List.map kind_mapper tds)


    
 

