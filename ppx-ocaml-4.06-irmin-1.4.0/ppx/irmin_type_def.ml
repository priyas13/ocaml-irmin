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

(* derive_to_json takes a list of type declarations as argument *)
(* it is a function for producing the AST of the json value of the type *)
(* t is any type *)
let irmin_type_def (tds:type_declaration list) td mn =
  (* t is here a type declaration *)
  let mk_to_irmin_name t = 
    (* ptype_name field of t is the type name *)
    (* ^ concatenates the name with _to_json *)
    (* Because we are converting the type to json, hence we concatenate the type name of the declaration with to_json. The function name is like t_to_json *)
    t.ptype_name.txt in
    (* here ctd is the type declaration *)
  let rec kind_mapper ctd =
  (* here tt is the type expression which contains ptyp_desc, ptyp_loc and ptyp_attributes *)
  (* ptyp_desc for the core type tt is matched against each constructor *)
  (* a is the value *)
    let main_td_mapper = { default_mapper with typ = (fun mapper tt a ->
      (match tt.ptyp_desc with
        (* Here n is a type and t is the string. Longident.flatten n returns the string of type n *)
        (* evar a creates the Exp.ident with the text and location *)
        (* app calls the function on the second argument by yusing E.apply *)
        (* here the first case is for the recursive type *)
      | Ptyp_constr ({txt = Longident.Ldot (n, "t")}, x) ->
        let prefix = "K.t" in 
        prefix 
      | Ptyp_constr ({txt = n}, x) ->
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "Irmin.Type.int64"
          | "string" -> "Irmin.TYpe.string"
          | "char" -> "Irmin.Type.char"
          | x -> x ^ "_") in
        prefix
      | Ptyp_var n -> assert false 
      | _ -> failwith "[derive ezjsonm]: Not implemented.") in
    (match ctd.ptype_kind with
      (* type_kind of the type declaration ctd is tackled here *)
      (* type can be a variant type, abstract type, record type etc *)
      (* First case is if the type is a variant kind where l is the list of constructor_declarations *)
      (* c is the constructor_declaration *)
      (* As l is a variant type so it will be made of bunch of constructors *)
    | Ptype_variant l ->
      let prhs = evar "a" in 
        Vb.mk (pvar @@ mk_to_irmin_name ctd) prhs
    | Ptype_record l ->
      let prhs = 
        let ttt x = app (evar "|+") [evar "field"; (str x.pld_name.txt); evar (type_to_to_irmin_expr x.pld_type x.pld_name.txt); lam (pvar "t") (evar ("t." ^ (x.pld_name.txt)))] in
        let cc = List.map ttt l in
        let rr = List.map (fun e -> e.pld_name.txt, evar e.pld_name.txt) l in 
        (*let aa x = pvar (x.pld_name.txt) in*)
        (*let aa' = List.map aa l in *)
        let a = pconstr ((List.hd l).pld_name.txt) [] in  
        let ff = lam a (record rr) in 
        let rrr = app (evar "record") (List.append [str (mk_to_irmin_name ctd); ff] cc) in 
        app (evar "|>") [rrr ; (evar "sealr")] in 
        Vb.mk (pvar @@ mk_to_irmin_name ctd) prhs
    | Ptype_abstract ->  assert false
    | Ptype_open -> assert false) in
  (* Here we are using List.map to map the function kind_mapper to each element of the tds (type declarations) *)
  (* As we saw before the kind_mapper takes a type declaration as an argument *)
  Str.value Nonrecursive (List.map kind_mapper tds) in 
   let type_dec_mapper = 
    {   
      default_mapper with
      type_declaration = (fun mapper t -> 
          match t with
          | { ptype_attributes = [({txt = "derive"}, _) ]; } -> 
            main_td_mapper.type_declaration main_td_mapper 
              { t with ptype_name = mkaststr "madt"; ptype_params = []; ptype_attributes = [] }
          | x -> default_mapper.type_declaration other_td_mapper x);
    } in
  let madts = List.map (type_dec_mapper.type_declaration type_dec_mapper) tds in 
  ((Ast_helper.Str.type_ Recursive madts), madts)

