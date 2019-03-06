open Asttypes
open Parsetree
open Ast_mapper
open Ast_convenience
open Irmin

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
      let err = Location.error ?sub ?if_highlight ?loc str in
      raise (Location.Error err))

let mkaststr str = {txt=str; loc = !Ast_helper.default_loc}
 
(* To map:
   [%%dali_imodstr_rename]
   [%%dali_mmodsig_functs]
   [%%dali_mmodstr_functs]
   ...
   [%dali_adt_mod] = module
   [%dali_adt_typesig] = 'a t --> A.t t
   [%dali_madt_typedef] = type 'a t = T0 | T1 of 'a * 'a t --> type internal = T0 | T1 of K.t * A.t
   [%dali_aovalue_equal] = match t1, t2 with
   [%dali_aovalue_to_json] = to_json
   [%dali_aovalue_of_json] = of_json
   [%dali_of_adt]
   [%dali_to_adt]
*)

(* mk_dali_mapper *)
(* This function takes care of all the mapping needed  *)
(* all the arguments provided are of different types *)
let mk_dali_mapper (mn, mnf, mts, mtsf, mtd, ic, ict, ofa, toa, ada, rda, uadt, imr, msigf, mstrf, mergf, mmodi, bcstoi) = {
  default_mapper with 
  module_expr = (fun mapper t ->
      match t with
      | { pmod_desc = Pmod_extension ({txt = "dali_adt_mod"}, _) } -> mn
      | { pmod_desc = Pmod_extension ({txt = "dali_adt_mod_func"}, _) } -> mnf
      | { pmod_desc = Pmod_extension ({txt = "dali_mmod_inst"}, _) } -> mmodi
      | { pmod_desc = Pmod_extension ({txt = "dali_bcsto_inst"}, _) } -> bcstoi
      | x -> default_mapper.module_expr mapper x);
  (* mapping of the type signatures *)
  typ = (fun mapper t ->
      match t with
      | { ptyp_desc = Ptyp_extension ({txt = "dali_adt_typesig"}, _) } -> mts
      | { ptyp_desc = Ptyp_extension ({txt = "dali_adt_typesig_func"}, _) } -> mtsf
      | x -> default_mapper.typ mapper x);
  structure_item = (fun mapper t ->
      match t with
      | { pstr_desc = Pstr_extension (({txt = "dali_madt_typedef"}, _), _) } -> mapper.structure_item mapper mtd
      | { pstr_desc = Pstr_extension (({txt = "dali_irmin_convert"}, _), _) } -> ic
      | { pstr_desc = Pstr_extension (({txt = "dali_irmin_tie_convert"}, _), _) } -> ict
      | { pstr_desc = Pstr_extension (({txt = "dali_imodstr_rename"}, p), _) } ->
        mapper.structure_item mapper (imr p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mmodsig_functs"}, p), _) } ->
        mapper.structure_item mapper (msigf p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mmodstr_functs"}, p), _) } ->
        mapper.structure_item mapper (mstrf p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mergeable_functs"}, p), _) } ->
        mapper.structure_item mapper (mergf p) 
      | x -> default_mapper.structure_item mapper x);
  (* mapping the of_adt and to_adt functions *)
  expr = (fun mapper e ->
      match e with
      | { pexp_desc = Pexp_extension ({txt = "dali_of_adt"}, _)} -> ofa
      | { pexp_desc = Pexp_extension ({txt = "dali_to_adt"}, _)} -> toa
      | { pexp_desc = Pexp_extension ({txt = "dali_add_adt"}, _)} -> ada
      | { pexp_desc = Pexp_extension ({txt = "dali_read_adt"}, _)} -> rda
      | { pexp_desc = Pexp_extension ({txt = "dali_update_adt"}, _)} -> uadt
      | x -> default_mapper.expr mapper x);
}

(* It renames the module by adding an I to it. For example, Canvas module is Icanvas module in the transformed code *)
(* Here mn is the module name *)
(* tds is the list of type declarations *)
(* p is the structure where if it is a module then concatenate "I" in the module name *)
(* AST_helped.Str.module_ creates a module structure with module binding name field as I added to it *)
(* So basically this whole function changes the module name from Canvas to Icanvas *)
(* It is a arrow type of payload -> stucture_item *)
(* This is ICanvas *)
let dali_imodstr_rename tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ {x with pmb_name = mkaststr @@ "I" ^ mn }
  | _ -> assert false

let dali_get_atom_name tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] -> mn
  | _ -> assert false

(* It takes care of module type *)
(* Signature module *)
let dali_mmodsig_functs tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_modtype x}] ->
    Ast_helper.Str.modtype 
      { x with 
        (* Always the module type name is given in capital letter 
         For example Mcanvas of type MCANVAS *)
        pmtd_name = (mkaststr @@ "IRMIN_STORE_VALUE" ^ "_" ^ String.uppercase_ascii mn); 
        pmtd_type = (match x.pmtd_type with
            | Some typ ->
              let rec aux params =
                match params with
                | [] -> typ
                | ({ ptyp_desc = desc },_) :: r ->
                  (match desc with
                   | Ptyp_var s ->
                     let fcname = mkaststr @@ "IRMIN_STORE_VALUE" in
                     let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "Irmin.Type" in
                     Ast_helper.Mty.functor_ fcname (Some fcsig) (aux r)
                   | _ -> assert false) in 
              Some (aux td.ptype_params)
            | None -> assert false);
      }
  | _ -> assert false

(* Module structure *)
(* This is module MCanvas module of type MCANVAS *)
(* This is structure *)
let dali_mmodstr_functs tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ 
      { x with 
        pmb_name = (mkaststr @@ "M" ^ mn); 
        pmb_expr = 
          let rec aux params =
            match params with
            | [] -> x.pmb_expr
            | ({ ptyp_desc = desc },_) :: r ->
              (match desc with
               | Ptyp_var s ->
                 (* for example MCANVAS is the module type for MCanvas module *)
                 let fcname = mkaststr @@ String.uppercase_ascii s in
                 let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "Irmin.Type" in
                 Ast_helper.Mod.functor_ fcname (Some fcsig) (aux r)
               | _ -> assert false) in 
          let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid ("M" ^ String.uppercase_ascii mn) in
          Ast_helper.Mod.constraint_ (aux td.ptype_params) fcsig
      }
  | _ -> assert false

(* here p is a structure_item whose description denoted by ptr_desc field is matched against structure_item_desc *)
(* If the structure is a module then we create a structure which is a module structure with module binding *)
let dali_mergeable_functs tds td mn p =
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ 
      { x with 
        pmb_expr = 
          let rec aux params =
            match params with
            | [] -> x.pmb_expr
            | ({ ptyp_desc = desc },_) :: r ->
              (match desc with
               | Ptyp_var s ->
                 let fcname = mkaststr @@ String.uppercase_ascii s in
                 let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "MERGEABLE" in
                 Ast_helper.Mod.functor_ fcname (Some fcsig) (aux r)
               | _ -> assert false) in 
          aux td.ptype_params
      }
  | _ -> assert false

let dali_mmod_inst tds td mn =
  let mn = String.concat "." (Longident.flatten mn) in
  let rec aux params =
    match params with
    | [] -> Ast_helper.Mod.ident @@ Ast_convenience.lid ("M" ^ mn) 
    | ({ ptyp_desc = desc },_) :: r ->
      (match desc with
       | Ptyp_var s ->
         let fcid = Ast_helper.Mod.ident @@ Ast_convenience.lid (String.uppercase_ascii s) in
         Ast_helper.Mod.apply (aux r) fcid
       | _ -> assert false) in 
  aux td.ptype_params

let dali_bcsto_inst tds td mn =
  let rec aux params =
    match params with
    | [] -> Ast_helper.Mod.ident @@ Ast_convenience.lid ("BC_store") 
    | ({ ptyp_desc = desc },_) :: r ->
      (match desc with
       | Ptyp_var s ->
         let fcid = Ast_helper.Mod.ident @@ Ast_convenience.lid (String.uppercase_ascii s) in
         Ast_helper.Mod.apply (aux r) fcid
       | _ -> assert false) in 
  aux td.ptype_params

(* here mn is the module name and ident function creates a value expression for the module language *)
let dali_adt_mod mn =
  Ast_helper.Mod.ident (Location.mkloc mn !Ast_helper.default_loc)

(* here mn is the module name and creates the module expression functor *)
let dali_adt_mod_func mn =
 Ast_helper.Mod.ident {txt = (Longident.Lapply (mn, (Longident.Lident "Atom"))); loc= (!Ast_helper.default_loc)} 


let dali_adt_typesig tds td mn  =
  let open Ast_helper in
  let type_mapper { ptyp_desc = desc } =
    let open Typ in
    match desc with
    | Ptyp_var s -> constr (Ast_convenience.lid (String.uppercase_ascii s ^ ".t")) [] 
    | _ -> assert false in
  let map_fst f (x, y) = f x in
  let original_type_params = List.map (map_fst type_mapper) td.ptype_params  in
  let original_type_ident = Longident.Ldot (mn, td.ptype_name.txt) in
  Typ.constr (Location.mkloc original_type_ident !default_loc) original_type_params

  let get_core_list c' = match c' with
                               | Pcstr_tuple ct' -> ct'
                               | Pcstr_record rt' -> invalid_arg "PPx_dali.get_core_list" 


let dali_adt_typesig_func tds td mn  =
  let open Ast_helper in
  let type_mapper { ptyp_desc = desc } =
    let open Typ in
    match desc with
    | Ptyp_var s -> constr (Ast_convenience.lid (String.uppercase_ascii s ^ ".t")) [] 
    | _ -> assert false in
  let map_fst f (x, y) = f x in
  let original_type_params = List.map (map_fst type_mapper) td.ptype_params  in
  let original_type_ident = Longident.Ldot (Longident.(Lident "OM"), td.ptype_name.txt) in
  Typ.constr (Location.mkloc original_type_ident !default_loc) original_type_params


let dali_madt_typedef tds td mn =
  let open Ast_helper in
  let open Ast_helper.Typ in
  let main_td_mapper = {   
    default_mapper with
    typ = (fun mapper t ->
        (* Here t is the core type : type expression *)
        match t with

        | { ptyp_desc = Ptyp_var x } -> 
          default_mapper.typ mapper @@ constr (Ast_convenience.lid ("String.uppercase_ascii x" ^ ".t")) []
          (* This is the case where the type is suppose t *)
        | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident n}, x) } ->
         default_mapper.typ mapper @@ 
          if n = td.ptype_name.txt then constr (Ast_convenience.lid ("K.t")) []
          else  (match n with
          | "int" -> default_mapper.typ mapper @@ constr (Ast_convenience.lid ("int64")) []
          | "atom" -> default_mapper.typ mapper @@ constr (Ast_convenience.lid ("OM.atom")) []
          | _ -> t)
        | {ptyp_desc = Ptyp_constr ({txt = n}, x)} ->   
        let tn = String.concat "." (Longident.flatten n) in
        let prefix = (match tn with
          | "int" -> "int64"
          | "int32" -> "int32"
          | "int64" -> "int64"
          | "string" -> "string"
          | "char" -> "char"
          | "Atom.t" -> "Atom.t"
          | "atom" -> "OM.atom"
          | x -> x) in default_mapper.typ mapper @@ constr (Ast_convenience.lid (prefix)) []
        | x -> default_mapper.typ mapper x)} in 
  let type_dec_mapper = 
    {   
      default_mapper with
      type_declaration = (fun mapper t -> 
        (* t is the type declaration *)
          match t with
          | { ptype_attributes = [({txt = "derive"}, _) ]; } -> 
            (main_td_mapper.type_declaration main_td_mapper 
              { t with ptype_name = mkaststr "madt"; ptype_params = []; ptype_attributes = [] }) 
          | x -> default_mapper.type_declaration main_td_mapper x);
    } in 
  let madts = List.map (type_dec_mapper.type_declaration type_dec_mapper) (tds) in 
  ((Ast_helper.Str.type_ Recursive madts), madts) 



let dali_match_type tds td mn = 
  let open Ast_helper in 
  let open Ast_convenience in 
  let rec match_mapper ctd = 
     match ctd.ptype_kind with 
      | Ptype_record l -> List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l 
      | Ptype_variant l -> assert false
      | Ptype_abstract -> assert false 
      | Ptype_open -> assert false in 
      match_mapper td


let dali_update_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      let pat_mapper c =
        (*let name = mn ^ "." ^ c.pcd_name.txt in*)
        let plhs = pconstr c.pcd_name.txt 
                  (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) 
                    (get_core_list (c.pcd_args))) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] -> evar "Lwt.return()"
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "read_adt") 
                  [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") 
                         [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 (get_core_list (c.pcd_args)) [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> mn ^ "."^ fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = snd (fst x) in  
            (*let args' = fst (fst x) in*)
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
              let rr = List.map (fun e -> evar e.pld_name.txt) l in 
              (app (evar "List.fold_left") 
                (List.append [(lam (pvar "m") (lam (pvar "k") 
                                                   (app (evar ">>=") 
                                                         [evar "m" ; 
                                                         (Exp.fun_ Nolabel None (punit()) 
                                                          (constr "link_to_tree" [(evar "k")]))])))] 
                             [(evar "(Lwt.return())") ; list rr]))
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; 
                                     lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> ("t", e.pld_name.txt), e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract -> (match ctd.ptype_manifest with 
        | Some x -> 
          (match x.ptyp_desc with 
          | Ptyp_tuple l -> 
             let rec get_key_const l = 
              (match l with
               | [] -> []
               | a :: al -> 
                 (match a.ptyp_desc with 
                  | Ptyp_constr ({txt = Longident.Lident "t"}, []) -> (a :: get_key_const al)
                  | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                    if n = td.ptype_name.txt then 
                    a ::  (get_key_const al) else 
                    let st = find_some_type tds n in
                        (match st with
                         | Some p -> (a :: (get_key_const al))
                         | None -> (get_key_const al))
                  | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> 
                    (a :: (get_key_const al))
                  | Ptyp_constr ({txt = t}, x) -> (a :: get_key_const al)
                  | _ -> (get_key_const al))) in 
           let plhs = 
              let rec aux i l =        
               match l with 
               | [] -> (pvar "x")
               | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        if n = td.ptype_name.txt then
                        (ptuple @@ (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l)) else 
                        (pvar "x")                                                                        
                    | _ -> aux (i+1) al in 
                    aux 0 l in 
               let prhs = 
                let rec aux i l ls = 
                match l with 
                 | [] -> 
                  let a' = List.map (fun x -> evar x) (List.rev ls) in 
                  app (evar "@@") [evar "Lwt.return"; (tuple a')]
                 | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 let argn = mkarg i in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        let al' = List.mapi (fun i e -> evar (mkarg i)) (get_key_const l) in 
                        if n = td.ptype_name.txt then 
                        (app (evar "List.fold_left") 
                            (List.append [(lam (pvar "m") (lam (pvar "k") 
                                                   (app (evar ">>=") 
                                                         [evar "m" ; 
                                                         (Exp.fun_ Nolabel None (punit()) 
                                                          (constr "link_to_tree" [(evar "k")]))])))] 
                             [(evar "(Lwt.return())") ; list al'])) else 
                        let st = find_some_type tds n in
                        (match st with
                         | Some p ->
                           (app (evar "List.fold_left") 
                            (List.append [(lam (pvar "m") (lam (pvar "k") 
                                                   (app (evar ">>=") 
                                                         [evar "m" ; 
                                                         (Exp.fun_ Nolabel None (punit()) 
                                                          (constr "link_to_tree" [(evar "k")]))])))] 
                             [(evar "(Lwt.return())") ; list []]))
                         | None -> aux (i+1) al (argn :: ls))   
                    | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> 
                      aux (i+1) al (argn :: ls)                                                                    
                    | _ -> aux (i+1) al (argn :: ls) in 
                    aux 0 l [] in 
          
                    Exp.match_ (evar ak) [(Exp.case plhs prhs)]     
                    | Ptyp_constr ({txt = Longident.Lident "int"}, []) -> evar "Lwt.return ()" 
                    | Ptyp_constr ({txt = Longident.Lident "int32"}, []) -> evar "Lwt.return ()"
                    | Ptyp_constr ({txt = Longident.Lident "int64"}, []) -> evar "Lwt.return ()"
                    | Ptyp_constr ({txt = Longident.Lident "char"}, []) -> evar "Lwt.return ()"
                    | Ptyp_constr ({txt = Longident.Lident "string"}, []) -> evar "Lwt.return ()"    
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, [])}]) -> evar "Lwt.return ()" 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "char")}, [])}]) -> evar "Lwt.return ()" 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "string")}, [])}]) -> evar "Lwt.return ()"
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int64")}, [])}]) -> evar "Lwt.return ()"
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int32")}, [])}]) -> evar "Lwt.return ()"  
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int")}, [])}]) -> evar "Lwt.return ()"  
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_tuple l}]) -> evar "Lwt.return ()"  
                    | x -> evar "Lwt.return ()")
         | None -> assert false) 
    | Ptype_open -> assert false in
  kind_mapper td "v"


  
(* dali_add_adt *)
  (* It adds the adt and create the key *)
  let dali_add_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  (* here tn is a type name and l is the list of type declaration. *)
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      (* c is a constructor declaration present in the type declaration l *)
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let c' = c.pcd_args in 
        let plhs = pconstr name (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) (get_core_list c'))  in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr c.pcd_name.txt a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "add_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some p ->
                     app (evar ">>=") [kind_mapper p argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 (get_core_list (c.pcd_args)) [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
      (* here l is the list of label declaration *)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = snd (fst x) in
            let args' = fst (fst x) in 
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "add_adt") [evar args'; evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some p ->
                   app (evar ">>=") [kind_mapper p argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        (aux (List.map (fun e -> ("t", e.pld_name.txt), e.pld_type) l) []) in
        (* here ak is the record *)
        Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract -> 
      (match ctd.ptype_manifest with 
        | Some x -> 
          (match x.ptyp_desc with 
          | Ptyp_tuple l ->
             let rec check_core_type l = match l with 
              | [] -> true
              | x :: xl -> (match x.ptyp_desc with 
                             | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                               if n = td.ptype_name.txt then true 
                               else check_core_type xl
                             | _ -> false) in 
             if (check_core_type l) then let plhs = 
              let rec aux i l =        
               match l with 
               | [] -> (pvar "x")
               | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        if n = td.ptype_name.txt then
                        (ptuple @@ (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l)) else 
                        (pvar "x")                                                                        
                    | _ -> aux (i+1) al in 
                    aux 0 l in 
               let prhs = 
                let rec aux i l ls = 
                (*let mkarg x = "b" ^ string_of_int x in*)
                match l with 
                 | [] -> 
                  let a' = List.map (fun x -> evar x) (List.rev ls) in 
                  app (evar "@@") [evar "Lwt.return"; (tuple a')]
                 | a :: al ->  
                 let mkarg x = "b" ^ string_of_int x in 
                 let mkparg x = mkp @@ mkarg x in 
                 let argn = mkarg i in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        let argnp = mkparg i in  
                        if n = td.ptype_name.txt then
                        (app (evar ">>=") 
                             [app (evar "add_adt") 
                                  [evar "t"; evar argn]; 
                                   lam (pvar argnp) (aux (i+1) al (argnp :: ls))]) else 
                        let st = find_some_type tds n in
                        (match st with
                         | Some p ->
                           app (evar ">>=") [kind_mapper p argn; 
                                             lam (pvar argnp) (aux (i+1) al (argnp :: ls))]
                         | None -> aux (i+1) al (argn :: ls))   
                    | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> aux (i+1) al (argn :: ls)                                                                    
                    | _ -> aux (i+1) al (argn :: ls) in 
                    aux 0 l [] in 
                    Exp.match_ (evar ak) [(Exp.case plhs prhs)]
                    else  app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "int"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; constr "Int64.of_int" [(evar "a")]] 
                    | Ptyp_constr ({txt = Longident.Lident "int32"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "int64"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
                    | Ptyp_constr ({txt = Longident.Lident "string"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "char"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]    
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, [])}]) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "char")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "string")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int32")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int64")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_tuple l}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]           
                    | x -> app (evar "@@") [evar "Lwt.return" ; (evar "a")] )
         | None -> assert false) 
    | Ptype_open -> assert false in
  kind_mapper td "a"


    (* dali_read_adt *)
  (* It read the keys in the store *)
let dali_read_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let plhs = pconstr c.pcd_name.txt (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) (get_core_list (c.pcd_args))) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr name a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "read_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 (get_core_list (c.pcd_args)) [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> mn ^ "."^ fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = snd (fst x) in  
            let args' = fst (fst x) in
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "read_adt") [evar args'; evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> ("t", e.pld_name.txt), e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract -> (match ctd.ptype_manifest with 
        | Some x -> 
          (match x.ptyp_desc with 
          | Ptyp_tuple l ->
             let rec check_core_type l = match l with 
              | [] -> true
              | x :: xl -> (match x.ptyp_desc with 
                             | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                               if n = td.ptype_name.txt then true 
                               else check_core_type xl
                             | _ -> false) in 
             if (check_core_type l) then 
             let plhs = 
              let rec aux i l =        
               match l with 
               | [] -> (pvar "x")
               | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        if n = td.ptype_name.txt then
                        (ptuple @@ (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l)) else 
                        (pvar "x")                                                                        
                    | _ -> aux (i+1) al in 
                    aux 0 l in 
               let prhs = 
                let rec aux i l ls = 
                match l with 
                 | [] -> 
                  let a' = List.map (fun x -> evar x) (List.rev ls) in 
                  app (evar "@@") [evar "Lwt.return"; (tuple a')]
                 | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 let mkparg x = mkp @@ mkarg x in 
                 let argn = mkarg i in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        let argnp = mkparg i in  
                        if n = td.ptype_name.txt then
                        (app (evar ">>=") 
                             [app (evar "read_adt") 
                                  [evar "t"; evar argn]; 
                                   lam (pvar argnp) (aux (i+1) al (argnp :: ls))]) else 
                        let st = find_some_type tds n in
                        (match st with
                         | Some p ->
                           app (evar ">>=") [kind_mapper p argn; 
                                             lam (pvar argnp) (aux (i+1) al (argnp :: ls))]
                         | None -> aux (i+1) al (argn :: ls))   
                    | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> aux (i+1) al (argn :: ls)                                                                    
                    | _ -> aux (i+1) al (argn :: ls) in 
                    aux 0 l [] in 
                    Exp.match_ (evar ak) [(Exp.case plhs prhs)]  
                    else app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "int"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; constr "Int64.of_int" [(evar "a")]] 
                    | Ptyp_constr ({txt = Longident.Lident "int32"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "int64"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, [])}]) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "char")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "string")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int32")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int64")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int")}, [])}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                       [{ptyp_desc = Ptyp_tuple l}]) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
                    | Ptyp_constr ({txt = Longident.Lident "char"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
                    | Ptyp_constr ({txt = Longident.Lident "string"}, []) -> 
                       app (evar "@@") [evar "Lwt.return" ; (evar "a")]              
                    | x -> app (evar "@@") [evar "Lwt.return" ; (evar "a")])
         | None -> assert false) 
    | Ptype_open -> assert false in
  kind_mapper td "a"



let dali_of_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      (* c is a constructor declaration present in the type declaration l *)
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let c' = c.pcd_args in 
        let plhs = pconstr name (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) (get_core_list c'))  in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr c.pcd_name.txt a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "aostore_add") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 (get_core_list (c.pcd_args)) [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = fst x in  
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "aostore_add") [evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> e.pld_name.txt, e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract ->  (match ctd.ptype_manifest with 
        | Some x -> 
          (match x.ptyp_desc with 
          | Ptyp_tuple l -> let rec check_core_type l = match l with 
              | [] -> true
              | x :: xl -> (match x.ptyp_desc with 
                             | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                               if n = td.ptype_name.txt then true 
                               else check_core_type xl
                             | _ -> false) in 
             if (check_core_type l) then 
             let plhs = 
              let rec aux i l =        
               match l with 
               | [] -> (pvar "x")
               | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        if n = td.ptype_name.txt then
                        (ptuple @@ (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l)) else 
                        (pvar "x")                                                                        
                    | _ -> aux (i+1) al in 
                    aux 0 l in 
               let prhs = 
                let rec aux i l ls = 
                (*let mkarg x = "b" ^ string_of_int x in*)
                match l with 
                 | [] -> 
                  let a' = List.map (fun x -> evar x) (List.rev ls) in 
                  app (evar "@@") [evar "Lwt.return"; (tuple a')]
                 | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 let mkparg x = mkp @@ mkarg x in 
                 let argn = mkarg i in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        let argnp = mkparg i in  
                        if n = td.ptype_name.txt then
                        (app (evar ">>=") 
                             [app (evar "aostore_add") 
                                  [evar argn]; 
                                   lam (pvar argnp) (aux (i+1) al (argnp :: ls))]) else 
                        let st = find_some_type tds n in
                        (match st with
                         | Some p ->
                           app (evar ">>=") [kind_mapper p argn; 
                                             lam (pvar argnp) (aux (i+1) al (argnp :: ls))]
                         | None -> aux (i+1) al (argn :: ls))   
                    | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> aux (i+1) al (argn :: ls)                                                                    
                    | _ -> aux (i+1) al (argn :: ls) in 
                    aux 0 l [] in 
                    Exp.match_ (evar ak) [(Exp.case plhs prhs)] 
                    else app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
        | Ptyp_constr ({txt = Longident.Lident "int"}, []) -> 
          app (evar "@@") [evar "Lwt.return" ; constr "Int64.of_int" [(evar "a")]] 
        | Ptyp_constr ({txt = Longident.Lident "int64"}, []) -> 
          app (evar "@@") [evar "Lwt.return" ; (evar "a")]
        | Ptyp_constr ({txt = Longident.Lident "int32"}, []) -> 
          app (evar "@@") [evar "Lwt.return" ; (evar "a")]
        | Ptyp_constr ({txt = Longident.Lident "char"}, []) -> 
          app (evar "@@") [evar "Lwt.return" ; (evar "a")]
        | Ptyp_constr ({txt = Longident.Lident "string"}, []) -> 
          app (evar "@@") [evar "Lwt.return" ; (evar "a")]
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "char")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")]
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "string")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int32")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")]  
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int64")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int")}, [])}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")] 
        | Ptyp_constr ({txt = Longident.Lident "list"},
           [{ptyp_desc = Ptyp_tuple l}]) -> 
           app (evar "@@") [evar "Lwt.return" ; (evar "a")]   
        | x -> app (evar "@@") [evar "Lwt.return" ; (evar "a")])              
        | None -> assert false) 
    | Ptype_open -> assert false in
  kind_mapper td "a"

let dali_to_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let plhs = pconstr c.pcd_name.txt (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) (get_core_list (c.pcd_args))) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr name a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "aostore_read") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 (get_core_list (c.pcd_args)) [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> mn ^ "."^ fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = fst x in  
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "aostore_read") [evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> e.pld_name.txt, e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract -> (match ctd.ptype_manifest with 
        | Some x -> 
          (match x.ptyp_desc with 
          | Ptyp_tuple l -> 
             let rec check_core_type l = match l with 
              | [] -> true
              | x :: xl -> (match x.ptyp_desc with 
                             | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                               if n = td.ptype_name.txt then true 
                               else check_core_type xl
                             | _ -> false) in 
             if (check_core_type l) then 
             let plhs = 
              let rec aux i l =        
               match l with 
               | [] -> (pvar "x")
               | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        if n = td.ptype_name.txt then
                        (ptuple @@ (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l)) else 
                        (pvar "x")                                                                        
                    | _ -> aux (i+1) al in 
                    aux 0 l in 
               let prhs = 
                let rec aux i l ls = 
                (*let mkarg x = "b" ^ string_of_int x in*)
                match l with 
                 | [] -> 
                  let a' = List.map (fun x -> evar x) (List.rev ls) in 
                  app (evar "@@") [evar "Lwt.return"; (tuple a')]
                 | a :: al -> 
                 let mkarg x = "b" ^ string_of_int x in 
                 let mkparg x = mkp @@ mkarg x in 
                 let argn = mkarg i in 
                 match a.ptyp_desc with 
                    | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                        let argnp = mkparg i in  
                        if n = td.ptype_name.txt then
                        (app (evar ">>=") 
                             [app (evar "aostore_read") 
                                  [evar argn]; 
                                   lam (pvar argnp) (aux (i+1) al (argnp :: ls))]) else 
                        let st = find_some_type tds n in
                        (match st with
                         | Some p ->
                           app (evar ">>=") [kind_mapper p argn; 
                                             lam (pvar argnp) (aux (i+1) al (argnp :: ls))]
                         | None -> aux (i+1) al (argn :: ls))   
                    | Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, []) -> aux (i+1) al (argn :: ls)                                                                    
                    | _ -> aux (i+1) al (argn :: ls) in 
                    aux 0 l [] in 
                    Exp.match_ (evar ak) [(Exp.case plhs prhs)] else
                    app (evar "@@") [evar "Lwt.return" ; (evar "t")]
                    | Ptyp_constr ({txt = Longident.Lident "int"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; constr "Int64.to_int" [(evar "a")]]
                    | Ptyp_constr ({txt = Longident.Lident "int32"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "t")]
                    | Ptyp_constr ({txt = Longident.Lident "int64"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "t")]
                    | Ptyp_constr ({txt = Longident.Lident "char"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "t")]
                    | Ptyp_constr ({txt = Longident.Lident "string"}, []) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "t")]
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                      [{ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Longident.Lident "Atom", "t")}, [])}]) -> 
                      app (evar "@@") [evar "Lwt.return" ; (evar "t")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "char")}, [])}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")]  
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "string")}, [])}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int32")}, [])}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int64")}, [])}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_constr ({txt = (Longident.Lident "int")}, [])}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")] 
                    | Ptyp_constr ({txt = Longident.Lident "list"},
                     [{ptyp_desc = Ptyp_tuple l}]) -> 
                     app (evar "@@") [evar "Lwt.return" ; (evar "t")]                 
                    | x -> app (evar "@@") [evar "Lwt.return" ; (evar "a")])
         | None -> assert false) 
    | Ptype_open -> assert false in
  kind_mapper td "t"

(* dali_irmin_convert returns a structure item *)
(* here tds are the type declarations an madt is the final type made from those, basically both constitute 
   the available types *)
(* Here the module name is IrminConvert and it is a structure *)
(*let dali_irmin_convert madt tds =
  let open Ast_helper in
  let to_irmin_stris = List.map Derive_irmin_type.derive_to_irmin_type_value [tds; madt] in
  Str.module_ @@ Mb.mk (mkaststr "IrminConvert") (Mod.structure to_irmin_stris)*)

(* dali_json_convert returns a structure item *)
let dali_irmin_convert madt tds =
  let open Ast_helper in
  let to_irmin = List.map Derive_irmin_type_with_atom.derive_to_irmin [tds; madt] in
  Str.module_ @@ Mb.mk (mkaststr "IrminConvert") (Mod.structure to_irmin)


(* dali_json_convert returns a structure item *)
let dali_irmin_tie_convert madt tds =
  let open Ast_helper in
  let to_irmin_tie = List.map Derive_irmin_type_with_atom.derive_to_irmin_tie [tds; madt] in
  Str.module_ @@ Mb.mk (mkaststr "IrminConvertTie") (Mod.structure to_irmin_tie)

(* mn is module type name *)
(* tds is the list of type declarations *)
(* td is a type declaration *)
(* p is the structure_item *)
(* adt_mod is module_expr type *)
(* adt_typesig is a core_type type *)
(* madt_typedef is a structure_item type and madt is the list of type declarations  *)
(* irmin_convert is a structure_item *)
(* of_adt is expression type *)
(* to_adt is expression type *)
(* imodstr_rename is payload -> structure_item type *)
(* mmodsig_functs is payload -> structure_item type *)
(* mmodstr_functs is payload -> structure_item type *)
(* mergeable_functs is payload -> structure_item type *)
(* mmod_inst is module_expr type *)
(* bcsto_inst is a module_expr type *)
let dali_derive tds td dts mn =
   (*let template = "" in *)
  let template = [%blob "ppx/new_dali_template_with_atom.ml"] in
  let adt_mod = dali_adt_mod mn in
  let adt_mod_func = dali_adt_mod_func mn in
  let adt_typesig = dali_adt_typesig tds td mn in
  let adt_typesig_func = dali_adt_typesig_func tds td mn in
  let madt_typedef, madt = dali_madt_typedef tds td mn in
  let irmin_convert = dali_irmin_convert madt dts in
  let irmin_convert_tie = dali_irmin_tie_convert madt dts in
  let of_adt = dali_of_adt tds td mn in
  let to_adt = dali_to_adt tds td mn in
  let add_adt = dali_add_adt tds td mn in 
  let read_adt = dali_read_adt tds td mn in
  let up_adt = dali_update_adt tds td mn in 
  let imodstr_rename = dali_imodstr_rename tds td mn   in
  let mmodsig_functs = dali_mmodsig_functs tds td mn  in
  let mmodstr_functs = dali_mmodstr_functs tds td mn  in
  let mergeable_functs = dali_mergeable_functs tds td mn  in
  let mmod_inst = dali_mmod_inst tds td mn in
  let bcsto_inst = dali_bcsto_inst tds td mn in
  let dali_mapper = mk_dali_mapper ( adt_mod, adt_mod_func, adt_typesig, adt_typesig_func, 
      madt_typedef, irmin_convert, irmin_convert_tie, of_adt, to_adt, add_adt, read_adt, up_adt,
      imodstr_rename, mmodsig_functs, mmodstr_functs, mergeable_functs, mmod_inst, bcsto_inst) in 
  dali_mapper.structure dali_mapper (Parse.implementation @@ Lexing.from_string template)

let using_dali_mapper argv = 
  let mk_dali_derive_mapper mname = {
    default_mapper with 
    structure = fun mapper s ->
      let rec aux s dt = match s with
        | [] -> raise_errorf "[dalify]: Unavailable dalify-ing type." 
        | { pstr_desc = Pstr_type (Recursive, tds) } :: r ->
          let rec do_dalify td = match td with
            | [] -> aux r dt
            | { ptype_attributes = [({txt = "derive"}, p) ]; } as td :: _ ->
              (match p with
               | PStr [{pstr_desc= Pstr_eval({pexp_desc = Pexp_ident {txt = Longident.Lident "versioned"}}, _); _}] ->
                 dali_derive tds td (List.rev dt) mname 
               | PStr [{pstr_desc= Pstr_eval({pexp_desc = Pexp_ident {txt = Longident.Lident "ezjsonm"}}, _); _}] ->
                 aux r (td :: dt)
               | _ -> assert false)
            | x :: r -> do_dalify r in
          do_dalify tds
        | x :: r -> aux r dt in
      default_mapper.structure mapper (aux s [])
  } in
  { default_mapper with 
    structure = fun mapper s ->
      let rec aux s = match s with
        | [] -> []
        | { pstr_desc = Pstr_module 
                {pmb_name; pmb_expr; 
                 pmb_attributes = [({txt = "derive_versioned"}, _) ];} 
          } :: r ->
          let mname = Longident.Lident pmb_name.txt in
          (match pmb_expr.pmod_desc with
           | Pmod_structure x ->
             let dali_derive_mapper = mk_dali_derive_mapper mname in
             s @ (dali_derive_mapper.structure mapper x)
            | Pmod_functor ({txt = "Atom"},Some {pmty_desc = Pmty_ident {txt = (Longident.Lident "ATOM")}}, 
              {pmod_desc = (Pmod_structure x); pmod_loc = _; pmod_attributes = []}) ->   
            (let dali_derive_mapper = mk_dali_derive_mapper mname in
             s @ (dali_derive_mapper.structure mapper x))
            | Pmod_functor ({txt = "Item"},Some {pmty_desc = Pmty_ident {txt = (Longident.Lident "Item")}}, 
              {pmod_desc = (Pmod_structure x); pmod_loc = _; pmod_attributes = []}) ->   
            (let dali_derive_mapper = mk_dali_derive_mapper mname in
             s @ (dali_derive_mapper.structure mapper x))
           | _p -> raise_errorf "[dalify]: Invalid module binding.") (**NEED TO CHANGE HERE**)
        | x :: r -> x :: (aux r) in
      default_mapper.structure mapper (aux s)
  }

let () = register "ppx_dali" using_dali_mapper