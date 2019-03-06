open Ocamlbuild_plugin

let () = dispatch (fun phase ->
  match phase with
  | After_rules ->
      (* For building Dali *)
      dep ["file:ppx/new_ppx_dali.ml"] ["ppx/new_dali_template.ml"];
      dep ["file:ppx/new_ppx_dali_with_atom.ml"] ["ppx/new_dali_template_with_atom.ml"];

      (* Sample usage in test *)
      flag ["ocaml"; "compile"; "use_dali"] & S [A "-ppx"; A "ppx/new_ppx_dali.native"];
  | _ -> ())
