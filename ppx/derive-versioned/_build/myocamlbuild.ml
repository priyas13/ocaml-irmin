open Ocamlbuild_plugin

let () = dispatch (fun phase ->
  match phase with
  | After_rules ->
      (* For building Dali *)
      dep ["file:ppx/ppx_dali.ml"] ["ppx/dali_template.ml"];

      (* Sample usage in test *)
      flag ["ocaml"; "compile"; "use_dali"] & S [A "-ppx"; A "ppx/ppx_dali.native"];
  | _ -> ())
