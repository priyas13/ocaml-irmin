   let mktarg t =
              let open Irmin.Type in
                pair t
                  (pair int t)