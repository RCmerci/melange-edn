module Accept (Edn : Melange_edn.S) = struct
  let render_sample () =
    Edn.to_edn_string
      (Edn.any
         (Edn.vector
            [
              Edn.any (Edn.keyword "ok");
              Edn.any (Edn.bool true);
              Edn.any (Edn.int 42L);
            ]))
end

module Melange = Accept (Melange_edn_melange)

let () =
  Test_support.assert_string "Melange backend satisfies Melange_edn.S"
    "[:ok true 42]" (Melange.render_sample ())
