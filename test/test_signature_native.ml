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

module Native = Accept (Melange_edn_native)

let () =
  Test_support.assert_string "native backend satisfies Melange_edn.S"
    "[:ok true 42]" (Native.render_sample ())
