(lang dune 3.16)

(using melange 0.1)

(name melange_edn)

(generate_opam_files true)

(package
 (name melange_edn)
 (synopsis "EDN reader and writer for melange")
 (depends
  (ocaml (>= 5.0))
  melange
  (melange-jest :with-test)))
