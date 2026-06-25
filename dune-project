(lang dune 3.16)

(using melange 0.1)

(name melange-edn)
(version 0.5.0)

(generate_opam_files true)

(package
 (name melange-edn)
 (synopsis "EDN reader and writer for OCaml, js_of_ocaml, and Melange")
 (description
 "melange-edn parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and JSON values on native, js_of_ocaml, and Melange.")
 (authors "rcmerci")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (license MIT)
 (source
  (github RCmerci/melange-edn))
 (homepage "https://github.com/RCmerci/melange-edn")
 (bug_reports "https://github.com/RCmerci/melange-edn/issues")
 (depends
  (ocaml  (>= 5.2))
  (melange (>= 5.0.0-52))
  (js_of_ocaml (>= 5.7.0))
  (yojson (and (>= 2.2.2) (< 3.0.0)))
  (alcotest (and :with-test (>= 1.5.0)))))
