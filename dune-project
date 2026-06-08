(lang dune 3.16)

(using melange 0.1)

(name melange-edn)
(version 0.3.0)

(generate_opam_files true)

(package
 (name melange-edn)
 (synopsis "EDN reader and writer for Melange")
 (description
 "melange-edn parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and JavaScript JSON values.")
 (authors "rcmerci")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (license MIT)
 (source
  (github RCmerci/melange-edn))
 (homepage "https://github.com/RCmerci/melange-edn")
 (bug_reports "https://github.com/RCmerci/melange-edn/issues")
 (depends
  (ocaml  (>= 5.4))
  melange
  (melange-jest :with-test)))
