(lang dune 3.16)

(using melange 0.1)

(name melange-edn)
(version 0.5.0)

(generate_opam_files true)

(package
 (name melange-edn-core)
 (synopsis "Shared EDN value model, parser, and writer")
 (description
 "melange-edn-core parses EDN strings into a typed OCaml value model and writes values back to EDN for reuse by native OCaml, js_of_ocaml, and Melange packages.")
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
  (alcotest (and :with-test (>= 1.5.0)))))

(package
 (name melange-edn-native)
 (synopsis "EDN reader and writer for native OCaml")
 (description
 "melange-edn-native parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and Yojson values on native OCaml.")
 (authors "rcmerci")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (license MIT)
 (source
  (github RCmerci/melange-edn))
 (homepage "https://github.com/RCmerci/melange-edn")
 (bug_reports "https://github.com/RCmerci/melange-edn/issues")
 (depends
  (ocaml  (>= 5.2))
  (melange-edn-core (= :version))
  (yojson (>= 3.0.0))
  (alcotest (and :with-test (>= 1.5.0)))))

(package
 (name melange-edn-jsoo)
 (synopsis "EDN reader and writer for js_of_ocaml")
 (description
 "melange-edn-jsoo parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and JavaScript JSON values with js_of_ocaml.")
 (authors "rcmerci")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (license MIT)
 (source
  (github RCmerci/melange-edn))
 (homepage "https://github.com/RCmerci/melange-edn")
 (bug_reports "https://github.com/RCmerci/melange-edn/issues")
 (depends
  (ocaml  (>= 5.2))
  (melange-edn-core (= :version))
  (js_of_ocaml (>= 5.7.0))
  (alcotest (and :with-test (>= 1.5.0)))))

(package
 (name melange-edn-melange)
 (synopsis "EDN reader and writer for Melange")
 (description
 "melange-edn-melange parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and Js.Json.t values with Melange.")
 (authors "rcmerci")
 (maintainers "rcmerci <rcmerci@gmail.com>")
 (license MIT)
 (source
  (github RCmerci/melange-edn))
 (homepage "https://github.com/RCmerci/melange-edn")
 (bug_reports "https://github.com/RCmerci/melange-edn/issues")
 (depends
  (ocaml  (>= 5.2))
  (melange-edn-core (= :version))
  (melange (>= 5.0.0-52))
  (alcotest (and :with-test (>= 1.5.0)))))
