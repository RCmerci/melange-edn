# melange-edn

EDN reader and writer for OCaml, js_of_ocaml, and Melange.

`melange-edn` parses EDN strings into a typed OCaml value model, writes values
back to EDN, and converts between EDN values and JSON values across native,
js_of_ocaml, and Melange targets.

## Packages

- `melange-edn-core`: shared EDN value model, parser, and writer.
- `melange-edn-native`: native OCaml package with Yojson conversion.
- `melange-edn-jsoo`: js_of_ocaml package with JavaScript JSON conversion.
- `melange-edn-melange`: Melange package with `Js.Json.t` conversion.

Each platform package depends on `melange-edn-core` and exposes the core EDN
API plus the platform-specific JSON bridge from its main module, so installing
one bridge package does not pull in the other bridge dependencies.

## Features

- Parse one EDN form with `of_edn_string`.
- Parse multiple EDN forms with `of_edn_string_all`.
- Write EDN with `to_edn_string`.
- Convert JSON values with `of_json`, `of_json_string`, `to_json`, and
  `to_json_string` in the package's main module.
- Supports EDN atoms, collections, comments, discard forms, tagged values,
  namespaced maps, symbolic floating-point values, octal escapes, ratios, regex
  literals, common reader macros, and `#uuid` / `#inst` default tags.

## Usage

```ocaml
open Melange_edn_native

let edn =
  of_edn_string {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}

let edn_text =
  to_edn_string edn

let json_text =
  to_json_string edn

let from_json =
  of_json_string {|{"name":"Ada","tags":["ocaml",null],"ok":true}|}
```

Use the backend packages when working with JavaScript JSON values:

```ocaml
open Melange_edn_melange

let melange_json =
  to_json (of_edn_string {|{:name "Ada"}|})
```

```ocaml
open Melange_edn_jsoo

let jsoo_json =
  to_json (of_edn_string {|{:name "Ada"}|})
```

Construct values with the typed creation functions:

```ocaml
open Melange_edn_native

let value =
  any
    (map
       [
         (any (keyword "name"), any (string "Ada"));
         (any (keyword "ok"), any (bool true));
       ])

let text =
  to_edn_string value
```

## Development

Run the test suite:

```sh
dune runtest
```

## License

MIT
