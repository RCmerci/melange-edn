# melange-edn

EDN reader and writer for OCaml, js_of_ocaml, and Melange.

`melange-edn` parses EDN strings into a typed OCaml value model, writes values
back to EDN, and converts between EDN values and JSON values across native,
js_of_ocaml, and Melange targets.

## Libraries

- `melange-edn`: pure EDN value model, parser, and writer.
- `melange-edn.yojson`: Yojson conversion and JSON string parser/writer.
- `melange-edn.melange`: Melange `Js.Json.t` bridge.
- `melange-edn.jsoo`: js_of_ocaml JavaScript JSON value bridge.

## Features

- Parse one EDN form with `Melange_edn.of_edn_string`.
- Parse multiple EDN forms with `Melange_edn.of_edn_string_all`.
- Write EDN with `Melange_edn.to_edn_string`.
- Convert Yojson values with `Melange_edn_yojson.of_json`,
  `Melange_edn_yojson.of_json_string`, `Melange_edn_yojson.to_json`, and
  `Melange_edn_yojson.to_json_string`.
- Convert Melange `Js.Json.t` with `Melange_edn_melange`.
- Convert js_of_ocaml JavaScript JSON values with `Melange_edn_jsoo`.
- Supports EDN atoms, collections, comments, discard forms, tagged values,
  namespaced maps, symbolic floating-point values, octal escapes, ratios, regex
  literals, common reader macros, and `#uuid` / `#inst` default tags.

## Usage

```ocaml
open Melange_edn

let edn =
  of_edn_string {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}

let edn_text =
  to_edn_string edn
```

Use the Yojson library on native:

```ocaml
let json_text =
  Melange_edn_yojson.to_json_string edn

let from_json =
  Melange_edn_yojson.of_json_string
    {|{"name":"Ada","tags":["ocaml",null],"ok":true}|}
```

Use the backend bridges when working with JavaScript JSON values:

```ocaml
let melange_json =
  Melange_edn_melange.to_json edn

let jsoo_json =
  Melange_edn_jsoo.to_json edn
```

Construct values with the typed creation functions:

```ocaml
open Melange_edn

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
