# melange-edn

EDN reader and writer for Melange.

`melange-edn` parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and `Js.Json.t` / JSON strings.

## Features

- Parse one EDN form with `of_edn_string`
- Parse multiple EDN forms with `of_edn_string_all`
- Write EDN with `to_edn_string`
- Convert JSON with `of_json`, `of_json_string`, `to_json`, and `to_json_string`
- Supports EDN atoms, collections, comments, discard forms, and tagged values
- Follows `cljs.reader/read-string` by reading empty or comment-only input as `nil`
- Supports namespaced maps, symbolic floating-point values, and octal escapes
- Reads the first form from a string, and supports ratios, regex literals, and common reader macros
- Validates `#uuid` and `#inst` default tags

## Usage

```ocaml
open Melange_edn

let edn =
  of_edn_string {|{:name "Ada" :tags ["ocaml" nil] :ok true}|}

let edn_text =
  to_edn_string edn

let json_text =
  to_json_string edn
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

Install JavaScript dependencies:

```sh
npm install
```

Run the test suite:

```sh
dune runtest
```

## License

MIT
