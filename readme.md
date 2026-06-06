# melange-edn

EDN reader and writer for Melange.

`melange-edn` parses EDN strings into a typed OCaml value model, writes values back to EDN, and converts between EDN values and `Js.Json.t` / JSON strings.

## Features

- Parse one EDN form with `of_edn_string`
- Parse multiple EDN forms with `of_edn_string_all`
- Write EDN with `to_edn_string`
- Convert JSON with `of_json`, `of_json_string`, `to_json`, and `to_json_string`
- Supports EDN atoms, collections, comments, discard forms, and tagged values

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

Construct values directly with the typed constructors:

```ocaml
open Melange_edn

let value =
  Any
    (Map
       (Iarray.of_list
          [
            (Any (Keyword "name"), Any (String "Ada"));
            (Any (Keyword "ok"), Any (Bool true));
          ]))

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
