open Js_of_ocaml
module Edn = Melange_edn

type json = Js.Unsafe.any

let stringify (value : json) : Js.js_string Js.t =
  Js.Unsafe.meth_call Js._JSON "stringify" [| Js.Unsafe.inject value |]

let parse source : json =
  Js.Unsafe.meth_call Js._JSON "parse" [| Js.Unsafe.inject source |]

let is_null value : bool =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr "(function(value) { return value === null; })")
    [| Js.Unsafe.inject value |]

let is_array value : bool =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr "(function(value) { return Array.isArray(value); })")
    [| Js.Unsafe.inject value |]

let bool_value value : bool =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr "(function(value) { return value; })")
    [| Js.Unsafe.inject value |]

let number_value value : float =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr "(function(value) { return value; })")
    [| Js.Unsafe.inject value |]

let string_value value : Js.js_string Js.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr "(function(value) { return value; })")
    [| Js.Unsafe.inject value |]

let min_safe_json_integer = -9007199254740991.
let max_safe_json_integer = 9007199254740991.

let is_safe_json_integer value =
  value >= min_safe_json_integer
  && value <= max_safe_json_integer
  && value = floor value

let int64_is_safe_json_integer value =
  value >= Int64.of_float min_safe_json_integer
  && value <= Int64.of_float max_safe_json_integer

let edn_number_of_json_number value =
  if is_safe_json_integer value then Edn.any (Edn.int (Int64.of_float value))
  else Edn.any (Edn.float value)

let js_type value = Js.to_string (Js.typeof (Js.Unsafe.coerce value))

let rec of_json value =
  if is_null value then Edn.any Edn.nil
  else
    match js_type value with
    | "boolean" -> Edn.any (Edn.bool (bool_value value))
    | "number" -> edn_number_of_json_number (number_value value)
    | "string" -> Edn.any (Edn.string (Js.to_string (string_value value)))
    | "object" when is_array value -> of_json_array value
    | "object" -> of_json_object value
    | kind -> invalid_arg ("JSON value has unsupported JavaScript type: " ^ kind)

and of_json_array value =
  let values : json Js.js_array Js.t = Js.Unsafe.coerce value in
  let length : int = Js.Unsafe.get values "length" in
  Edn.any
    (Edn.vector
       (List.init length (fun index ->
            of_json
              (Js.Optdef.get (Js.array_get values index) (fun () ->
                   invalid_arg "JSON array contains a hole")))))

and of_json_object value =
  let object_ : < .. > Js.t = Js.Unsafe.coerce value in
  let keys = Js.object_keys object_ in
  let length : int = Js.Unsafe.get keys "length" in
  Edn.any
    (Edn.map
       (List.init length (fun index ->
            let key =
              Js.to_string
                (Js.Optdef.get (Js.array_get keys index) (fun () ->
                     invalid_arg "Object.keys returned a sparse array"))
            in
            (Edn.any (Edn.string key), of_json (Js.Unsafe.get value key)))))

let of_json_string source = of_json (parse (Js.string source))

let json_key_of_value (Edn.Any value) =
  match value with
  | Edn.String value -> value
  | Edn.Keyword value -> Edn.keyword_to_string value
  | Edn.Symbol value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let json_number_of_int64 value =
  if int64_is_safe_json_integer value then
    Js.Unsafe.inject (Int64.to_float value)
  else Js.Unsafe.inject (Js.string (Int64.to_string value))

let rec json_array values =
  Js.Unsafe.inject
    (Js.array
       (Array.init (Array.length values) (fun index ->
            to_json (Array.get values index))))

and json_object entries =
  Js.Unsafe.inject
    (Js.Unsafe.obj
       (Array.map
          (fun (key, value) ->
            (json_key_of_value key, Js.Unsafe.inject (to_json value)))
          entries))

and to_json (Edn.Any value) =
  match value with
  | Edn.Nil -> Js.Unsafe.inject Js.null
  | Edn.Bool value -> Js.Unsafe.inject value
  | Edn.String value -> Js.Unsafe.inject (Js.string value)
  | Edn.Char value ->
      Js.Unsafe.inject
        (Js.string (Edn.to_edn_string (Edn.any (Edn.char value))))
  | Edn.Symbol value -> Js.Unsafe.inject (Js.string value)
  | Edn.Keyword value ->
      Js.Unsafe.inject (Js.string (":" ^ Edn.keyword_to_string value))
  | Edn.Int value -> json_number_of_int64 value
  | Edn.Bigint value -> Js.Unsafe.inject (Js.string value)
  | Edn.Float value -> Js.Unsafe.inject value
  | Edn.Decimal value -> Js.Unsafe.inject (Js.string value)
  | Edn.Ratio value -> Js.Unsafe.inject (Js.string value)
  | Edn.Regex value -> Js.Unsafe.inject (Js.string value)
  | Edn.List values | Edn.Vector values | Edn.Set values -> json_array values
  | Edn.Map entries -> json_object entries
  | Edn.Tagged (tag, value) ->
      json_object
        [|
          (Edn.any (Edn.string "tag"), Edn.any (Edn.string tag));
          (Edn.any (Edn.string "value"), value);
        |]

let to_json_string value = Js.to_string (stringify (to_json value))
