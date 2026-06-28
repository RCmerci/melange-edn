include Melange_edn

module Edn = Melange_edn

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

let json_string_of_non_finite_float value =
  match classify_float value with
  | FP_nan -> Some "NaN"
  | FP_infinite when value > 0. -> Some "Infinity"
  | FP_infinite -> Some "-Infinity"
  | FP_normal | FP_subnormal | FP_zero -> None

let rec of_json json =
  match Js.Json.classify json with
  | JSONNull -> Edn.any Edn.nil
  | JSONFalse -> Edn.any (Edn.bool false)
  | JSONTrue -> Edn.any (Edn.bool true)
  | JSONString value -> Edn.any (Edn.string value)
  | JSONNumber value -> edn_number_of_json_number value
  | JSONArray values ->
      Edn.any
        (Edn.vector
           (List.init (Array.length values) (fun index ->
                of_json values.(index))))
  | JSONObject entries ->
      let entries = Js.Dict.entries entries in
      Edn.any
        (Edn.map
           (List.init (Array.length entries) (fun index ->
                let key, value = entries.(index) in
                (Edn.any (Edn.string key), of_json value))))

let of_json_string source = Js.Json.parseExn source |> of_json

let json_number_of_int64 value =
  if int64_is_safe_json_integer value then Js.Json.number (Int64.to_float value)
  else Js.Json.string (Int64.to_string value)

let json_key_of_value (Edn.Any value) =
  match value with
  | Edn.String value -> value
  | Edn.Keyword value -> Edn.keyword_to_string value
  | Edn.Symbol value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let rec json_array values =
  Js.Json.array
    (Array.init (Array.length values) (fun index ->
         to_json (Array.get values index)))

and json_object entries =
  let object_ = Js.Dict.empty () in
  Array.iter
    (fun (key, value) ->
      Js.Dict.set object_ (json_key_of_value key) (to_json value))
    entries;
  Js.Json.object_ object_

and to_json (Edn.Any value) =
  match value with
  | Edn.Nil -> Js.Json.null
  | Edn.Bool value -> Js.Json.boolean value
  | Edn.String value -> Js.Json.string value
  | Edn.Char value ->
      Js.Json.string (Edn.to_edn_string (Edn.any (Edn.char value)))
  | Edn.Symbol value -> Js.Json.string value
  | Edn.Keyword value -> Js.Json.string (":" ^ Edn.keyword_to_string value)
  | Edn.Int value -> json_number_of_int64 value
  | Edn.Bigint value -> Js.Json.string value
  | Edn.Float value -> (
      match json_string_of_non_finite_float value with
      | Some value -> Js.Json.string value
      | None -> Js.Json.number value)
  | Edn.Decimal value -> Js.Json.string value
  | Edn.Ratio value -> Js.Json.string value
  | Edn.Regex value -> Js.Json.string value
  | Edn.List values | Edn.Vector values | Edn.Set values -> json_array values
  | Edn.Map entries -> json_object entries
  | Edn.Tagged (tag, value) ->
      json_object
        [|
          (Edn.any (Edn.string "tag"), Edn.any (Edn.string tag));
          (Edn.any (Edn.string "value"), value);
        |]

let to_json_string value = Js.Json.stringify (to_json value)
