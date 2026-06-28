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

let rec of_json = function
  | `Null -> Edn.any Edn.nil
  | `Bool value -> Edn.any (Edn.bool value)
  | `String value -> Edn.any (Edn.string value)
  | `Int value -> Edn.any (Edn.int (Int64.of_int value))
  | `Intlit value -> Edn.any (Edn.bigint value)
  | `Float value -> edn_number_of_json_number value
  | `List values -> Edn.any (Edn.vector (List.map of_json values))
  | `Assoc entries ->
      Edn.any
        (Edn.map
           (List.map
              (fun (key, value) -> (Edn.any (Edn.string key), of_json value))
              entries))

let of_json_string source = of_json (Yojson.Safe.from_string source)

let json_key_of_value (Edn.Any value) =
  match value with
  | Edn.String value -> value
  | Edn.Keyword value -> Edn.keyword_to_string value
  | Edn.Symbol value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let json_number_of_int64 value =
  if int64_is_safe_json_integer value then `Int (Int64.to_int value)
  else `String (Int64.to_string value)

let rec json_array values =
  `List
    (List.init (Array.length values) (fun index ->
         to_json (Array.get values index)))

and json_object entries =
  `Assoc
    (List.init (Array.length entries) (fun index ->
         let key, value = Array.get entries index in
         (json_key_of_value key, to_json value)))

and to_json (Edn.Any value) =
  match value with
  | Edn.Nil -> `Null
  | Edn.Bool value -> `Bool value
  | Edn.String value -> `String value
  | Edn.Char value -> `String (Edn.to_edn_string (Edn.any (Edn.char value)))
  | Edn.Symbol value -> `String value
  | Edn.Keyword value -> `String (":" ^ Edn.keyword_to_string value)
  | Edn.Int value -> json_number_of_int64 value
  | Edn.Bigint value -> `String value
  | Edn.Float value -> (
      match json_string_of_non_finite_float value with
      | Some value -> `String value
      | None -> `Float value)
  | Edn.Decimal value -> `String value
  | Edn.Ratio value -> `String value
  | Edn.Regex value -> `String value
  | Edn.List values | Edn.Vector values | Edn.Set values -> json_array values
  | Edn.Map entries -> json_object entries
  | Edn.Tagged (tag, value) ->
      `Assoc [ ("tag", `String tag); ("value", to_json value) ]

let to_json_string value = Yojson.Safe.to_string (to_json value)
