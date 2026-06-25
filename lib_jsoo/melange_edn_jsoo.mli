type json = Js_of_ocaml.Js.Unsafe.any

val of_json : json -> Melange_edn.any
val of_json_string : string -> Melange_edn.any
val to_json : Melange_edn.any -> json
val to_json_string : Melange_edn.any -> string
