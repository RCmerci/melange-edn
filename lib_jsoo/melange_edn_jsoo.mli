type json = Js_of_ocaml.Js.Unsafe.any

include module type of Melange_edn

val of_json : json -> any
val of_json_string : string -> any
val to_json : any -> json
val to_json_string : any -> string
