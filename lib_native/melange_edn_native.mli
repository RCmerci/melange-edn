include module type of Melange_edn

val of_json : Yojson.Safe.t -> any
val of_json_string : string -> any
val to_json : any -> Yojson.Safe.t
val to_json_string : any -> string
