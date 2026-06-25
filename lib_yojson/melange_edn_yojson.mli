val of_json : Yojson.Safe.t -> Melange_edn.any
val of_json_string : string -> Melange_edn.any
val to_json : Melange_edn.any -> Yojson.Safe.t
val to_json_string : Melange_edn.any -> string
