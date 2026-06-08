type keyword
type symbol
type map
type set
type vector
type list_
type number

type _ t = private
  | Nil : unit t
  | Bool : bool -> bool t
  | String : string -> string t
  | Char : Uchar.t -> Uchar.t t
  | Symbol : string -> symbol t
  | Keyword : string -> keyword t
  | Int : int64 -> number t
  | Bigint : string -> number t
  | Float : float -> number t
  | Decimal : string -> number t
  | List : any iarray -> list_ t
  | Vector : any iarray -> vector t
  | Map : (any * any) iarray -> map t
  | Set : any iarray -> set t
  | Tagged : string * any -> (string * any) t

and any = Any : _ t -> any

exception Parse_error of string

val any : _ t -> any
val nil : unit t
val bool : bool -> bool t
val string : string -> string t
val char : Uchar.t -> Uchar.t t
val symbol : string -> symbol t
val keyword : string -> keyword t
val int : int64 -> number t
val bigint : string -> number t
val float : float -> number t
val decimal : string -> number t
val list : any list -> list_ t
val vector : any list -> vector t
val map : (any * any) list -> map t
val set : any list -> set t
val tagged : string -> any -> (string * any) t

val of_edn_string : string -> any
val of_edn_string_all : string -> any list
val to_edn_string : any -> string
val of_json : Js.Json.t -> any
val of_json_string : string -> any
val to_json : any -> Js.Json.t
val to_json_string : any -> string
