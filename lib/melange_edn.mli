type char_ = Char_tag
type keyword = Keyword_tag
type symbol = Symbol_tag
type map = Map_tag
type set = Set_tag
type vector = Vector_tag
type list_ = List_tag
type number = Number_tag
type regex = Regex_tag
type keyword_value

type _ t = private
  | Nil : unit t
  | Bool : bool -> bool t
  | String : string -> string t
  | Char : Uchar.t -> char_ t
  | Symbol : string -> symbol t
  | Keyword : keyword_value -> keyword t
  | Int : int64 -> number t
  | Bigint : string -> number t
  | Float : float -> number t
  | Decimal : string -> number t
  | Ratio : string -> number t
  | Regex : string -> regex t
  | List : any array -> list_ t
  | Vector : any array -> vector t
  | Map : (any * any) array -> map t
  | Set : any array -> set t
  | Tagged : string * any -> (string * any) t

and any = Any : _ t -> any

exception Parse_error of string

val any : _ t -> any
val nil : unit t
val bool : bool -> bool t
val string : string -> string t
val char : Uchar.t -> char_ t
val symbol : string -> symbol t
val keyword : string -> keyword t
val int : int64 -> number t
val bigint : string -> number t
val float : float -> number t
val decimal : string -> number t
val ratio : string -> number t
val regex : string -> regex t
val list : any list -> list_ t
val vector : any list -> vector t
val map : (any * any) list -> map t
val set : any list -> set t
val tagged : string -> any -> (string * any) t
val keyword_to_string : keyword_value -> string
val of_edn_string : string -> any
val of_edn_string_all : string -> any list
val to_edn_string : any -> string
