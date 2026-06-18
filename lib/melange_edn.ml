type char_ = Char_tag
type keyword = Keyword_tag
type symbol = Symbol_tag
type map = Map_tag
type set = Set_tag
type vector = Vector_tag
type list_ = List_tag
type number = Number_tag
type regex = Regex_tag

type keyword_value = string

type _ t =
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
  | List : any iarray -> list_ t
  | Vector : any iarray -> vector t
  | Map : (any * any) iarray -> map t
  | Set : any iarray -> set t
  | Tagged : string * any -> (string * any) t

and any = Any : _ t -> any

exception Parse_error of string

let invalid_keyword_name value =
  value = ""
  || value.[0] = ':'
  || value = "/"
  || String.starts_with ~prefix:"/" value

let invalid_symbol_name value =
  String.length value > 0 && value.[0] = ':'

let any value = Any value
let nil = Nil
let bool value = Bool value
let string value = String value
let char value = Char value
let symbol value =
  if invalid_symbol_name value then raise (Parse_error ("invalid symbol: " ^ value));
  Symbol value

let keyword value =
  if invalid_keyword_name value then
    raise (Parse_error ("invalid keyword: :" ^ value));
  Keyword value

let keyword_to_string value = value

let int value = Int value
let bigint value = Bigint value
let float value = Float value
let decimal value = Decimal value
let ratio value = Ratio value
let regex value = Regex value
let list values = List (Iarray.of_list values)
let vector values = Vector (Iarray.of_list values)
let map entries = Map (Iarray.of_list entries)
let set values = Set (Iarray.of_list values)
let tagged tag value = Tagged (tag, value)

module Parser = struct
  type parser = { source : string; mutable pos : int; len : int }

  let create source = { source; pos = 0; len = String.length source }
  let is_eof parser = parser.pos >= parser.len

  let peek parser =
    if is_eof parser then None else Some parser.source.[parser.pos]

  let parse_error parser message =
    raise (Parse_error (Printf.sprintf "at position %d: %s" parser.pos message))

  let next parser =
    match peek parser with
    | None -> parse_error parser "unexpected end of input"
    | Some ch ->
        parser.pos <- parser.pos + 1;
        ch

  let is_whitespace = function
    | ' ' | '\n' | '\r' | '\t' | ',' -> true
    | _ -> false

  let rec skip_ws parser =
    match peek parser with
    | Some ch when is_whitespace ch ->
        parser.pos <- parser.pos + 1;
        skip_ws parser
    | Some ';' ->
        skip_comment parser;
        skip_ws parser
    | _ -> ()

  and skip_comment parser =
    match peek parser with
    | None -> ()
    | Some '\n' -> ()
    | Some _ ->
        parser.pos <- parser.pos + 1;
        skip_comment parser

  let is_delimiter = function
    | '"' | '(' | ')' | '[' | ']' | '{' | '}' | ';' -> true
    | ch when is_whitespace ch -> true
    | _ -> false

  let is_octal_digit = function '0' .. '7' -> true | _ -> false
  let octal_digit_value ch = Char.code ch - Char.code '0'

  let read_hex4 parser =
    if parser.pos + 4 > parser.len then
      parse_error parser "incomplete unicode escape";
    let code = ref 0 in
    for _ = 1 to 4 do
      let ch = next parser in
      let digit =
        match ch with
        | '0' .. '9' -> Char.code ch - Char.code '0'
        | 'a' .. 'f' -> 10 + Char.code ch - Char.code 'a'
        | 'A' .. 'F' -> 10 + Char.code ch - Char.code 'A'
        | _ -> parse_error parser "invalid unicode escape"
      in
      code := (!code * 16) + digit
    done;
    match Uchar.of_int !code with
    | uchar -> uchar
    | exception Invalid_argument _ ->
        parse_error parser "invalid unicode scalar"

  let read_octal_escape parser first_digit =
    let rec loop remaining value =
      if remaining = 0 then value
      else
        match peek parser with
        | Some ch when is_octal_digit ch ->
            parser.pos <- parser.pos + 1;
            loop (remaining - 1) ((value * 8) + octal_digit_value ch)
        | _ -> value
    in
    let value = loop 2 first_digit in
    if value > 255 then parse_error parser "octal escape is out of range";
    Uchar.of_int value

  let octal_char_of_token token =
    let len = String.length token in
    if len < 2 || len > 4 || token.[0] <> 'o' then None
    else
      let rec loop pos value =
        if pos = len then Some value
        else if is_octal_digit token.[pos] then
          loop (pos + 1) ((value * 8) + octal_digit_value token.[pos])
        else None
      in
      match loop 1 0 with
      | Some value when value <= 255 -> Some (Uchar.of_int value)
      | _ -> None

  let read_string parser =
    let buffer = Buffer.create 32 in
    let rec loop () =
      match next parser with
      | '"' -> any (String (Buffer.contents buffer))
      | '\\' -> (
          match next parser with
          | 't' ->
              Buffer.add_char buffer '\t';
              loop ()
          | 'r' ->
              Buffer.add_char buffer '\r';
              loop ()
          | 'n' ->
              Buffer.add_char buffer '\n';
              loop ()
          | 'b' ->
              Buffer.add_char buffer '\b';
              loop ()
          | 'f' ->
              Buffer.add_char buffer '\012';
              loop ()
          | '\\' ->
              Buffer.add_char buffer '\\';
              loop ()
          | '"' ->
              Buffer.add_char buffer '"';
              loop ()
          | 'u' ->
              Buffer.add_utf_8_uchar buffer (read_hex4 parser);
              loop ()
          | '0' .. '7' as ch ->
              Buffer.add_utf_8_uchar buffer
                (read_octal_escape parser (octal_digit_value ch));
              loop ()
          | ch ->
              parse_error parser
                (Printf.sprintf "unsupported string escape: \\%c" ch))
      | ch ->
          Buffer.add_char buffer ch;
          loop ()
    in
    loop ()

  let read_token parser =
    let start = parser.pos in
    let rec loop () =
      match peek parser with
      | Some ch when not (is_delimiter ch) ->
          parser.pos <- parser.pos + 1;
          loop ()
      | _ -> String.sub parser.source start (parser.pos - start)
    in
    loop ()

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_nonzero_digit = function '1' .. '9' -> true | _ -> false

  let skip_sign token =
    if String.length token > 0 && (token.[0] = '+' || token.[0] = '-') then 1
    else 0

  let consume_digits token pos =
    let len = String.length token in
    let rec loop pos =
      if pos < len && is_digit token.[pos] then loop (pos + 1) else pos
    in
    loop pos

  let consume_integer_body token pos =
    let len = String.length token in
    if pos >= len then None
    else if token.[pos] = '0' then Some (pos + 1)
    else if is_nonzero_digit token.[pos] then
      Some (consume_digits token (pos + 1))
    else None

  let full_integer_literal token =
    match consume_integer_body token (skip_sign token) with
    | Some pos -> pos = String.length token
    | None -> false

  let full_float_literal token =
    let len = String.length token in
    match consume_integer_body token (skip_sign token) with
    | None -> false
    | Some integer_end -> (
        match
          if integer_end < len && token.[integer_end] = '.' then
            let fraction_start = integer_end + 1 in
            let after_digits = consume_digits token fraction_start in
            if after_digits = fraction_start then None else Some after_digits
          else Some integer_end
        with
        | None -> false
        | Some pos -> (
            match
              if pos < len && (token.[pos] = 'e' || token.[pos] = 'E') then
                let exponent_start =
                  if
                    pos + 1 < len
                    && (token.[pos + 1] = '+' || token.[pos + 1] = '-')
                  then pos + 2
                  else pos + 1
                in
                let exponent_end = consume_digits token exponent_start in
                if exponent_end = exponent_start then None
                else Some exponent_end
              else Some pos
            with
            | None -> false
            | Some pos -> pos = len))

  let has_float_marker token =
    String.exists (function '.' | 'e' | 'E' -> true | _ -> false) token

  let digit_value = function
    | '0' .. '9' as ch -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'z' as ch -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'Z' as ch -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None

  let decimal_digit_value ch = Char.code ch - Char.code '0'

  let parse_unsigned_based_integer base token start =
    let len = String.length token in
    if start >= len then None
    else
      let rec loop pos acc =
        if pos = len then Some acc
        else
          match digit_value token.[pos] with
          | Some digit when digit < base ->
              loop (pos + 1)
                Int64.(add (mul acc (of_int base)) (of_int digit))
          | _ -> None
      in
      loop start 0L

  let parse_decimal_base token start marker_pos =
    if start = marker_pos then None
    else
      let rec loop pos acc =
        if pos = marker_pos then Some acc
        else
          match token.[pos] with
          | ch when is_digit ch ->
              loop (pos + 1) ((acc * 10) + decimal_digit_value ch)
          | _ -> None
      in
      loop start 0

  let find_radix_marker token start =
    let len = String.length token in
    let rec loop pos =
      if pos >= len then None
      else
        match token.[pos] with
        | 'r' | 'R' -> Some pos
        | _ -> loop (pos + 1)
    in
    loop start

  let parse_based_integer token =
    let len = String.length token in
    let sign_start = skip_sign token in
    let sign = if sign_start = 1 && token.[0] = '-' then -1L else 1L in
    let apply_sign value = Int64.mul sign value in
    if sign_start + 2 < len && token.[sign_start] = '0'
       && (token.[sign_start + 1] = 'x' || token.[sign_start + 1] = 'X')
    then
      Option.map apply_sign
        (parse_unsigned_based_integer 16 token (sign_start + 2))
    else
      match find_radix_marker token sign_start with
      | Some marker_pos -> (
          match parse_decimal_base token sign_start marker_pos with
          | Some base when base >= 2 && base <= 36 ->
              Option.map apply_sign
                (parse_unsigned_based_integer base token (marker_pos + 1))
          | _ -> None)
      | None ->
          if sign_start + 1 < len && token.[sign_start] = '0'
             && String.for_all is_octal_digit
                  (String.sub token sign_start (len - sign_start))
          then
            Option.map apply_sign
              (parse_unsigned_based_integer 8 token sign_start)
          else None

  let strip_leading_plus value =
    if String.length value > 0 && value.[0] = '+' then
      String.sub value 1 (String.length value - 1)
    else value

  let normalize_numeric_literal value =
    match strip_leading_plus value with "-0" -> "0" | value -> value

  let full_ratio_literal token =
    let len = String.length token in
    let start = skip_sign token in
    match consume_integer_body token start with
    | Some numerator_end
      when numerator_end < len && token.[numerator_end] = '/' -> (
        match consume_integer_body token (numerator_end + 1) with
        | Some denominator_end -> denominator_end = len
        | None -> false)
    | _ -> false

  let invalid_based_integer_literal token =
    let len = String.length token in
    let start = skip_sign token in
    if start >= len || not (is_digit token.[start]) then false
    else if start + 1 < len && token.[start] = '0' && is_digit token.[start + 1]
            && not
                 (String.for_all is_octal_digit
                    (String.sub token start (len - start)))
    then true
    else if start + 1 < len && token.[start] = '0'
            && (token.[start + 1] = 'x' || token.[start + 1] = 'X')
    then Option.is_none (parse_unsigned_based_integer 16 token (start + 2))
    else
      match find_radix_marker token start with
      | None -> false
      | Some marker_pos -> (
          match parse_decimal_base token start marker_pos with
          | Some base when base >= 2 && base <= 36 ->
              Option.is_none
                (parse_unsigned_based_integer base token (marker_pos + 1))
          | _ -> true)

  let parse_number token =
    let len = String.length token in
    if len > 1 && token.[len - 1] = 'N' then
      let body = String.sub token 0 (len - 1) in
      (match parse_based_integer body with
      | Some value -> Some (any (Bigint (Int64.to_string value)))
      | None ->
          if full_integer_literal body then
            Some (any (Bigint (normalize_numeric_literal body)))
          else None)
    else if len > 1 && token.[len - 1] = 'M' then
      let body = String.sub token 0 (len - 1) in
      if full_float_literal body then
        Some (any (Decimal (normalize_numeric_literal body)))
      else None
    else if full_ratio_literal token then
      Some (any (Ratio (normalize_numeric_literal token)))
    else
      (match parse_based_integer token with
      | Some value -> Some (any (Int value))
      | None ->
          if has_float_marker token && full_float_literal token then
            Some (any (Float (float_of_string token)))
          else if full_integer_literal token then
            Some (any (Int (Int64.of_string token)))
          else None)

  let parse_token parser token =
    match token with
    | "" -> parse_error parser "expected token"
    | "nil" -> any Nil
    | "true" -> any (Bool true)
    | "false" -> any (Bool false)
    | _ -> (
        match parse_number token with
        | Some value -> value
        | None when String.length token > 0 && token.[0] = ':' ->
            let value = String.sub token 1 (String.length token - 1) in
            if invalid_keyword_name value then
              parse_error parser ("invalid keyword: " ^ token)
            else any (Keyword value)
        | None when invalid_based_integer_literal token ->
            parse_error parser ("Invalid number: " ^ token)
        | None -> any (Symbol token))

  let read_char parser =
    let token = read_token parser in
    match token with
    | "" -> parse_error parser "missing character literal"
    | "newline" -> any (Char (Uchar.of_char '\n'))
    | "return" -> any (Char (Uchar.of_char '\r'))
    | "space" -> any (Char (Uchar.of_char ' '))
    | "tab" -> any (Char (Uchar.of_char '\t'))
    | "backspace" -> any (Char (Uchar.of_char '\b'))
    | "formfeed" -> any (Char (Uchar.of_char '\012'))
    | _ when String.length token > 0 && token.[0] = 'o' -> (
        match octal_char_of_token token with
        | Some uchar -> any (Char uchar)
        | None -> parse_error parser ("invalid character literal: \\" ^ token))
    | _ when String.length token = 5 && token.[0] = 'u' ->
        let char_parser = create (String.sub token 1 4) in
        any (Char (read_hex4 char_parser))
    | _ when String.length token = 1 -> any (Char (Uchar.of_char token.[0]))
    | _ -> parse_error parser ("invalid character literal: \\" ^ token)

  let reader_macro_list macro value =
    any
      (List
         (Iarray.of_list [ any (Symbol macro); value ]))

  let rec read_required parser =
    match read_value parser with
    | Some value -> value
    | None -> parse_error parser "expected EDN value"

  and read_value parser =
    skip_ws parser;
    match peek parser with
    | None -> None
    | Some (')' | ']' | '}') -> None
    | Some '"' ->
        ignore (next parser);
        Some (read_string parser)
    | Some '\\' ->
        ignore (next parser);
        Some (read_char parser)
    | Some '(' ->
        ignore (next parser);
        Some (any (List (Iarray.of_list (read_sequence parser ')'))))
    | Some '[' ->
        ignore (next parser);
        Some (any (Vector (Iarray.of_list (read_sequence parser ']'))))
    | Some '{' ->
        ignore (next parser);
        Some (read_map parser)
    | Some '#' ->
        ignore (next parser);
        read_dispatch parser
    | Some '\'' ->
        ignore (next parser);
        Some (reader_macro_list "quote" (read_required parser))
    | Some '@' ->
        ignore (next parser);
        Some (reader_macro_list "deref" (read_required parser))
    | Some '^' ->
        ignore (next parser);
        ignore (read_required parser);
        Some (read_required parser)
    | Some _ -> Some (parse_token parser (read_token parser))

  and read_sequence parser closing =
    let rec loop acc =
      skip_ws parser;
      match peek parser with
      | Some ch when ch = closing ->
          ignore (next parser);
          List.rev acc
      | Some ((')' | ']' | '}') as ch) ->
          parse_error parser
            (Printf.sprintf "unexpected closing delimiter: %c" ch)
      | None -> parse_error parser (Printf.sprintf "missing closing %c" closing)
      | _ -> (
          match read_value parser with
          | Some value -> loop (value :: acc)
          | None -> loop acc)
    in
    loop []

  and read_map_with_key_transform parser transform_key =
    let values = read_sequence parser '}' in
    let rec pairs keys acc = function
      | [] -> any (Map (Iarray.of_list (List.rev acc)))
      | [ _ ] -> parse_error parser "map requires an even number of forms"
      | key :: value :: rest ->
          let key = transform_key key in
          if List.exists (fun existing -> existing = key) keys then
            parse_error parser "Map literal contains duplicate key"
          else pairs (key :: keys) ((key, value) :: acc) rest
    in
    pairs [] [] values

  and read_map parser = read_map_with_key_transform parser (fun key -> key)

  and expand_namespaced_key namespace (Any value as key) =
    let expand_name name =
      if String.starts_with ~prefix:"_/" name then
        String.sub name 2 (String.length name - 2)
      else if String.contains name '/' then name
      else namespace ^ "/" ^ name
    in
    match value with
    | Keyword name -> any (Keyword (expand_name name))
    | Symbol name -> any (Symbol (expand_name name))
    | _ -> key

  and read_namespaced_map parser =
    let namespace = read_token parser in
    if namespace = "" then parse_error parser "missing namespaced map namespace";
    skip_ws parser;
    match peek parser with
    | Some '{' ->
        ignore (next parser);
        read_map_with_key_transform parser (expand_namespaced_key namespace)
    | Some ch ->
        parse_error parser
          (Printf.sprintf "expected namespaced map body, got %c" ch)
    | None -> parse_error parser "missing namespaced map body"

  and read_symbolic_value parser =
    let token = read_token parser in
    match token with
    | "NaN" -> Some (any (Float Float.nan))
    | "Inf" -> Some (any (Float Float.infinity))
    | "-Inf" -> Some (any (Float Float.neg_infinity))
    | _ -> parse_error parser ("unsupported symbolic value: ##" ^ token)

  and read_regex parser =
    let buffer = Buffer.create 32 in
    let rec loop () =
      match next parser with
      | '"' -> Some (any (Regex (Buffer.contents buffer)))
      | '\\' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer (next parser);
          loop ()
      | ch ->
          Buffer.add_char buffer ch;
          loop ()
    in
    loop ()

  and string_payload parser tag (Any value) =
    match value with
    | String value -> value
    | _ -> parse_error parser (tag ^ " literal expects a string representation")

  and is_hex_digit = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false

  and read_uuid_tag parser value =
    let uuid = string_payload parser "UUID" value in
    let valid =
      String.length uuid = 36
      && List.for_all
           (fun index -> uuid.[index] = '-')
           [ 8; 13; 18; 23 ]
      && String.for_all
           (fun ch -> ch = '-' || is_hex_digit ch)
           uuid
    in
    if valid then any (Tagged ("uuid", any (String (String.lowercase_ascii uuid))))
    else parse_error parser ("Invalid UUID literal: " ^ uuid)

  and parse_fixed_digits parser source pos count label =
    let len = String.length source in
    if pos + count > len then parse_error parser ("Invalid inst " ^ label);
    let rec loop index value =
      if index = count then value
      else
        match source.[pos + index] with
        | '0' .. '9' as ch ->
            loop (index + 1) ((value * 10) + Char.code ch - Char.code '0')
        | _ -> parse_error parser ("Invalid inst " ^ label)
    in
    loop 0 0

  and check_range parser label low high value =
    if value < low || value > high then
      parse_error parser (Printf.sprintf "Invalid inst %s" label)

  and validate_inst_literal parser source =
    let len = String.length source in
    let pos = ref 0 in
    let read count label =
      let value = parse_fixed_digits parser source !pos count label in
      pos := !pos + count;
      value
    in
    let expect ch label =
      if !pos >= len || source.[!pos] <> ch then
        parse_error parser ("Invalid inst " ^ label);
      incr pos
    in
    ignore (read 4 "year");
    if !pos < len then begin
      expect '-' "month separator";
      let month = read 2 "month" in
      check_range parser "month" 1 12 month;
      if !pos < len then begin
        expect '-' "day separator";
        let day = read 2 "day" in
        check_range parser "day" 1 31 day;
        if !pos < len then begin
          expect 'T' "time separator";
          let hour = read 2 "hour" in
          check_range parser "hour" 0 23 hour;
          expect ':' "minute separator";
          let minute = read 2 "minute" in
          check_range parser "minute" 0 59 minute;
          if !pos < len && source.[!pos] = ':' then (
            incr pos;
            let second = read 2 "second" in
            check_range parser "second" 0 60 second);
          if !pos < len && source.[!pos] = '.' then (
            incr pos;
            let fraction_start = !pos in
            while !pos < len && is_digit source.[!pos] do
              incr pos
            done;
            if !pos = fraction_start then
              parse_error parser "Invalid inst fraction");
          if !pos < len then
            match source.[!pos] with
            | 'Z' ->
                incr pos
            | '+' | '-' ->
                incr pos;
                let offset_hour = read 2 "offset hour" in
                check_range parser "offset hour" 0 23 offset_hour;
                expect ':' "offset separator";
                let offset_minute = read 2 "offset minute" in
                check_range parser "offset minute" 0 59 offset_minute
            | _ -> parse_error parser "Invalid inst offset"
        end
      end
    end;
    if !pos <> len then parse_error parser "Invalid inst literal"

  and read_inst_tag parser value =
    let instant = string_payload parser "inst" value in
    validate_inst_literal parser instant;
    any (Tagged ("inst", any (String instant)))

  and read_tagged_value parser tag value =
    match tag with
    | "uuid" -> read_uuid_tag parser value
    | "inst" -> read_inst_tag parser value
    | _ -> any (Tagged (tag, value))

  and read_anonymous_function parser =
    let body = any (List (Iarray.of_list (read_sequence parser ')'))) in
    Some
      (any
         (List
            (Iarray.of_list
                 [
                 any (Symbol "fn*");
                 any (Vector (Iarray.of_list []));
                 body;
               ])))

  and read_dispatch parser =
    match peek parser with
    | Some '{' ->
        ignore (next parser);
        let values = read_sequence parser '}' in
        let rec ensure_unique seen = function
          | [] -> ()
          | value :: rest ->
              if List.exists (fun existing -> existing = value) seen then
                parse_error parser "Set literal contains duplicate key"
              else ensure_unique (value :: seen) rest
        in
        ensure_unique [] values;
        Some (any (Set (Iarray.of_list values)))
    | Some '_' ->
        ignore (next parser);
        ignore (read_required parser);
        read_value parser
    | Some ':' ->
        ignore (next parser);
        Some (read_namespaced_map parser)
    | Some '#' ->
        ignore (next parser);
        read_symbolic_value parser
    | Some '"' ->
        ignore (next parser);
        read_regex parser
    | Some '\'' ->
        ignore (next parser);
        Some (reader_macro_list "var" (read_required parser))
    | Some '(' ->
        ignore (next parser);
        read_anonymous_function parser
    | Some ch when ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') ->
        let tag = read_token parser in
        let value = read_required parser in
        Some (read_tagged_value parser tag value)
    | Some ch ->
        parse_error parser (Printf.sprintf "unsupported dispatch: #%c" ch)
    | None -> parse_error parser "missing dispatch character"
end

let of_edn_string_all source =
  let parser = Parser.create source in
  let rec loop acc =
    match Parser.read_value parser with
    | Some value -> loop (value :: acc)
    | None ->
        Parser.skip_ws parser;
        if Parser.is_eof parser then List.rev acc
        else Parser.parse_error parser "unexpected closing delimiter"
  in
  loop []

let of_edn_string source =
  let parser = Parser.create source in
  match Parser.read_value parser with
  | None ->
      Parser.skip_ws parser;
      if Parser.is_eof parser then any Nil
      else Parser.parse_error parser "unexpected closing delimiter"
  | Some value ->
      value

let escape_string value =
  let buffer = Buffer.create (String.length value + 8) in
  let add_escape = function
    | '\t' -> Buffer.add_string buffer "\\t"
    | '\r' -> Buffer.add_string buffer "\\r"
    | '\n' -> Buffer.add_string buffer "\\n"
    | '\b' -> Buffer.add_string buffer "\\b"
    | '\012' -> Buffer.add_string buffer "\\f"
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '"' -> Buffer.add_string buffer "\\\""
    | ch when Char.code ch < 0x20 ->
        Buffer.add_string buffer (Printf.sprintf "\\u%04X" (Char.code ch))
    | ch -> Buffer.add_char buffer ch
  in
  String.iter add_escape value;
  Buffer.contents buffer

let string_of_char_literal uchar =
  if Uchar.equal uchar (Uchar.of_char '\n') then "\\newline"
  else if Uchar.equal uchar (Uchar.of_char '\r') then "\\return"
  else if Uchar.equal uchar (Uchar.of_char ' ') then "\\space"
  else if Uchar.equal uchar (Uchar.of_char '\t') then "\\tab"
  else if Uchar.equal uchar (Uchar.of_char '\b') then "\\backspace"
  else if Uchar.equal uchar (Uchar.of_char '\012') then "\\formfeed"
  else if Uchar.to_int uchar < 0x20 then
    Printf.sprintf "\\u%04X" (Uchar.to_int uchar)
  else
    match Uchar.to_char uchar with
    | ch -> Printf.sprintf "\\%c" ch
    | exception Invalid_argument _ ->
        Printf.sprintf "\\u%04X" (Uchar.to_int uchar)

let join_iarray separator render values =
  let buffer = Buffer.create 32 in
  Iarray.iteri
    (fun index value ->
      if index > 0 then Buffer.add_string buffer separator;
      Buffer.add_string buffer (render value))
    values;
  Buffer.contents buffer

let rec to_edn_string (Any value) =
  match value with
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | String value -> "\"" ^ escape_string value ^ "\""
  | Char value -> string_of_char_literal value
  | Symbol value -> value
  | Keyword value -> ":" ^ keyword_to_string value
  | Int value -> Int64.to_string value
  | Bigint value -> normalize_number value ^ "N"
  | Float value when classify_float value = FP_nan -> "##NaN"
  | Float value when classify_float value = FP_infinite && value > 0. -> "##Inf"
  | Float value when classify_float value = FP_infinite -> "##-Inf"
  | Float value -> string_of_float value
  | Decimal value -> normalize_number value ^ "M"
  | Ratio value -> normalize_number value
  | Regex value -> "#\"" ^ value ^ "\""
  | List values -> "(" ^ join_iarray " " to_edn_string values ^ ")"
  | Vector values -> "[" ^ join_iarray " " to_edn_string values ^ "]"
  | Set values -> "#{" ^ join_iarray " " to_edn_string values ^ "}"
  | Map entries ->
      let render_entry (key, value) =
        to_edn_string key ^ " " ^ to_edn_string value
      in
      "{" ^ join_iarray " " render_entry entries ^ "}"
  | Tagged (tag, value) -> "#" ^ tag ^ " " ^ to_edn_string value

and normalize_number value =
  if value = "-0" || value = "+0" then "0" else Parser.strip_leading_plus value

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
  if is_safe_json_integer value then any (Int (Int64.of_float value))
  else any (Float value)

let json_number_of_int64 value =
  if int64_is_safe_json_integer value then Js.Json.number (Int64.to_float value)
  else Js.Json.string (Int64.to_string value)

let rec of_json json =
  match Js.Json.classify json with
  | JSONNull -> any Nil
  | JSONFalse -> any (Bool false)
  | JSONTrue -> any (Bool true)
  | JSONString value -> any (String value)
  | JSONNumber value -> edn_number_of_json_number value
  | JSONArray values ->
      any (Vector (Iarray.of_list (List.map of_json (Array.to_list values))))
  | JSONObject entries ->
      any
        (Map
           (Iarray.of_list
              (List.map
                 (fun (key, value) -> (any (String key), of_json value))
                 (Array.to_list (Js.Dict.entries entries)))))

let of_json_string source = Js.Json.parseExn source |> of_json

let json_key_of_value (Any value) =
  match value with
  | String value -> value
  | Keyword value -> keyword_to_string value
  | Symbol value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let iarray_to_list render values =
  Iarray.fold_right (fun value acc -> render value :: acc) values []

let rec json_array values =
  Js.Json.array (Array.of_list (iarray_to_list to_json values))

and json_object entries =
  let json = Js.Dict.empty () in
  Iarray.iter
    (fun (key, value) ->
      Js.Dict.set json (json_key_of_value key) (to_json value))
    entries;
  Js.Json.object_ json

and to_json (Any value) =
  match value with
  | Nil -> Js.Json.null
  | Bool value -> Js.Json.boolean value
  | String value -> Js.Json.string value
  | Char value -> Js.Json.string (string_of_char_literal value)
  | Symbol value -> Js.Json.string value
  | Keyword value -> Js.Json.string (":" ^ keyword_to_string value)
  | Int value -> json_number_of_int64 value
  | Bigint value -> Js.Json.string value
  | Float value -> Js.Json.number value
  | Decimal value -> Js.Json.string value
  | Ratio value -> Js.Json.string value
  | Regex value -> Js.Json.string value
  | List values | Vector values -> json_array values
  | Set values -> json_array values
  | Map entries -> json_object entries
  | Tagged (tag, value) ->
      json_object
        (Iarray.of_list
           [
             (any (String "tag"), any (String tag));
             (any (String "value"), value);
           ])

let to_json_string value = Js.Json.stringify (to_json value)
