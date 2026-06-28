module Melange_edn = Melange_edn_native

module Assert = struct
  let string = Alcotest.(check string) "string"
  let strings = Alcotest.(check (array string)) "string array"
  let ints = Alcotest.(check (array int)) "int array"
  let floats = Alcotest.(check (array (float 0.000000001))) "float array"

  let raises run =
    let raised =
      try
        run ();
        false
      with _ -> true
    in
    if not raised then Alcotest.fail "expected function to raise"

  let raises_parse_error run =
    match run () with
    | exception Melange_edn.Parse_error _ -> ()
    | exception exn ->
        Alcotest.fail
          (Printf.sprintf "expected Parse_error, got %s" (Printexc.to_string exn))
    | _ -> Alcotest.fail "expected function to raise Parse_error"
end

open Melange_edn

let edn value = any value
let edn_nil = edn nil
let edn_bool value = edn (bool value)
let edn_string value = edn (string value)
let edn_char value = edn (char value)
let edn_symbol value = edn (symbol value)
let edn_keyword value = edn (keyword value)
let edn_int value = edn (int value)
let edn_bigint value = edn (bigint value)
let edn_float value = edn (float value)
let edn_decimal value = edn (decimal value)
let edn_list values = edn (list values)
let edn_vector values = edn (vector values)
let edn_map entries = edn (map entries)
let edn_set values = edn (set values)
let edn_tagged tag value = edn (tagged tag value)

let parsed_string source =
  match of_edn_string source with
  | Any (String value) -> value
  | _ -> "not a string"

let parsed_char_code source =
  match of_edn_string source with
  | Any (Char value) -> Uchar.to_int value
  | _ -> -1

let parsed_float source =
  match of_edn_string source with Any (Float value) -> value | _ -> 0.0

let indexed_entries count render = List.init count render

let large_edn_map_source count =
  let entries =
    indexed_entries count (fun index ->
        ":k" ^ string_of_int index ^ " " ^ string_of_int index)
  in
  "{" ^ String.concat " " entries ^ "}"

let large_edn_set_source count =
  let values =
    indexed_entries count (fun index -> ":v" ^ string_of_int index)
  in
  "#{" ^ String.concat " " values ^ "}"

let large_json_array_source count =
  let values = indexed_entries count string_of_int in
  "[" ^ String.concat "," values ^ "]"

let large_json_object_source count =
  let entries =
    indexed_entries count (fun index ->
        "\"k" ^ string_of_int index ^ "\":" ^ string_of_int index)
  in
  "{" ^ String.concat "," entries ^ "}"

let typed_bool : bool Melange_edn.t = bool true
let typed_int : number Melange_edn.t = int 42L
let typed_bigint : number Melange_edn.t = bigint "42"
let typed_float : number Melange_edn.t = float 42.5
let typed_decimal : number Melange_edn.t = decimal "42.5"
let typed_string : string Melange_edn.t = string "typed"
let typed_char : char_ Melange_edn.t = char (Uchar.of_char 'x')
let typed_symbol : symbol Melange_edn.t = symbol "typed/symbol"
let typed_keyword : keyword Melange_edn.t = keyword "typed/keyword"

let typed_list : list_ Melange_edn.t =
  list [ edn typed_symbol; edn typed_keyword ]

let typed_set : set Melange_edn.t =
  set [ edn typed_int; edn typed_bigint; edn typed_float; edn typed_decimal ]

let typed_map : map Melange_edn.t = map [ (edn typed_keyword, edn typed_list) ]

let typed_vector : vector Melange_edn.t =
  vector [ edn typed_bool; edn typed_int; edn typed_string ]

let json_conversion_edn =
  edn_map
    [
      (edn_string "name", edn_string "Ada");
      (edn_string "age", edn_int 37L);
      (edn_string "large", edn_int 3000000000L);
      (edn_string "admin", edn_bool false);
      (edn_string "tags", edn_vector [ edn_string "ocaml"; edn_nil ]);
    ]

let gadt_constructors_tests =
  [
    Alcotest.test_case "writes existential typed vectors" `Quick (fun () ->
        Assert.string {|[true 42 "typed"]|} (to_edn_string (edn typed_vector)));
    Alcotest.test_case "writes existential typed chars" `Quick (fun () ->
        Assert.string {|\x|} (to_edn_string (edn typed_char)));
    Alcotest.test_case "keeps typed collection constructors distinct" `Quick
      (fun () ->
        Assert.string {|{:typed/keyword (typed/symbol :typed/keyword)}|}
          (to_edn_string (edn typed_map)));
    Alcotest.test_case "keeps typed set constructor distinct" `Quick (fun () ->
        Assert.string {|#{42 42N 42.5 42.5M}|} (to_edn_string (edn typed_set)));
    Alcotest.test_case "exposes keyword names through an accessor" `Quick
      (fun () ->
        let constructed =
          match keyword "typed/keyword" with Keyword value -> value | _ -> .
        in
        Assert.string "typed/keyword" (keyword_to_string constructed));
  ]

let edn_parsing_tests =
  [
    Alcotest.test_case "parses nil" `Quick (fun () ->
        Assert.string "nil" (to_edn_string (of_edn_string "nil")));
    Alcotest.test_case "parses booleans" `Quick (fun () ->
        Assert.strings [| "true"; "false" |]
          [|
            to_edn_string (of_edn_string "true");
            to_edn_string (of_edn_string "false");
          |]);
    Alcotest.test_case "parses string escapes" `Quick (fun () ->
        Assert.string {|"a\tb\n\"c\"\\"|}
          (to_edn_string (of_edn_string {| "a\tb\n\"c\"\\" |})));
    Alcotest.test_case "parses unicode string escapes" `Quick (fun () ->
        Assert.string {|"snowman: ☃"|}
          (to_edn_string (of_edn_string {| "snowman: \u2603" |})));
    Alcotest.test_case "parses character literals" `Quick (fun () ->
        Assert.strings
          [| {|\x|}; {|\space|}; {|\u2603|} |]
          [|
            to_edn_string (of_edn_string {|\x|});
            to_edn_string (of_edn_string {|\space|});
            to_edn_string (of_edn_string {|\u2603|});
          |]);
    Alcotest.test_case "parses symbols and keywords" `Quick (fun () ->
        Assert.strings
          [| "my.ns/name"; ":my.ns/name" |]
          [|
            to_edn_string (of_edn_string "my.ns/name");
            to_edn_string (of_edn_string ":my.ns/name");
          |]);
    Alcotest.test_case "parses abstract keyword values" `Quick (fun () ->
        let parsed_name =
          match of_edn_string ":my.ns/name" with
          | Any (Keyword value) -> keyword_to_string value
          | _ -> "not a keyword"
        in
        Assert.string "my.ns/name" parsed_name);
    Alcotest.test_case "parses numeric literals" `Quick (fun () ->
        Assert.strings
          [|
            "42"; "0"; "123456789012345678901234567890N"; "6.02e+23"; "1.20M";
          |]
          [|
            to_edn_string (of_edn_string "+42");
            to_edn_string (of_edn_string "-0");
            to_edn_string (of_edn_string "123456789012345678901234567890N");
            to_edn_string (of_edn_string "6.02e23");
            to_edn_string (of_edn_string "1.20M");
          |]);
    Alcotest.test_case "keeps malformed numeric tokens as symbols" `Quick
      (fun () ->
        Assert.strings [| "1."; "1e" |]
          [|
            to_edn_string (of_edn_string "1.");
            to_edn_string (of_edn_string "1e");
          |]);
    Alcotest.test_case "parses collections, comments, commas, and discard"
      `Quick (fun () ->
        let source =
          {|
      [a b #_foo 42 ; comments run to the end of the line
       {:a 1, "b" [true nil] :c #{foo \space}}]
    |}
        in
        Assert.string {|[a b 42 {:a 1 "b" [true nil] :c #{foo \space}}]|}
          (to_edn_string (of_edn_string source)));
    Alcotest.test_case "parses tagged values" `Quick (fun () ->
        Assert.string {|#inst "1985-04-12T23:20:50.52Z"|}
          (to_edn_string (of_edn_string {|#inst "1985-04-12T23:20:50.52Z"|})));
    Alcotest.test_case "reads all values" `Quick (fun () ->
        Assert.string "(1 2 :done)"
          (to_edn_string (edn_list (of_edn_string_all "1 #_ignored 2 :done"))));
  ]

let edn_parsing_cljs_reader_default_date_and_uuid_tags_tests =
  [
    Alcotest.test_case "parses UUID literals and writes canonical lowercase"
      `Quick (fun () ->
        Assert.string {|#uuid "550e8400-e29b-41d4-a716-446655440000"|}
          (to_edn_string
             (of_edn_string {|#uuid "550E8400-E29B-41D4-A716-446655440000"|})));
    Alcotest.test_case "rejects malformed UUID literals" `Quick (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [|
              {|#uuid "not-a-uuid"|};
              {|#uuid "550e8400-e29b-41d4-a716-44665544000z"|};
              {|#uuid "550e8400-e29b-41d4-a716-4466554400000"|};
              {|#uuid [550e8400-e29b-41d4-a716-446655440000]|};
            |]
        in
        Assert.strings
          [| "rejected"; "rejected"; "rejected"; "rejected" |]
          results);
    Alcotest.test_case "accepts cljs.reader inst date literals" `Quick
      (fun () ->
        Assert.string {|#inst "2010-11-12T13:14:15.666-05:00"|}
          (to_edn_string
             (of_edn_string {|#inst "2010-11-12T13:14:15.666-05:00"|})));
    Alcotest.test_case "rejects malformed inst date literals" `Quick (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [|
              {|#inst "2010-13-12T13:14:15.666-05:00"|};
              {|#inst "2010-11-32T13:14:15.666-05:00"|};
              {|#inst "2010-11-12T25:14:15.666-05:00"|};
              {|#inst [2010 11 12]|};
            |]
        in
        Assert.strings
          [| "rejected"; "rejected"; "rejected"; "rejected" |]
          results);
    Alcotest.test_case "falls back date tag to a generic tagged literal" `Quick
      (fun () ->
        Assert.string {|#date "2010-11-12"|}
          (to_edn_string (of_edn_string {|#date "2010-11-12"|})));
  ]

let edn_parsing_cljs_reader_compatibility_cases_tests =
  [
    Alcotest.test_case "parses octal, hex, and radix integer literals" `Quick
      (fun () ->
        Assert.strings
          [| "42"; "42"; "42"; "42"; "42"; "42" |]
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             [| "052"; "0x2a"; "2r101010"; "8R52"; "16r2a"; "36r16" |]));
    Alcotest.test_case "parses signed octal, hex, and radix integer literals"
      `Quick (fun () ->
        Assert.strings
          [|
            "42";
            "42";
            "42";
            "42";
            "42";
            "42";
            "-42";
            "-42";
            "-42";
            "-42";
            "-42";
            "-42";
          |]
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             [|
               "+052";
               "+0x2a";
               "+2r101010";
               "+8r52";
               "+16R2a";
               "+36r16";
               "-052";
               "-0X2a";
               "-2r101010";
               "-8r52";
               "-16r2a";
               "-36R16";
             |]));
    Alcotest.test_case "parses backspace and formfeed string escapes" `Quick
      (fun () ->
        Assert.string {|"escape \b and \f"|}
          (to_edn_string (of_edn_string {|"escape \b and \f"|})));
    Alcotest.test_case "parses named backspace and formfeed character literals"
      `Quick (fun () ->
        Assert.string {|[\backspace \formfeed]|}
          (to_edn_string (of_edn_string {|[\backspace \formfeed]|})));
    Alcotest.test_case "reads empty input as nil" `Quick (fun () ->
        Assert.string "nil" (to_edn_string (of_edn_string "")));
    Alcotest.test_case "reads comment-only input as nil" `Quick (fun () ->
        Assert.string "nil" (to_edn_string (of_edn_string "; ignored")));
    Alcotest.test_case "rejects duplicate map keys after numeric normalization"
      `Quick (fun () ->
        Assert.raises (fun () ->
            ignore (of_edn_string "{052 :octal 42 :decimal}")));
    Alcotest.test_case "rejects duplicate small map keys" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "{:a 1 :b 2 :a 3}")));
    Alcotest.test_case "rejects duplicate large map keys" `Quick (fun () ->
        Assert.raises (fun () ->
            ignore
              (of_edn_string
                 "{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :a 10}")));
    Alcotest.test_case "rejects duplicate set values" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "#{:a :b :c :a}")));
  ]

let edn_parsing_additional_cljs_reader_compatibility_cases_tests =
  [
    Alcotest.test_case "parses explicit namespaced keyword maps" `Quick
      (fun () ->
        Assert.string {|{:person/name "Ada" :person/age 42}|}
          (to_edn_string (of_edn_string {|#:person{:name "Ada" :age 42}|})));
    Alcotest.test_case "parses explicit namespaced symbol maps" `Quick
      (fun () ->
        Assert.string {|{person/name "Ada" person/age 42}|}
          (to_edn_string (of_edn_string {|#:person{name "Ada" age 42}|})));
    Alcotest.test_case "leaves namespace-qualified keys in namespaced maps"
      `Quick (fun () ->
        Assert.string {|{:person/name "Ada" :org/id 7}|}
          (to_edn_string (of_edn_string {|#:person{:name "Ada" :org/id 7}|})));
    Alcotest.test_case "uses underscore namespace escape in namespaced maps"
      `Quick (fun () ->
        Assert.string {|{:person/name "Ada" :id 7}|}
          (to_edn_string (of_edn_string {|#:person{:name "Ada" :_/id 7}|})));
    Alcotest.test_case "parses empty namespaced maps" `Quick (fun () ->
        Assert.string "{}" (to_edn_string (of_edn_string {|#:person{}|})));
    Alcotest.test_case "parses symbolic NaN" `Quick (fun () ->
        Assert.string "##NaN" (to_edn_string (of_edn_string "##NaN")));
    Alcotest.test_case "parses symbolic infinities" `Quick (fun () ->
        Assert.strings [| "##Inf"; "##-Inf" |]
          [|
            to_edn_string (of_edn_string "##Inf");
            to_edn_string (of_edn_string "##-Inf");
          |]);
    Alcotest.test_case "parses octal character literals" `Quick (fun () ->
        Assert.string {|[\A \S]|}
          (to_edn_string (of_edn_string {|[\o101 \o123]|})));
    Alcotest.test_case "parses octal string escapes" `Quick (fun () ->
        Assert.string {|"octal 0A"|}
          (to_edn_string (of_edn_string {|"octal \060\101"|})));
    Alcotest.test_case "rejects malformed based integer literals" `Quick
      (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [| "09"; "0x"; "2r102"; "37r10" |]
        in
        Assert.strings
          [| "rejected"; "rejected"; "rejected"; "rejected" |]
          results);
    Alcotest.test_case "rejects overflowing integer literals with parse errors"
      `Quick (fun () ->
        Array.iter
          (fun source ->
            Assert.raises_parse_error (fun () -> ignore (of_edn_string source)))
          [|
            "9223372036854775808";
            "-9223372036854775809";
            "0x8000000000000000";
            "-0x8000000000000001";
            "36rzzzzzzzzzzzzzz";
          |]);
  ]

let edn_parsing_remaining_cljs_reader_compatibility_cases_tests =
  [
    Alcotest.test_case "reads the first form and ignores trailing forms" `Quick
      (fun () -> Assert.string "1" (to_edn_string (of_edn_string "1 2")));
    Alcotest.test_case "reads the first non-comment form with later forms"
      `Quick (fun () ->
        Assert.string "3" (to_edn_string (of_edn_string ";foo\n3\n5")));
    Alcotest.test_case "parses ratio literals" `Quick (fun () ->
        Assert.strings [| "4/2"; "4/2"; "-4/2" |]
          [|
            to_edn_string (of_edn_string "4/2");
            to_edn_string (of_edn_string "+4/2");
            to_edn_string (of_edn_string "-4/2");
          |]);
    Alcotest.test_case "parses quote reader macro" `Quick (fun () ->
        Assert.string "(quote foo)" (to_edn_string (of_edn_string "'foo")));
    Alcotest.test_case "parses deref reader macro" `Quick (fun () ->
        Assert.string "(deref foo)" (to_edn_string (of_edn_string "@foo")));
    Alcotest.test_case "parses var reader macro" `Quick (fun () ->
        Assert.string "(var foo)" (to_edn_string (of_edn_string "#'foo")));
    Alcotest.test_case "parses regex literals" `Quick (fun () ->
        Assert.strings
          [| {|#"(?i)abc"|}; {|#"\[\]?(\\\")\\"|} |]
          [|
            to_edn_string (of_edn_string {|#"(?i)abc"|});
            to_edn_string (of_edn_string {|#"\[\]?(\\\")\\"|});
          |]);
    Alcotest.test_case "consumes metadata prefix before maps" `Quick (fun () ->
        Assert.string "{:a 1}" (to_edn_string (of_edn_string "^String {:a 1}")));
    Alcotest.test_case "consumes metadata prefix before quoted forms" `Quick
      (fun () ->
        Assert.string "(quote bar)" (to_edn_string (of_edn_string "^:foo 'bar")));
    Alcotest.test_case "parses simple anonymous function reader macro" `Quick
      (fun () ->
        Assert.string "(fn* [] (foo bar baz))"
          (to_edn_string (of_edn_string "#(foo bar baz)")));
  ]

let edn_parsing_upstream_no_runtime_compatibility_cases_tests =
  [
    Alcotest.test_case "parses additional integer literal examples" `Quick
      (fun () ->
        Assert.strings
          [| "1070"; "1070"; "-1070"; "511"; "-511"; "1340"; "-1340" |]
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             [|
               "0x42e"; "+0x42e"; "-0x42e"; "0777"; "-0777"; "02474"; "-02474";
             |]));
    Alcotest.test_case "parses additional floating literal examples" `Quick
      (fun () ->
        Assert.floats
          [| 42.23; 42.23; -42.23; 42200.; 42200.; -0.0422 |]
          (Array.map parsed_float
             [| "42.23"; "+42.23"; "-42.23"; "42.2e3"; "+42.2e+3"; "-42.2e-3" |]));
    Alcotest.test_case "parses additional symbol forms" `Quick (fun () ->
        let sources =
          [|
            "*+!-_?";
            "abc:def:ghi";
            "abc.def/ghi";
            "abc/def.ghi";
            "abc:def/ghi:jkl.mno";
            "foo//";
          |]
        in
        Assert.strings sources
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             sources));
    Alcotest.test_case "parses slash symbol as a map key" `Quick (fun () ->
        Assert.string "({/ 0})" (to_edn_string (of_edn_string "({/ 0})")));
    Alcotest.test_case "parses additional keyword forms" `Quick (fun () ->
        let sources =
          [|
            ":foo-bar";
            ":*+!-_?";
            ":abc:def:ghi";
            ":abc.def/ghi";
            ":abc/def.ghi";
            ":abc:def/ghi:jkl.mno";
          |]
        in
        Assert.strings sources
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             sources));
    Alcotest.test_case "parses additional character literal scalar values"
      `Quick (fun () ->
        Assert.ints
          [| 102; 404; 0; 0; 255; 65; 64; 55295; 57344; 65535 |]
          (Array.map parsed_char_code
             [|
               {|\f|};
               {|\u0194|};
               {|\o0|};
               {|\o000|};
               {|\o377|};
               {|\u0041|};
               {|\@|};
               {|\ud7ff|};
               {|\ue000|};
               {|\uffff|};
             |]));
    Alcotest.test_case "rejects surrogate unicode escape values" `Quick
      (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [| {|\ud800|}; {|"\ud800"|} |]
        in
        Assert.strings [| "rejected"; "rejected" |] results);
    Alcotest.test_case "parses additional string unicode and octal escapes"
      `Quick (fun () ->
        Assert.strings
          [| "foo\000bar"; "fooƔbar"; "fooSbar"; "0"; "à"; "ÿ" |]
          (Array.map parsed_string
             [|
               {|"foo\000bar"|};
               {|"foo\u0194bar"|};
               {|"foo\123bar"|};
               {|"0"|};
               {|"\340"|};
               {|"\377"|};
             |]));
    Alcotest.test_case "parses empty and nested collections" `Quick (fun () ->
        let sources =
          [|
            "()";
            "(foo (bar) baz)";
            "[]";
            "[foo [bar] baz]";
            "{}";
            "{foo {bar baz}}";
            "#{}";
            "#{foo #{bar} baz}";
          |]
        in
        Assert.strings sources
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             sources));
    Alcotest.test_case "parses no-space and namespaced tagged literals" `Quick
      (fun () ->
        Assert.strings
          [|
            {|#inst "2010-11-12T13:14:15.666-05:00"|};
            {|#uuid "550e8400-e29b-41d4-a716-446655440000"|};
            "#foo bar";
            "#foo.bar/baz [1 2]";
          |]
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             [|
               {|#inst"2010-11-12T13:14:15.666-05:00"|};
               {|#uuid"550e8400-e29b-41d4-a716-446655440000"|};
               "#foo bar";
               "#foo.bar/baz [1 2]";
             |]));
    Alcotest.test_case "parses unicode strings" `Quick (fun () ->
        let sources =
          [|
            {|"اختبار"|};
            {|"ทดสอบ"|};
            {|"こんにちは"|};
            {|"你好"|};
            {|"אַ גוט יאָר"|};
            {|"cześć"|};
            {|"привет"|};
          |]
        in
        Assert.strings sources
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             sources));
    Alcotest.test_case "parses unicode symbols and keywords" `Quick (fun () ->
        let sources =
          [|
            "ทดสอบ";
            "こんにちは";
            "你好";
            "cześć";
            "привет";
            ":ทดสอบ";
            ":こんにちは";
            ":你好";
            ":cześć";
            ":привет";
          |]
        in
        Assert.strings sources
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             sources));
    Alcotest.test_case "parses compound unicode maps" `Quick (fun () ->
        Assert.string {|{:привет :ru "你好" :cn}|}
          (to_edn_string (of_edn_string {|{:привет :ru "你好" :cn}|})));
    Alcotest.test_case "rejects malformed unicode string escapes" `Quick
      (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [| {|"abc \ua"|}; {|"abc \x0z"|}; {|"abc \u0g00"|} |]
        in
        Assert.strings [| "rejected"; "rejected"; "rejected" |] results);
    Alcotest.test_case "accepts partial inst literals without normalization"
      `Quick (fun () ->
        Assert.strings
          [|
            {|#inst "1500"|};
            {|#inst "1582-10-04"|};
            {|#inst "1582-10-04T23:59:59.999"|};
          |]
          (Array.map
             (fun source -> to_edn_string (of_edn_string source))
             [|
               {|#inst "1500"|};
               {|#inst "1582-10-04"|};
               {|#inst "1582-10-04T23:59:59.999"|};
             |]));
    Alcotest.test_case "rejects multiple duplicate set values" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "#{foo foo bar bar}")));
  ]

let edn_parsing_performance_sensitive_behavior_guards_tests =
  [
    Alcotest.test_case "preserves ordering for larger parsed maps and sets"
      `Quick (fun () ->
        let map_source = large_edn_map_source 256 in
        let set_source = large_edn_set_source 256 in
        Assert.strings
          [| map_source; set_source |]
          [|
            to_edn_string (of_edn_string map_source);
            to_edn_string (of_edn_string set_source);
          |]);
    Alcotest.test_case
      "rejects duplicate keys and values near the end of larger literals" `Quick
      (fun () ->
        let duplicate_map =
          let entries =
            indexed_entries 256 (fun index ->
                ":k" ^ string_of_int index ^ " " ^ string_of_int index)
          in
          "{" ^ String.concat " " (entries @ [ ":k128 :duplicate" ]) ^ "}"
        in
        let duplicate_set =
          let values =
            indexed_entries 256 (fun index -> ":v" ^ string_of_int index)
          in
          "#{" ^ String.concat " " (values @ [ ":v128" ]) ^ "}"
        in
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [| duplicate_map; duplicate_set |]
        in
        Assert.strings [| "rejected"; "rejected" |] results);
    Alcotest.test_case "writes larger maps without changing entry order" `Quick
      (fun () ->
        let entries =
          indexed_entries 256 (fun index ->
              ( edn_keyword ("k" ^ string_of_int index),
                edn_int (Int64.of_int index) ))
        in
        Assert.string (large_edn_map_source 256)
          (to_edn_string (edn_map entries)));
  ]

let edn_parsing_performance_sensitive_behavior_guards_edn_writing_tests =
  [
    Alcotest.test_case "writes atoms" `Quick (fun () ->
        Assert.string
          {|[nil true false "a\nb" \space :k sym 42 1.5 123N 1.20M]|}
          (to_edn_string
             (edn_vector
                [
                  edn_nil;
                  edn_bool true;
                  edn_bool false;
                  edn_string "a\nb";
                  edn_char (Uchar.of_char ' ');
                  edn_keyword "k";
                  edn_symbol "sym";
                  edn_int 42L;
                  edn_float 1.5;
                  edn_bigint "123";
                  edn_decimal "1.20";
                ])));
    Alcotest.test_case "writes collections" `Quick (fun () ->
        Assert.string
          {|{:a 1 "b" [true nil] :s #{x y} :tag #my/app {:ok true}}|}
          (to_edn_string
             (edn_map
                [
                  (edn_keyword "a", edn_int 1L);
                  (edn_string "b", edn_vector [ edn_bool true; edn_nil ]);
                  (edn_keyword "s", edn_set [ edn_symbol "x"; edn_symbol "y" ]);
                  ( edn_keyword "tag",
                    edn_tagged "my/app"
                      (edn_map [ (edn_keyword "ok", edn_bool true) ]) );
                ])));
  ]

let edn_parsing_json_conversion_tests =
  [
    Alcotest.test_case "reads JSON strings" `Quick (fun () ->
        Assert.string
          (to_edn_string json_conversion_edn)
          (to_edn_string
             (of_json_string
                {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|})));
    Alcotest.test_case "writes JSON strings" `Quick (fun () ->
        Assert.string
          {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}
          (to_json_string json_conversion_edn));
    Alcotest.test_case "writes keyword map keys as JSON object names" `Quick
      (fun () ->
        Assert.string {|{"ok":true}|}
          (to_json_string (edn_map [ (edn_keyword "ok", edn_bool true) ])));
    Alcotest.test_case "round trips larger JSON arrays and objects" `Quick
      (fun () ->
        let array_source = large_json_array_source 256 in
        let object_source = large_json_object_source 256 in
        Assert.strings
          [| array_source; object_source |]
          [|
            to_json_string (of_json_string array_source);
            to_json_string (of_json_string object_source);
          |]);
    Alcotest.test_case "keeps Yojson integer literals as EDN bigints" `Quick
      (fun () ->
        Assert.strings
          [| "9007199254740993N"; "123456789012345678901234567890N" |]
          [|
            to_edn_string (Melange_edn.of_json (`Intlit "9007199254740993"));
            to_edn_string
              (Melange_edn.of_json
                 (`Intlit "123456789012345678901234567890"));
          |]);
  ]

let edn_parsing_errors_tests =
  [
    Alcotest.test_case "rejects odd map entry count" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "{:a 1 :b}")));
    Alcotest.test_case "rejects mismatched closing delimiter" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "{:a/b [1 2 3])")));
    Alcotest.test_case "rejects invalid keyword" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "::bad")));
    Alcotest.test_case "rejects slash-only keyword namespace forms" `Quick
      (fun () ->
        let results =
          Array.map
            (fun source ->
              try
                ignore (of_edn_string source);
                "accepted"
              with Parse_error _ -> "rejected")
            [| ":/"; ":/anything" |]
        in
        Assert.strings [| "rejected"; "rejected" |] results);
    Alcotest.test_case "rejects invalid keyword creation inputs" `Quick
      (fun () ->
        let results =
          Array.map
            (fun value ->
              try
                ignore (keyword value);
                "accepted"
              with Parse_error _ -> "rejected")
            [| ""; ":bad"; "/"; "/anything" |]
        in
        Assert.strings
          [| "rejected"; "rejected"; "rejected"; "rejected" |]
          results);
    Alcotest.test_case "rejects symbol creation inputs that look like keywords"
      `Quick (fun () ->
        Assert.raises (fun () -> ignore (symbol ":looks-like-keyword")));
    Alcotest.test_case "rejects discard without value" `Quick (fun () ->
        Assert.raises (fun () -> ignore (of_edn_string "[1 #_]")));
    Alcotest.test_case "rejects non-string JSON object keys" `Quick (fun () ->
        Assert.raises (fun () ->
            ignore
              (to_json_string
                 (edn_map
                    [
                      ( edn_vector
                          [ edn_string "not"; edn_string "a"; edn_string "key" ],
                        edn_int 1L );
                    ]))));
  ]

let () =
  Alcotest.run "melange-edn core"
    [
      ("GADT constructors", gadt_constructors_tests);
      ("EDN parsing", edn_parsing_tests);
      ( "EDN parsing / cljs.reader default date and UUID tags",
        edn_parsing_cljs_reader_default_date_and_uuid_tags_tests );
      ( "EDN parsing / cljs.reader compatibility cases",
        edn_parsing_cljs_reader_compatibility_cases_tests );
      ( "EDN parsing / additional cljs.reader compatibility cases",
        edn_parsing_additional_cljs_reader_compatibility_cases_tests );
      ( "EDN parsing / remaining cljs.reader compatibility cases",
        edn_parsing_remaining_cljs_reader_compatibility_cases_tests );
      ( "EDN parsing / upstream no-runtime compatibility cases",
        edn_parsing_upstream_no_runtime_compatibility_cases_tests );
      ( "EDN parsing / performance-sensitive behavior guards",
        edn_parsing_performance_sensitive_behavior_guards_tests );
      ( "EDN parsing / performance-sensitive behavior guards / EDN writing",
        edn_parsing_performance_sensitive_behavior_guards_edn_writing_tests );
      ("EDN parsing / JSON conversion", edn_parsing_json_conversion_tests);
      ("EDN parsing / errors", edn_parsing_errors_tests);
    ]
