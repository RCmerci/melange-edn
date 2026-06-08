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

let () =
  Jest.describe "EDN reader/writer" (fun () ->
      Jest.describe "GADT constructors" (fun () ->
          let typed_bool : bool Melange_edn.t = bool true in
          let typed_int : number Melange_edn.t = int 42L in
          let typed_bigint : number Melange_edn.t = bigint "42" in
          let typed_float : number Melange_edn.t = float 42.5 in
          let typed_decimal : number Melange_edn.t = decimal "42.5" in
          let typed_string : string Melange_edn.t = string "typed" in
          let typed_char : char_ Melange_edn.t = char (Uchar.of_char 'x') in
          let typed_symbol : symbol Melange_edn.t = symbol "typed/symbol" in
          let typed_keyword : keyword Melange_edn.t = keyword "typed/keyword" in
          let typed_list : list_ Melange_edn.t =
            list [ edn typed_symbol; edn typed_keyword ]
          in
          let typed_set : set Melange_edn.t =
            set
              [ edn typed_int; edn typed_bigint; edn typed_float; edn typed_decimal ]
          in
          let typed_map : map Melange_edn.t =
            map [ (edn typed_keyword, edn typed_list) ]
          in
          let typed_vector : vector Melange_edn.t =
            vector [ edn typed_bool; edn typed_int; edn typed_string ]
          in
          Jest.test "writes existential typed vectors" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (edn typed_vector))
                |> toEqual {|[true 42 "typed"]|}));
          Jest.test "writes existential typed chars" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (edn typed_char)) |> toEqual {|\x|}));
          Jest.test "keeps typed collection constructors distinct" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (edn typed_map))
                |> toEqual {|{:typed/keyword (typed/symbol :typed/keyword)}|}));
          Jest.test "keeps typed set constructor distinct" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (edn typed_set))
                |> toEqual {|#{42 42N 42.5 42.5M}|}));
          Jest.test "exposes keyword names through an accessor" (fun () ->
              let constructed =
                match keyword "typed/keyword" with
                | Keyword value -> value
                | _ -> .
              in
              Jest.Expect.(
                expect (keyword_to_string constructed) |> toEqual "typed/keyword")));
      Jest.describe "EDN parsing" (fun () ->
          Jest.test "parses nil" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string "nil")) |> toEqual "nil"));
          Jest.test "parses booleans" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "true");
                    to_edn_string (of_edn_string "false");
                  |]
                |> toEqual [| "true"; "false" |]));
          Jest.test "parses string escapes" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string {| "a\tb\n\"c\"\\" |}))
                |> toEqual {|"a\tb\n\"c\"\\"|}));
          Jest.test "parses unicode string escapes" (fun () ->
              Jest.Expect.(
                expect (to_edn_string (of_edn_string {| "snowman: \u2603" |}))
                |> toEqual {|"snowman: ☃"|}));
          Jest.test "parses character literals" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string {|\x|});
                    to_edn_string (of_edn_string {|\space|});
                    to_edn_string (of_edn_string {|\u2603|});
                  |]
                |> toEqual [| {|\x|}; {|\space|}; {|\u2603|} |]));
          Jest.test "parses symbols and keywords" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "my.ns/name");
                    to_edn_string (of_edn_string ":my.ns/name");
                  |]
                |> toEqual [| "my.ns/name"; ":my.ns/name" |]));
          Jest.test "parses abstract keyword values" (fun () ->
              let parsed_name =
                match of_edn_string ":my.ns/name" with
                | Any (Keyword value) -> keyword_to_string value
                | _ -> "not a keyword"
              in
              Jest.Expect.(expect parsed_name |> toEqual "my.ns/name"));
          Jest.test "parses numeric literals" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "+42");
                    to_edn_string (of_edn_string "-0");
                    to_edn_string
                      (of_edn_string "123456789012345678901234567890N");
                    to_edn_string (of_edn_string "6.02e23");
                    to_edn_string (of_edn_string "1.20M");
                  |]
                |> toEqual
                     [|
                       "42";
                       "0";
                       "123456789012345678901234567890N";
                       "6.02e+23";
                       "1.20M";
                     |]));
          Jest.test "keeps malformed numeric tokens as symbols" (fun () ->
              Jest.Expect.(
                expect
                  [|
                    to_edn_string (of_edn_string "1.");
                    to_edn_string (of_edn_string "1e");
                  |]
                |> toEqual [| "1."; "1e" |]));
          Jest.test "parses collections, comments, commas, and discard"
            (fun () ->
              let source =
                {|
      [a b #_foo 42 ; comments run to the end of the line
       {:a 1, "b" [true nil] :c #{foo \space}}]
    |}
              in
              Jest.Expect.(
                expect (to_edn_string (of_edn_string source))
                |> toEqual {|[a b 42 {:a 1 "b" [true nil] :c #{foo \space}}]|}));
          Jest.test "parses tagged values" (fun () ->
              Jest.Expect.(
                expect
                  (to_edn_string
                     (of_edn_string {|#inst "1985-04-12T23:20:50.52Z"|}))
                |> toEqual {|#inst "1985-04-12T23:20:50.52Z"|}));
          Jest.test "reads all values" (fun () ->
              Jest.Expect.(
                expect
                  (to_edn_string
                     (edn_list (of_edn_string_all "1 #_ignored 2 :done")))
                |> toEqual "(1 2 :done)"));
          Jest.describe "EDN writing" (fun () ->
              Jest.test "writes atoms" (fun () ->
                  Jest.Expect.(
                    expect
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
                            ]))
                    |> toEqual
                         {|[nil true false "a\nb" \space :k sym 42 1.5 123N 1.20M]|}));
              Jest.test "writes collections" (fun () ->
                  Jest.Expect.(
                    expect
                      (to_edn_string
                         (edn_map
                            [
                              (edn_keyword "a", edn_int 1L);
                              (edn_string "b", edn_vector [ edn_bool true; edn_nil ]);
                              ( edn_keyword "s",
                                edn_set [ edn_symbol "x"; edn_symbol "y" ] );
                              ( edn_keyword "tag",
                                edn_tagged "my/app"
                                  (edn_map [ (edn_keyword "ok", edn_bool true) ])
                              );
                            ]))
                    |> toEqual
                         {|{:a 1 "b" [true nil] :s #{x y} :tag #my/app {:ok true}}|})));
          Jest.describe "JSON conversion" (fun () ->
              let edn =
                edn_map
                  [
                    (edn_string "name", edn_string "Ada");
                    (edn_string "age", edn_int 37L);
                    (edn_string "large", edn_int 3000000000L);
                    (edn_string "admin", edn_bool false);
                    (edn_string "tags", edn_vector [ edn_string "ocaml"; edn_nil ]);
                  ]
              in
              Jest.test "reads JSON strings" (fun () ->
                  Jest.Expect.(
                    expect
                      (to_edn_string
                         (of_json_string
                            {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}))
                    |> toEqual (to_edn_string edn)));
              Jest.test "writes JSON strings" (fun () ->
                  Jest.Expect.(
                    expect (to_json_string edn)
                    |> toEqual
                         {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}));
              Jest.test "writes keyword map keys as JSON object names"
                (fun () ->
                  Jest.Expect.(
                    expect
                      (to_json_string
                         (edn_map [ (edn_keyword "ok", edn_bool true) ]))
                    |> toEqual {|{"ok":true}|})));
          Jest.describe "errors" (fun () ->
              Jest.test "rejects trailing forms" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "1 2")) ()
                    |> toThrow));
              Jest.test "rejects odd map entry count" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "{:a 1 :b}")) ()
                    |> toThrow));
              Jest.test "rejects mismatched closing delimiter" (fun () ->
                  Jest.Expect.(
                    expectFn
                      (fun () -> ignore (of_edn_string "{:a/b [1 2 3])"))
                      ()
                    |> toThrow));
              Jest.test "rejects invalid keyword" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "::bad")) ()
                    |> toThrow));
              Jest.test "rejects slash-only keyword namespace forms" (fun () ->
                  let results =
                    Array.map
                      (fun source ->
                        try
                          ignore (of_edn_string source);
                          "accepted"
                        with Parse_error _ -> "rejected")
                      [| ":/"; ":/anything" |]
                  in
                  Jest.Expect.(
                    expect results |> toEqual [| "rejected"; "rejected" |]));
              Jest.test "rejects invalid keyword creation inputs" (fun () ->
                  let results =
                    Array.map
                      (fun value ->
                        try
                          ignore (keyword value);
                          "accepted"
                        with Parse_error _ -> "rejected")
                      [| ""; ":bad"; "/"; "/anything" |]
                  in
                  Jest.Expect.(
                    expect results
                    |> toEqual
                         [| "rejected"; "rejected"; "rejected"; "rejected" |]));
              Jest.test "rejects symbol creation inputs that look like keywords"
                (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (symbol ":looks-like-keyword")) ()
                    |> toThrow));
              Jest.test "rejects discard without value" (fun () ->
                  Jest.Expect.(
                    expectFn (fun () -> ignore (of_edn_string "[1 #_]")) ()
                    |> toThrow));
              Jest.test "rejects non-string JSON object keys" (fun () ->
                  Jest.Expect.(
                    expectFn
                      (fun () ->
                        ignore
                          (to_json_string
                             (edn_map
                                [
                                  ( edn_vector
                                      [
                                        edn_string "not";
                                        edn_string "a";
                                        edn_string "key";
                                      ],
                                    edn_int 1L );
                                ])))
                      ()
                    |> toThrow)))))
