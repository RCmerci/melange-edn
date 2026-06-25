module Edn = Melange_edn
module Json = Melange_edn_yojson

let assert_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let edn value = Edn.any value

let yojson_2_safe_value value : Yojson.Safe.t =
  (* Yojson.Safe.t included these constructors before Yojson 3.0.0. Keep the
     compatibility test compiling when the current switch uses Yojson 3.x. *)
  Obj.magic value

let string_contains contents expected =
  let contents_len = String.length contents in
  let expected_len = String.length expected in
  let rec loop index =
    if index + expected_len > contents_len then false
    else if String.equal (String.sub contents index expected_len) expected then true
    else loop (index + 1)
  in
  expected_len = 0 || loop 0

let assert_file_contains label path expected =
  let path =
    match
      List.find_opt Sys.file_exists
        [ path; Filename.concat ".." path; Filename.concat "../.." path ]
    with
    | Some path -> path
    | None -> failwith (Printf.sprintf "%s: could not find %s" label path)
  in
  let channel = open_in path in
  let len = in_channel_length channel in
  let contents = really_input_string channel len in
  close_in channel;
  if not (string_contains contents expected) then
    failwith (Printf.sprintf "%s: expected %S in %s" label expected path)

let () =
  assert_equal "reads JSON strings"
    {|{"name" "Ada" "age" 37 "large" 3000000000 "admin" false "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Json.of_json_string
          {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}));
  assert_equal "writes JSON strings"
    {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}
    (Json.to_json_string
       (edn
          (Edn.map
             [
               (edn (Edn.string "name"), edn (Edn.string "Ada"));
               (edn (Edn.string "age"), edn (Edn.int 37L));
               (edn (Edn.string "large"), edn (Edn.int 3000000000L));
               (edn (Edn.string "admin"), edn (Edn.bool false));
               ( edn (Edn.string "tags"),
                 edn (Edn.vector [ edn (Edn.string "ocaml"); edn Edn.nil ]) );
             ])));
  assert_equal "writes keyword map keys as JSON object names" {|{"ok":true}|}
    (Json.to_json_string
       (edn (Edn.map [ (edn (Edn.keyword "ok"), edn (Edn.bool true)) ])));
  assert_equal "converts Yojson.Safe.t values"
    {|{"name" "Ada" "active" true "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Json.of_json
          (`Assoc
            [
              ("name", `String "Ada");
              ("active", `Bool true);
              ("tags", `List [ `String "ocaml"; `Null ]);
            ])));
  assert_equal "converts Yojson 2 tuple values" {|["Ada" 37]|}
    (Edn.to_edn_string
       (Json.of_json (yojson_2_safe_value (`Tuple [ `String "Ada"; `Int 37 ]))));
  assert_equal "converts Yojson 2 variants with values" {|#account/enabled true|}
    (Edn.to_edn_string
       (Json.of_json
          (yojson_2_safe_value (`Variant ("account/enabled", Some (`Bool true))))));
  assert_equal "converts Yojson 2 variants without values" {|#Nothing nil|}
    (Edn.to_edn_string
       (Json.of_json (yojson_2_safe_value (`Variant ("Nothing", None)))));
  assert_file_contains "advertises Yojson 2.2.2 support" "melange-edn.opam"
    {|"yojson" {>= "2.2.2"}|};
  (try
     ignore
       (Json.to_json_string
          (edn
             (Edn.map
                [
                  ( edn (Edn.vector [ edn (Edn.string "not"); edn (Edn.string "key") ]),
                    edn (Edn.int 1L) );
                ])));
     failwith "non-string JSON object keys should fail"
   with Invalid_argument _ -> ())
