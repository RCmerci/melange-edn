module Edn = Melange_edn_native

let edn value = Edn.any value

let () =
  let json_bridge_value =
    Edn.of_edn_string Test_support.edn_json_bridge_source
  in
  let json_mixed_roundtrip_value =
    Edn.of_edn_string Test_support.edn_mixed_roundtrip_source
  in
  let from_edn = Edn.of_edn_string Test_support.edn_tags_source in
  Test_support.assert_string "native JSON support writes EDN values"
    Test_support.edn_tags_source
    (Edn.to_edn_string from_edn);
  Test_support.assert_string "reads JSON strings"
    {|{"name" "Ada" "age" 37 "large" 3000000000 "admin" false "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Edn.of_json_string
          {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}));
  Test_support.assert_string "writes JSON strings"
    {|{"name":"Ada","age":37,"large":3000000000,"admin":false,"tags":["ocaml",null]}|}
    (Edn.to_json_string
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
  Test_support.assert_string "writes keyword map keys as JSON object names"
    Test_support.json_ok_source
    (Edn.to_json_string
       (edn (Edn.map [ (edn (Edn.keyword "ok"), edn (Edn.bool true)) ])));
  Test_support.assert_string "converts Yojson.Safe.t values"
    {|{"name" "Ada" "active" true "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Edn.of_json
          (`Assoc
            [
              ("name", `String "Ada");
              ("active", `Bool true);
              ("tags", `List [ `String "ocaml"; `Null ]);
            ])));
  Test_support.assert_string "of_json_string reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string (Edn.of_json_string Test_support.json_mixed_source));
  Test_support.assert_string "of_json reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string
       (Edn.of_json (Yojson.Safe.from_string Test_support.json_mixed_source)));
  Test_support.assert_string "to_json_string writes EDN values consistently"
    Test_support.json_edn_values_source
    (Edn.to_json_string json_bridge_value);
  Test_support.assert_string "to_json writes EDN values consistently"
    Test_support.json_edn_values_source
    (Yojson.Safe.to_string (Edn.to_json json_bridge_value));
  Test_support.assert_string "to_json_string writes mixed JSON values consistently"
    Test_support.json_mixed_roundtrip
    (Edn.to_json_string json_mixed_roundtrip_value);
  (try
     ignore
       (Edn.to_json_string
          (edn
             (Edn.map
                [
                  ( edn (Edn.vector [ edn (Edn.string "not"); edn (Edn.string "key") ]),
                    edn (Edn.int 1L) );
                ])));
     failwith "non-string JSON object keys should fail"
   with Invalid_argument _ -> ())
