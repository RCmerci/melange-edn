module Edn = Melange_edn
module Json = Melange_edn_yojson

let assert_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let edn value = Edn.any value

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
