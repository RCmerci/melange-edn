open Js_of_ocaml

module Edn = Melange_edn_jsoo

let () =
  let json_bridge_value =
    Edn.of_edn_string Test_support.edn_json_bridge_source
  in
  let json_mixed_roundtrip_value =
    Edn.of_edn_string Test_support.edn_mixed_roundtrip_source
  in
  let parse source : Edn.json =
    Js.Unsafe.meth_call Js._JSON "parse"
      [| Js.Unsafe.inject (Js.string source) |]
  in
  let stringify value =
    Js.to_string
      (Js.Unsafe.meth_call Js._JSON "stringify"
         [| Js.Unsafe.inject value |])
  in
  let from_edn = Edn.of_edn_string Test_support.edn_tags_source in
  Test_support.assert_string "js_of_ocaml bridge writes EDN values"
    Test_support.edn_tags_source
    (Edn.to_edn_string from_edn);
  Test_support.assert_string "js_of_ocaml bridge reads JS JSON values"
    Test_support.edn_tags_from_json
    (Edn.to_edn_string
       (Edn.of_json (parse Test_support.json_tags_source)));
  Test_support.assert_string "js_of_ocaml bridge writes JS JSON values"
    Test_support.json_tags_source
    (stringify (Edn.to_json (Edn.of_json_string Test_support.json_tags_source)));
  Test_support.assert_string "js_of_ocaml bridge keeps JSON string helpers"
    Test_support.json_ok_source
    (Edn.to_json_string (Edn.of_json_string Test_support.json_ok_source));
  Test_support.assert_string
    "js_of_ocaml of_json_string reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string (Edn.of_json_string Test_support.json_mixed_source));
  Test_support.assert_string
    "js_of_ocaml of_json reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string (Edn.of_json (parse Test_support.json_mixed_source)));
  Test_support.assert_string
    "js_of_ocaml to_json_string writes EDN values consistently"
    Test_support.json_edn_values_source
    (Edn.to_json_string json_bridge_value);
  Test_support.assert_string "js_of_ocaml to_json writes EDN values consistently"
    Test_support.json_edn_values_source
    (stringify (Edn.to_json json_bridge_value));
  Test_support.assert_string
    "js_of_ocaml to_json_string writes mixed JSON values consistently"
    Test_support.json_mixed_roundtrip
    (Edn.to_json_string json_mixed_roundtrip_value)
