module Edn = Melange_edn_melange

let () =
  let json_bridge_value =
    Edn.of_edn_string Test_support.edn_json_bridge_source
  in
  let json_mixed_roundtrip_value =
    Edn.of_edn_string Test_support.edn_mixed_roundtrip_source
  in
  let from_edn = Edn.of_edn_string Test_support.edn_tags_source in
  Test_support.assert_string "Melange bridge writes EDN values"
    Test_support.edn_tags_source
    (Edn.to_edn_string from_edn);
  Test_support.assert_string "Melange bridge reads Js.Json.t"
    Test_support.edn_tags_from_json
    (Edn.to_edn_string
       (Edn.of_json (Js.Json.parseExn Test_support.json_tags_source)));
  Test_support.assert_string "Melange bridge writes Js.Json.t"
    Test_support.json_tags_source
    (Js.Json.stringify
       (Edn.to_json (Edn.of_json_string Test_support.json_tags_source)));
  Test_support.assert_string "Melange bridge keeps JSON string helpers"
    Test_support.json_ok_source
    (Edn.to_json_string (Edn.of_json_string Test_support.json_ok_source));
  Test_support.assert_string
    "Melange of_json_string reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string (Edn.of_json_string Test_support.json_mixed_source));
  Test_support.assert_string "Melange of_json reads mixed JSON values consistently"
    Test_support.edn_mixed_from_json
    (Edn.to_edn_string
       (Edn.of_json (Js.Json.parseExn Test_support.json_mixed_source)));
  Test_support.assert_string "Melange to_json_string writes EDN values consistently"
    Test_support.json_edn_values_source
    (Edn.to_json_string json_bridge_value);
  Test_support.assert_string "Melange to_json writes EDN values consistently"
    Test_support.json_edn_values_source
    (Js.Json.stringify (Edn.to_json json_bridge_value));
  Test_support.assert_string
    "Melange to_json_string writes mixed JSON values consistently"
    Test_support.json_mixed_roundtrip
    (Edn.to_json_string json_mixed_roundtrip_value)
