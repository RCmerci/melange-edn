let json_tags_source = {|{"name":"Ada","tags":["ocaml",null]}|}
let edn_tags_source = {|{:name "Ada" :tags ["ocaml" nil]}|}
let edn_tags_from_json = {|{"name" "Ada" "tags" ["ocaml" nil]}|}
let json_ok_source = {|{"ok":true}|}
let json_mixed_source = {|{"nil":null,"false":false,"true":true,"string":"Ada","int":37,"float":1.5,"array":[1,"two",null],"object":{"nested":true}}|}
let edn_mixed_from_json = {|{"nil" nil "false" false "true" true "string" "Ada" "int" 37 "float" 1.5 "array" [1 "two" nil] "object" {"nested" true}}|}
let edn_mixed_roundtrip_source = {|{"nil" nil "false" false "true" true "string" "Ada" "int" 37 "float" 1.5 "array" [1 "two" nil] "object" {"nested" true}}|}
let json_mixed_roundtrip = {|{"nil":null,"false":false,"true":true,"string":"Ada","int":37,"float":1.5,"array":[1,"two",null],"object":{"nested":true}}|}
let edn_json_bridge_source = {|{"keyword-value" :ok "symbol-value" user/name "char-value" \space "tagged" #inst "2020-01-02T03:04:05Z" "set" #{1 2} "list" (3 4) "decimal" 1.20M "ratio" 3/4 "bigint" 9007199254740992N "unsafe-int" 9007199254740992 "nan" ##NaN "pos-inf" ##Inf "neg-inf" ##-Inf}|}
let json_edn_values_source = {|{"keyword-value":":ok","symbol-value":"user/name","char-value":"\\space","tagged":{"tag":"inst","value":"2020-01-02T03:04:05Z"},"set":[1,2],"list":[3,4],"decimal":"1.20","ratio":"3/4","bigint":"9007199254740992","unsafe-int":"9007199254740992","nan":"NaN","pos-inf":"Infinity","neg-inf":"-Infinity"}|}

let assert_string label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)
