module Edn = Melange_edn
module Bridge = Melange_edn_melange

let assert_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let () =
  assert_equal "Melange bridge reads Js.Json.t"
    {|{"name" "Ada" "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Bridge.of_json
          (Js.Json.parseExn {|{"name":"Ada","tags":["ocaml",null]}|})));
  assert_equal "Melange bridge writes Js.Json.t"
    {|{"name":"Ada","tags":["ocaml",null]}|}
    (Js.Json.stringify
       (Bridge.to_json
          (Bridge.of_json_string {|{"name":"Ada","tags":["ocaml",null]}|})));
  assert_equal "Melange bridge keeps JSON string helpers"
    {|{"ok":true}|}
    (Bridge.to_json_string (Bridge.of_json_string {|{"ok":true}|}))
