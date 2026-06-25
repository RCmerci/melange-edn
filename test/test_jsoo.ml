open Js_of_ocaml

module Edn = Melange_edn
module Bridge = Melange_edn_jsoo

let assert_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let () =
  let parse source : Bridge.json =
    Js.Unsafe.meth_call Js._JSON "parse"
      [| Js.Unsafe.inject (Js.string source) |]
  in
  let stringify value =
    Js.to_string
      (Js.Unsafe.meth_call Js._JSON "stringify"
         [| Js.Unsafe.inject value |])
  in
  assert_equal "js_of_ocaml bridge reads JS JSON values"
    {|{"name" "Ada" "tags" ["ocaml" nil]}|}
    (Edn.to_edn_string
       (Bridge.of_json (parse {|{"name":"Ada","tags":["ocaml",null]}|})));
  assert_equal "js_of_ocaml bridge writes JS JSON values"
    {|{"name":"Ada","tags":["ocaml",null]}|}
    (stringify
       (Bridge.to_json
          (Bridge.of_json_string {|{"name":"Ada","tags":["ocaml",null]}|})));
  assert_equal "js_of_ocaml bridge keeps JSON string helpers"
    {|{"ok":true}|}
    (Bridge.to_json_string (Bridge.of_json_string {|{"ok":true}|}))
