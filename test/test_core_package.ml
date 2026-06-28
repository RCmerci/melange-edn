module Edn = Melange_edn

let () =
  let value = Edn.of_edn_string {|{:name "Ada" :ok true}|} in
  Test_support.assert_string "core package exposes EDN parser and writer"
    {|{:name "Ada" :ok true}|}
    (Edn.to_edn_string value)
