open OUnit2


let test_simple ctxt =
  let printer x = x in
  assert_equal ~printer "abc"  [%string "abc"]
  ; begin
      let e = "b" in
      assert_equal ~printer "abc"  [%string "a{e}c"]
    end
  ; begin
      let e = "b" in
      let width = 4 in
      assert_equal ~printer "a   bc"  [%string "a{e:width}c"]
    end
  ; begin
      let e = "b1234" in
      let width = 4 in
      assert_equal ~printer "ab1234c"  [%string "a{e:width}c"]
    end

let suite = "Test pa_ppx_string" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

