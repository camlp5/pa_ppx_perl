open OUnit2


let test_simple ctxt =
  let printer x = x in
  assert_equal ~printer "abc"  (Re.Group.get ([%match "abc"/exc] "abc") 0)
  ; assert_equal None  ([%match "abc"] "abd")
  ; assert_equal None  ([%match "abc"/strings] "abd")
  ; assert_equal "abc"  ([%match "abc"/exc strings] "abc")
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings] "abc")
  ; assert_equal ("ac", None)  ([%match "a(?:(b)?)c"/exc strings] "ac")

let suite = "Test pa_ppx_string" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

