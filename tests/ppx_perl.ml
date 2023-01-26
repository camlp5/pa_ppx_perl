open OUnit2


let test_simple ctxt =
  ()
  ; assert_equal "abc"  (Re.Group.get ([%match "abc"/exc] "abc") 0)
  ; assert_equal "abc"  (Re.Group.get ([%match "abc"/exc group] "abc") 0)
  ; assert_equal (Some "abc")  ([%match "abc"/strings] "abc")
  ; assert_equal None  ([%match "abc"] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc strings] "abd")
  ; assert_equal None  ([%match "abc"/strings] "abd")
  ; assert_equal "abc"  ([%match "abc"/exc strings] "abc")
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings] "abc")
  ; assert_equal ("ac", None)  ([%match "a(?:(b)?)c"/exc strings] "ac")
  ; assert_equal "abc"  (Re.Group.get ([%match "ABC"/exc i] "abc") 0)

let suite = "Test pa_ppx_string" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

