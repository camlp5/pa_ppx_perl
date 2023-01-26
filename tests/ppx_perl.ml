open OUnit2


let test_simple_match ctxt =
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
  ; assert_equal ("abc", Some "a", Some "b", Some "c")  ([%match "(a)(b)(c)"/exc strings] "abc")

let test_search ctxt =
  ()
  ; assert_equal "abc"  ([%match "abc"/exc strings] "zzzabc")
  ; assert_equal None  ([%match "^abc"/strings] "zzzabc")


let test_simple_split ctxt =
  ()
  ; assert_equal ["bb"]  ([%split "a"] "bb")

let test_delim_split ctxt =
  ()
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text "b"; `Delim"a"]  ([%split "a"/ strings] "ababa")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text ""; `Delim"a"; `Text "b"; `Delim"a"]  ([%split "a"/ strings] "abaaba")
  ; assert_equal [`Delim("a",None); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("a",None)]  ([%split "a(c)?"/ strings] "abacba")


let test_patsubst ctxt =
  ()
  ; assert_equal "abc"  (let s = "abc" in ([%pattern s] ([%match "abc"/exc] "abc")))
  ; assert_equal "$b"  ([%pattern {|$$$1|}] ([%match "a(b)c"/exc] "abc"))
  ; assert_equal "b"  ([%pattern {|${01}|}] ([%match "a(b)c"/exc] "abc"))
  ; assert_equal "bx"  (let s = "x" in [%pattern {|${01}${s}|}] ([%match "a(b)c"/exc] "abc"))


let suite = "Test pa_ppx_string" >::: [
      "simple_match"   >:: test_simple_match
    ; "search"   >:: test_search
    ; "simple_split"   >:: test_simple_split
    ; "delim_split"   >:: test_delim_split
    ; "patsubst"   >:: test_patsubst
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

