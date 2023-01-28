(** -syntax camlp5o *)
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

let test_string_pattern ctxt =
  ()
  ; assert_equal "$b"  ([%pattern {|$$$1|}] ([%match "a(b)c"/exc] "abc"))
  ; assert_equal "b"  ([%pattern {|${01}|}] ([%match "a(b)c"/exc] "abc"))
  ; assert_equal "bx"  (let s = "x" in [%pattern {|${01}${s}|}] ([%match "a(b)c"/exc] "abc"))
  ; assert_equal {|"bx|}  (let s = "x" in [%pattern {|"${01}${s}|}] ([%match "a(b)c"/exc] "abc"))

let test_expr_pattern ctxt =
  ()
  ; assert_equal "abc"  ([%pattern "$0$" / e] ([%match "abc"/exc] "abc"))
  ; assert_equal "abcx"  ([%pattern {|$0$ ^ "x"|} / e] ([%match "abc"/exc] "abc"))
  ; assert_equal "abcx"  (let x = "x" in [%pattern {|$0$ ^ x|} / e] ([%match "abc"/exc] "abc"))

let test_string_subst ctxt =
  ()
  ; assert_equal "$b"  ([%subst "a(b)c" / {|$$$1|}] "abc")
  ; assert_equal "$b"  ([%subst "A(B)C" / {|$$$1|} / i] "abc")
  ; assert_equal "$babc"  ([%subst "A(B)C" / {|$$$1|} / i] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|$$$1|} / g i] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|"$" ^ $1$|} / e g i] "abcabc")

let test_ocamlfind_bits ctxt =
  ()
  ; assert_equal (Some " -syntax camlp5o ")
      (snd ([%match {|^\(\*\*(.*?)\*\)|} / exc strings]
       {|(** -syntax camlp5o *)
|}))

let envsubst envlookup s =
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false in

  [%subst {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|} / {| f $1$ $2$ |} / g e] s

let test_envsubst_via_replace ctxt =
  let f = function "A" -> "res1" | "B" -> "res2" in
  assert_equal "...res1...res2..." (envsubst f {|...$(A)...${B}...|})

let suite = "Test pa_ppx_perl" >::: [
      "simple_match"   >:: test_simple_match
    ; "search"   >:: test_search
    ; "simple_split"   >:: test_simple_split
    ; "delim_split"   >:: test_delim_split
    ; "string_pattern"   >:: test_string_pattern
    ; "expr_pattern"   >:: test_expr_pattern
    ; "string_subst"   >:: test_string_subst
    ; "ocamlfind bits"   >:: test_ocamlfind_bits
    ; "envsubst via replace"   >:: test_envsubst_via_replace
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

