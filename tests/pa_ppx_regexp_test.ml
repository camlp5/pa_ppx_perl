(**pp -syntax camlp5o -package pa_ppx.deriving_plugins.std *)
open OUnit2


let test_simple_match ctxt =
  ()
  ; assert_equal "abc"  (Re.Group.get ([%match "abc"/exc raw] "abc") 0)
  ; assert_equal (Some "abc")  ([%match "abc"] "abc")
  ; assert_equal (Some "abc")  ([%match "abc"/strings] "abc")
  ; assert_equal true  ([%match "abc"/pred] "abc")
  ; assert_equal false  ([%match "abc"/pred] "abd")
  ; assert_equal None  ([%match "abc"] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc strings] "abd")
  ; assert_equal None  ([%match "abc"/strings] "abd")
  ; assert_equal "abc"  ([%match "abc"/exc strings] "abc")
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings] "abc")
  ; assert_equal ("ac", None)  ([%match "a(?:(b)?)c"/exc strings] "ac")
  ; assert_equal "abc"  (Re.Group.get ([%match "ABC"/exc raw i] "abc") 0)
  ; assert_equal ("abc", Some "a", Some "b", Some "c")  ([%match "(a)(b)(c)"/exc strings] "abc")

let test_special_char_regexps ctxt =
  ()
  ; assert_equal "\n"  ([%match {|\n$|}/s exc pcre strings] "\n")
  ; assert_equal "\n"  ([%match {|\n$|}/s exc pcre2 strings] "\n")
  ; assert_equal "\n"  ([%match {|\n$|}/s exc strings] "\n")
  ; assert_equal ""  ([%subst {|\n+$|} / {||} /s pcre] "\n\n")
  ; assert_equal ""  ([%subst {|\n+$|} / {||} /s pcre2] "\n\n")
  ; assert_equal ""  ([%subst {|\n+$|} / {||} /s] "\n\n")

let test_pcre_simple_match ctxt =
  ()
  ; assert_equal "abc"  (Pcre.get_substring ([%match "abc"/exc raw pcre] "abc") 0)
  ; assert_equal (Some "abc")  ([%match "abc"/pcre] "abc")
  ; assert_equal (Some "abc")  ([%match "abc"/strings pcre] "abc")
  ; assert_equal true  ([%match "abc"/pred pcre] "abc")
  ; assert_equal false  ([%match "abc"/pred pcre] "abd")
  ; assert_equal None  ([%match "abc"/pcre] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc pcre] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc strings pcre] "abd")
  ; assert_equal None  ([%match "abc"/strings pcre] "abd")
  ; assert_equal "abc"  ([%match "abc"/exc strings pcre] "abc")
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings pcre] "abc")
  ; assert_equal ("ac", None)  ([%match "a(?:(b)?)c"/exc strings pcre] "ac")
  ; assert_equal "abc"  (Pcre.get_substring ([%match "ABC"/exc raw i pcre] "abc") 0)
  ; assert_equal ("abc", Some "a", Some "b", Some "c")  ([%match "(a)(b)(c)"/exc strings pcre] "abc")

let test_pcre2_simple_match ctxt =
  ()
  ; assert_equal "abc"  (Pcre2.get_substring ([%match "abc"/exc raw pcre2] "abc") 0)
  ; assert_equal (Some "abc")  ([%match "abc"/pcre2] "abc")
  ; assert_equal (Some "abc")  ([%match "abc"/strings pcre2] "abc")
  ; assert_equal true  ([%match "abc"/pred pcre2] "abc")
  ; assert_equal false  ([%match "abc"/pred pcre2] "abd")
  ; assert_equal None  ([%match "abc"/pcre2] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc pcre2] "abd")
  ; assert_raises Not_found (fun () -> [%match "abc"/exc strings pcre2] "abd")
  ; assert_equal None  ([%match "abc"/strings pcre2] "abd")
  ; assert_equal "abc"  ([%match "abc"/exc strings pcre2] "abc")
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings pcre2] "abc")
  ; assert_equal ("ac", None)  ([%match "a(?:(b)?)c"/exc strings pcre2] "ac")
  ; assert_equal "abc"  (Pcre2.get_substring ([%match "ABC"/exc raw i pcre2] "abc") 0)
  ; assert_equal ("abc", Some "a", Some "b", Some "c")  ([%match "(a)(b)(c)"/exc strings pcre2] "abc")

let test_selective_match ctxt =
  ()
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings (!0,1)] "abc")
  ; assert_equal ("abc", "b")  ([%match "a(b)c"/exc strings (!0,!1)] "abc")
  ; assert_equal "b"  ([%match "a(b)c"/exc strings !1] "abc")
  ; assert_equal (Some ("abc", "b"))  ([%match "a(b)c"/ strings (!0,!1)] "abc")
  ; assert_equal ("ac", None)  ([%match "a(b)?c"/exc strings (!0,1)] "ac")
  ; assert_raises Not_found  (fun _ -> [%match "a(b)?c"/exc strings (!0,!1)] "ac")
  ; assert_equal None  ([%match "a(b)?c"/ strings (!0,!1)] "ac")

let test_pcre_selective_match ctxt =
  ()
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings (!0,1) pcre] "abc")
  ; assert_equal ("abc", "b")  ([%match "a(b)c"/exc strings (!0,!1) pcre] "abc")
  ; assert_equal "b"  ([%match "a(b)c"/exc strings !1 pcre] "abc")
  ; assert_equal (Some ("abc", "b"))  ([%match "a(b)c"/ strings (!0,!1) pcre] "abc")
  ; assert_equal ("ac", None)  ([%match "a(b)?c"/exc strings (!0,1) pcre] "ac")
  ; assert_raises Not_found  (fun _ -> [%match "a(b)?c"/exc strings (!0,!1) pcre] "ac")
  ; assert_equal None  ([%match "a(b)?c"/ strings (!0,!1) pcre] "ac")

let test_pcre2_selective_match ctxt =
  ()
  ; assert_equal ("abc", Some "b")  ([%match "a(b)c"/exc strings (!0,1) pcre2] "abc")
  ; assert_equal ("abc", "b")  ([%match "a(b)c"/exc strings (!0,!1) pcre2] "abc")
  ; assert_equal "b"  ([%match "a(b)c"/exc strings !1 pcre2] "abc")
  ; assert_equal (Some ("abc", "b"))  ([%match "a(b)c"/ strings (!0,!1) pcre2] "abc")
  ; assert_equal ("ac", None)  ([%match "a(b)?c"/exc strings (!0,1) pcre2] "ac")
  ; assert_raises Not_found  (fun _ -> [%match "a(b)?c"/exc strings (!0,!1) pcre2] "ac")
  ; assert_equal None  ([%match "a(b)?c"/ strings (!0,!1) pcre2] "ac")

let test_search ctxt =
  ()
  ; assert_equal "abc"  ([%match "abc"/exc strings] "zzzabc")
  ; assert_equal None  ([%match "^abc"/strings] "zzzabc")

let test_pcre_search ctxt =
  ()
  ; assert_equal "abc"  ([%match "abc"/exc strings pcre] "zzzabc")
  ; assert_equal None  ([%match "^abc"/strings pcre] "zzzabc")
let test_pcre2_search ctxt =
  ()
  ; assert_equal "abc"  ([%match "abc"/exc strings pcre2] "zzzabc")
  ; assert_equal None  ([%match "^abc"/strings pcre2] "zzzabc")

let test_single ctxt =
let printer = [%show: string option] in
  ()
  ; assert_equal ~printer  None ([%match ".+"] "\n\n")
  ; assert_equal ~printer  None ([%match ".+" / m strings] "\n\n")

  ; assert_equal ~printer  None ([%match ".+"/ strings] "\n\n")
  ; assert_equal ~printer  (Some "\n\n") ([%match ".+"/s strings] "\n\n")
  ; assert_equal ~printer  None ([%match ".+"/m strings] "\n\n")

; let printer x = x in
  ()
  ; assert_equal ~printer  "\n\n" ([%match ".+" / s exc strings] "\n\n")
  ; assert_equal ~printer  "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|}] "abc\ndef")
  ; assert_equal ~printer  "<<abc\ndef>>" ([%subst ".+" / {|<<$0>>|}/s] "abc\ndef")
  ; assert_equal ~printer  "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|}/m] "abc\ndef")

  ; assert_equal ~printer  "<<abc>>\ndef"  ([%subst ".*" / {|<<$0>>|}] "abc\ndef")
  ; assert_equal ~printer  "<<abc>>\n<<def>>"  ([%subst ".*" / {|<<$0>>|} / g] "abc\ndef")
  ; assert_equal ~printer  "<<abc>>\n<<def>>" ([%subst ".+" / {|<<$0>>|} / g] "abc\ndef")
  ; assert_equal ~printer  "<<abc>>a\nc<<aec>>" ([%subst "a.c" / {|<<$0>>|} / g] "abca\ncaec")
  ; assert_equal ~printer  "<<abc>><<a\nc>><<aec>>" ([%subst "a.c" / {|<<$0>>|} / g s] "abca\ncaec")

let test_pcre_single ctxt =
  ()
  ; assert_equal None ([%match ".+"/pcre] "\n\n")
  ; assert_equal "\n\n" ([%match ".+" / s exc pcre strings] "\n\n")
  ; assert_equal None ([%match ".+" / m pcre strings] "\n\n")

  ; assert_equal None ([%match ".+"/ pcre strings] "\n\n")
  ; assert_equal (Some "\n\n") ([%match ".+"/s pcre strings] "\n\n")
  ; assert_equal None ([%match ".+"/m pcre strings] "\n\n")

  ; assert_equal "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|} / pcre] "abc\ndef")
  ; assert_equal "<<abc\ndef>>" ([%subst ".+" / {|<<$0>>|}/s pcre] "abc\ndef")
  ; assert_equal "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|}/m pcre] "abc\ndef")

  ; assert_equal "<<abc>>\ndef"  ([%subst ".*" / {|<<$0>>|} /pcre] "abc\ndef")
  ; assert_equal "<<abc>><<>>\n<<def>><<>>"  ([%subst ".*" / {|<<$0>>|} / g pcre] "abc\ndef")
  ; assert_equal "<<abc>>\n<<def>>" ([%subst ".+" / {|<<$0>>|} / g pcre] "abc\ndef")
  ; assert_equal "<<abc>>a\nc<<aec>>" ([%subst "a.c" / {|<<$0>>|} / g pcre] "abca\ncaec")
  ; assert_equal "<<abc>><<a\nc>><<aec>>" ([%subst "a.c" / {|<<$0>>|} / g s pcre] "abca\ncaec")

let test_pcre2_single ctxt =
let printer = [%show: string option] in
  ()
  ; assert_equal ~printer None ([%match ".+"/pcre2] "\n\n")
  ; assert_equal ~printer None ([%match ".+" / m pcre2 strings] "\n\n")

  ; assert_equal ~printer None ([%match ".+"/ pcre2 strings] "\n\n")
  ; assert_equal ~printer (Some "\n\n") ([%match ".+"/s pcre2 strings] "\n\n")
  ; assert_equal ~printer None ([%match ".+"/m pcre2 strings] "\n\n")

; let printer x = x in
  ()
  ; assert_equal ~printer "\n\n" ([%match ".+" / s exc pcre2 strings] "\n\n")
  ; assert_equal ~printer "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|} / pcre2] "abc\ndef")
  ; assert_equal ~printer "<<abc\ndef>>" ([%subst ".+" / {|<<$0>>|}/s pcre2] "abc\ndef")
  ; assert_equal ~printer "<<abc>>\ndef" ([%subst ".+" / {|<<$0>>|}/m pcre2] "abc\ndef")

  ; assert_equal ~printer "<<abc>>\ndef"  ([%subst ".*" / {|<<$0>>|} /pcre2] "abc\ndef")
  ; assert_equal ~printer "<<abc>><<>>\n<<def>><<>>"  ([%subst ".*" / {|<<$0>>|} / g pcre2] "abc\ndef")
  ; assert_equal ~printer "<<abc>>\n<<def>>" ([%subst ".+" / {|<<$0>>|} / g pcre2] "abc\ndef")
  ; assert_equal ~printer "<<abc>>a\nc<<aec>>" ([%subst "a.c" / {|<<$0>>|} / g pcre2] "abca\ncaec")
  ; assert_equal ~printer "<<abc>><<a\nc>><<aec>>" ([%subst "a.c" / {|<<$0>>|} / g s pcre2] "abca\ncaec")

let test_multiline ctxt =
  ()
  ; assert_equal (Some "bar")  ([%match ".+$"/ strings] "foo\nbar")
  ; assert_equal (Some "foo")  ([%match ".+$"/ m strings] "foo\nbar")

let test_pcre_multiline ctxt =
  ()
  ; assert_equal (Some "bar")  ([%match ".+$"/ strings pcre] "foo\nbar")
  ; assert_equal (Some "foo")  ([%match ".+$"/ m strings pcre] "foo\nbar")
let test_pcre2_multiline ctxt =
  ()
  ; assert_equal (Some "bar")  ([%match ".+$"/ strings pcre2] "foo\nbar")
  ; assert_equal (Some "foo")  ([%match ".+$"/ m strings pcre2] "foo\nbar")

let test_simple_split ctxt =
  ()
  ; assert_equal ["bb"]  ([%split "a"] "bb")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"] ([%split {|(x)|(u)|} / strings re_perl] "abxcd")

let test_pcre_simple_split ctxt =
  ()
  ; assert_equal ["bb"]  ([%split "a"/pcre] "bb")
let test_pcre2_simple_split ctxt =
  ()
  ; assert_equal ["bb"]  ([%split "a"/pcre2] "bb")


let test_delim_split ctxt =
  ()
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text "b"; `Delim"a"]  ([%split "a"/ strings] "ababa")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text ""; `Delim"a"; `Text "b"; `Delim"a"]  ([%split "a"/ strings] "abaaba")
  ; assert_equal [`Delim("a",None); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("a",None)]  ([%split "a(c)?"/ strings] "abacba")
  ; assert_equal [`Delim("ac",Some"c"); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("ac",Some "c")]  ([%split "a(c)"/ strings] "acbacbac")
  ; assert_equal [`Delim"c"; `Text "b";`Delim"c"; `Text "b"; `Delim"c"]  ([%split "a(c)"/ strings !1] "acbacbac")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"ac"; `Text "b"; `Delim"a"]  ([%split "a(c)?"/ strings !0] "abacba")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"] ([%split {|(x)|(u)|} / strings re_perl] "abxcd")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"; `Delim ("u", None, Some "u")] ([%split {|(x)|(u)|} / strings re_perl] "abxcdu")

let test_pcre_delim_split ctxt =
  ()
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text "b"; `Delim"a"] ([%split "a"/pcre strings] "ababa")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text ""; `Delim"a"; `Text "b"; `Delim"a"] ([%split "a"/pcre strings] "abaaba")
  ; assert_equal [`Delim("a",None); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("a",None)] ([%split "a(c)?"/pcre strings] "abacba")
  ; assert_equal [`Delim("ac",Some"c"); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("ac",Some "c")] ([%split "a(c)"/pcre strings] "acbacbac")
  ; assert_equal [`Delim"c"; `Text "b";`Delim"c"; `Text "b"; `Delim"c"] ([%split "a(c)"/pcre strings !1] "acbacbac")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"ac"; `Text "b"; `Delim"a"] ([%split "a(c)?"/pcre strings !0] "abacba")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"] ([%split {|(x)|(u)|} / strings pcre] "abxcd")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"; `Delim ("u", None, Some "u")] ([%split {|(x)|(u)|} / strings pcre] "abxcdu")

let test_pcre_delim_split_raw ctxt =
  let open Pcre in
  ()
  ; assert_equal [Delim "a"; Text "b"; Delim "a"; Text "b"] ([%split "a"/pcre raw] "ababa")
  ; assert_equal [Delim "a"; Text "b"; Delim "a"; Delim "a"; Text "b"] ([%split "a"/pcre raw] "abaaba")
  ; assert_equal [Delim "a"; NoGroup; Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "a"; NoGroup] ([%split "a(c)?"/pcre raw] "abacba")
  ; assert_equal [Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c")] ([%split "a(c)"/pcre raw] "acbacbac")
  ; assert_equal [Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c")] ([%split "a(c)"/pcre raw] "acbacbac")
  ; assert_equal [Delim "a"; NoGroup; Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "a"; NoGroup] ([%split "a(c)?"/pcre raw] "abacba")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"] ([%split {|(x)|(u)|} / raw pcre] "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u"; Group (1, ""); Group (2, "u")] ([%split {|(x)|(u)|} / raw pcre] "abxcdu")

let test_pcre2_delim_split ctxt =
  ()
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text "b"; `Delim"a"] ([%split "a"/pcre2 strings] "ababa")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"a"; `Text ""; `Delim"a"; `Text "b"; `Delim"a"] ([%split "a"/pcre2 strings] "abaaba")
  ; assert_equal [`Delim("a",None); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("a",None)] ([%split "a(c)?"/pcre2 strings] "abacba")
  ; assert_equal [`Delim("ac",Some"c"); `Text "b";`Delim("ac",Some"c"); `Text "b"; `Delim("ac",Some "c")] ([%split "a(c)"/pcre2 strings] "acbacbac")
  ; assert_equal [`Delim"c"; `Text "b";`Delim"c"; `Text "b"; `Delim"c"] ([%split "a(c)"/pcre2 strings !1] "acbacbac")
  ; assert_equal [`Delim"a"; `Text "b";`Delim"ac"; `Text "b"; `Delim"a"] ([%split "a(c)?"/pcre2 strings !0] "abacba")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"] ([%split {|(x)|(u)|} / strings pcre2] "abxcd")
  ; assert_equal [`Text "ab"; `Delim ("x", Some "x", None); `Text "cd"; `Delim ("u", None, Some "u")] ([%split {|(x)|(u)|} / strings pcre2] "abxcdu")

let test_pcre2_delim_split_raw ctxt =
  let open Pcre2 in
  ()
  ; assert_equal [Delim "a"; Text "b"; Delim "a"; Text "b"] ([%split "a"/pcre2 raw] "ababa")
  ; assert_equal [Delim "a"; Text "b"; Delim "a"; Delim "a"; Text "b"] ([%split "a"/pcre2 raw] "abaaba")
  ; assert_equal [Delim "a"; NoGroup; Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "a"; NoGroup] ([%split "a(c)?"/pcre2 raw] "abacba")
  ; assert_equal [Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c")] ([%split "a(c)"/pcre2 raw] "acbacbac")
  ; assert_equal [Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "ac"; Group (1, "c")] ([%split "a(c)"/pcre2 raw] "acbacbac")
  ; assert_equal [Delim "a"; NoGroup; Text "b"; Delim "ac"; Group (1, "c"); Text "b"; Delim "a"; NoGroup] ([%split "a(c)?"/pcre2 raw] "abacba")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"] ([%split {|(x)|(u)|} / raw pcre2] "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u"; NoGroup; Group (2, "u")] ([%split {|(x)|(u)|} / raw pcre2] "abxcdu")

let test_string_pattern ctxt =
  ()
  ; assert_equal "$b"  ([%pattern {|$$$1|}] ([%match "a(b)c"/exc raw] "abc"))
  ; assert_equal "b"  ([%pattern {|${01}|}] ([%match "a(b)c"/exc raw] "abc"))
  ; assert_equal "bx"  (let s = "x" in [%pattern {|${01}${s}|}] ([%match "a(b)c"/exc raw] "abc"))
  ; assert_equal {|"bx|}  (let s = "x" in [%pattern {|"${01}${s}|}] ([%match "a(b)c"/exc raw] "abc"))
  ; assert_equal {|"x|}  (let s = "x" in [%pattern {|"${s}|}])

let test_pcre_string_pattern ctxt =
  ()
  ; assert_equal "$b"  ([%pattern {|$$$1|} /pcre] ([%match "a(b)c"/exc pcre raw] "abc"))
  ; assert_equal "b"  ([%pattern {|${01}|} /pcre] ([%match "a(b)c"/exc pcre raw] "abc"))
  ; assert_equal "bx"  (let s = "x" in [%pattern {|${01}${s}|} /pcre] ([%match "a(b)c"/exc pcre raw] "abc"))
  ; assert_equal {|"bx|}  (let s = "x" in [%pattern {|"${01}${s}|} /pcre] ([%match "a(b)c"/exc pcre raw] "abc"))
  ; assert_equal {|"x|}  (let s = "x" in [%pattern {|"${s}|} /pcre])

let test_pcre2_string_pattern ctxt =
  ()
  ; assert_equal "$b"  ([%pattern {|$$$1|} /pcre2] ([%match "a(b)c"/exc pcre2 raw] "abc"))
  ; assert_equal "b"  ([%pattern {|${01}|} /pcre2] ([%match "a(b)c"/exc pcre2 raw] "abc"))
  ; assert_equal "bx"  (let s = "x" in [%pattern {|${01}${s}|} /pcre2] ([%match "a(b)c"/exc pcre2 raw] "abc"))
  ; assert_equal {|"bx|}  (let s = "x" in [%pattern {|"${01}${s}|} /pcre2] ([%match "a(b)c"/exc pcre2 raw] "abc"))
  ; assert_equal {|"x|}  (let s = "x" in [%pattern {|"${s}|} /pcre2])

let test_expr_pattern ctxt =
  ()
  ; assert_equal "abc"  ([%pattern "$0$" / e] ([%match "abc"/exc raw] "abc"))
  ; assert_equal "abcx"  ([%pattern {|$0$ ^ "x"|} / e] ([%match "abc"/exc raw] "abc"))
  ; assert_equal "abcx"  (let x = "x" in [%pattern {|$0$ ^ x|} / e] ([%match "abc"/exc raw] "abc"))
  ; assert_equal "x"  (let x = "x" in [%pattern {|"" ^ x|} / e])

let test_pcre_expr_pattern ctxt =
  ()
  ; assert_equal "abc"  ([%pattern "$0$" / e pcre] ([%match "abc"/exc pcre raw] "abc"))
  ; assert_equal "abcx"  ([%pattern {|$0$ ^ "x"|} / e pcre] ([%match "abc"/exc pcre raw] "abc"))
  ; assert_equal "abcx"  (let x = "x" in [%pattern {|$0$ ^ x|} / e pcre] ([%match "abc"/exc pcre raw] "abc"))
  ; assert_equal "x"  (let x = "x" in [%pattern {|"" ^ x|} / e pcre])

let test_pcre2_expr_pattern ctxt =
  ()
  ; assert_equal "abc"  ([%pattern "$0$" / e pcre2] ([%match "abc"/exc pcre2 raw] "abc"))
  ; assert_equal "abcx"  ([%pattern {|$0$ ^ "x"|} / e pcre2] ([%match "abc"/exc pcre2 raw] "abc"))
  ; assert_equal "abcx"  (let x = "x" in [%pattern {|$0$ ^ x|} / e pcre2] ([%match "abc"/exc pcre2 raw] "abc"))
  ; assert_equal "x"  (let x = "x" in [%pattern {|"" ^ x|} / e pcre2])

let test_subst ctxt =
  ()
  ; assert_equal "$b"  ([%subst "a(b)c" / {|$$$1|}] "abc")
  ; assert_equal "$b"  ([%subst "A(B)C" / {|$$$1|} / i] "abc")
  ; assert_equal "$babc"  ([%subst "A(B)C" / {|$$$1|} / i] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|$$$1|} / g i] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|"$" ^ $1$|} / e g i] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|"$"|} / e g i] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|$$|} / g i] "abcabc")

let test_pcre_subst ctxt =
  ()
  ; assert_equal "$b"  ([%subst "a(b)c" / {|$$$1|} /pcre] "abc")
  ; assert_equal "$b"  ([%subst "A(B)C" / {|$$$1|} / i pcre] "abc")
  ; assert_equal "$babc"  ([%subst "A(B)C" / {|$$$1|} / i pcre] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|$$$1|} / g i pcre] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|"$" ^ $1$|} / e g i pcre] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|"$"|} / e g i pcre] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|$$|} / g i pcre] "abcabc")

let test_pcre2_subst ctxt =
  ()
  ; assert_equal "$b"  ([%subst "a(b)c" / {|$$$1|} /pcre2] "abc")
  ; assert_equal "$b"  ([%subst "A(B)C" / {|$$$1|} / i pcre2] "abc")
  ; assert_equal "$babc"  ([%subst "A(B)C" / {|$$$1|} / i pcre2] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|$$$1|} / g i pcre2] "abcabc")
  ; assert_equal "$b$b"  ([%subst "A(B)C" / {|"$" ^ $1$|} / e g i pcre2] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|"$"|} / e g i pcre2] "abcabc")
  ; assert_equal "$$"  ([%subst "A(B)C" / {|$$|} / g i pcre2] "abcabc")

let test_ocamlfind_bits ctxt =
  ()
  ; assert_equal ~printer:[%show: string option] (Some "-syntax camlp5o ")
      (snd ([%match {|^\(\*\*pp (.*?)\*\)|} / exc strings]
       {|(**pp -syntax camlp5o *)
|}))

let test_pcre_ocamlfind_bits ctxt =
  ()
  ; assert_equal ~printer:[%show: string option] (Some "-syntax camlp5o ")
      (snd ([%match {|^\(\*\*pp (.*?)\*\)|} / exc strings pcre]
       {|(**pp -syntax camlp5o *)
|}))

let test_pcre2_ocamlfind_bits ctxt =
  ()
  ; assert_equal ~printer:[%show: string option] (Some "-syntax camlp5o ")
      (snd ([%match {|^\(\*\*pp (.*?)\*\)|} / exc strings pcre2]
       {|(**pp -syntax camlp5o *)
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

let pcre_envsubst envlookup s =
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false in

  [%subst {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|} / {| f $1$ $2$ |} / g e pcre] s

let test_pcre_envsubst_via_replace ctxt =
  let f = function "A" -> "res1" | "B" -> "res2" in
  assert_equal "...res1...res2..." (pcre_envsubst f {|...$(A)...${B}...|})

let pcre2_envsubst envlookup s =
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false in

  [%subst {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|} / {| f $1$ $2$ |} / g e pcre2] s

let test_pcre2_envsubst_via_replace ctxt =
  let f = function "A" -> "res1" | "B" -> "res2" in
  assert_equal "...res1...res2..." (pcre2_envsubst f {|...$(A)...${B}...|})

let suite = "Test pa_ppx_regexp" >::: [
      "simple_match"   >:: test_simple_match
    ; "pcre simple_match"   >:: test_pcre_simple_match
    ; "pcre2 simple_match"   >:: test_pcre2_simple_match
    ; "selective_match"   >:: test_selective_match
    ; "pcre selective_match"   >:: test_pcre_selective_match
    ; "pcre2 selective_match"   >:: test_pcre2_selective_match
    ; "search"   >:: test_search
    ; "pcre search"   >:: test_pcre_search
    ; "pcre2 search"   >:: test_pcre2_search
    ; "single"   >:: test_single
    ; "pcre single"   >:: test_pcre_single
    ; "pcre2 single"   >:: test_pcre2_single
    ; "multiline"   >:: test_multiline
    ; "pcre multiline"   >:: test_pcre_multiline
    ; "pcre2 multiline"   >:: test_pcre2_multiline
    ; "simple_split"   >:: test_simple_split
    ; "pcre simple_split"   >:: test_pcre_simple_split
    ; "pcre2 simple_split"   >:: test_pcre2_simple_split
    ; "delim_split"   >:: test_delim_split
    ; "pcre delim_split"   >:: test_pcre_delim_split
    ; "pcre delim_split raw"   >:: test_pcre_delim_split_raw
    ; "pcre2 delim_split raw"   >:: test_pcre2_delim_split_raw
    ; "string_pattern"   >:: test_string_pattern
    ; "pcre string_pattern"   >:: test_pcre_string_pattern
    ; "pcre2 string_pattern"   >:: test_pcre2_string_pattern
    ; "expr_pattern"   >:: test_expr_pattern
    ; "pcre expr_pattern"   >:: test_pcre_expr_pattern
    ; "pcre2 expr_pattern"   >:: test_pcre2_expr_pattern
    ; "subst"   >:: test_subst
    ; "pcre subst"   >:: test_pcre_subst
    ; "pcre2 subst"   >:: test_pcre2_subst
    ; "ocamlfind bits"   >:: test_ocamlfind_bits
    ; "pcre ocamlfind bits"   >:: test_pcre_ocamlfind_bits
    ; "pcre2 ocamlfind bits"   >:: test_pcre2_ocamlfind_bits
    ; "envsubst via replace"   >:: test_envsubst_via_replace
    ; "pcre envsubst via replace"   >:: test_pcre_envsubst_via_replace
    ; "pcre2 envsubst via replace"   >:: test_pcre2_envsubst_via_replace
    ; "pcre only_regexps"   >:: test_special_char_regexps
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

