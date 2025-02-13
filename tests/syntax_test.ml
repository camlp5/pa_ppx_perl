(**pp -syntax camlp5o *)
open OUnit2
open Pa_ppx_testutils
open Papr_util

let test_match ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/exc raw] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/ strings] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "match extension.*at most one of.*strings.*raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/raw strings] |foo})
  ; Testutil.assert_raises_exn_pattern "match extension.*at most one of.*strings.*raw.*pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/raw pred] |foo})
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/m] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/s] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "match extension.*at most one of.*<<s>>.*<<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/m s] |foo})
  ; Testutil.assert_raises_exn_pattern "match extension.*forbidden.*: e"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/e] |foo})
  ; Testutil.assert_raises_exn_pattern "match extension.*forbidden.*: g"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/g] |foo})

let test_special_chars ctxt =
  ()
  ; assert_equal ()
      (PAPR.Implem.pa1 {foo| [%match {|\n$|} /s] |foo} ; ())

let test_split ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/exc raw] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/ strings] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "split extension.*at most one of.*strings.*raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/raw strings] |foo})
  ; Testutil.assert_raises_exn_pattern "split extension.*forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/pred] |foo})
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/m] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/s] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "split extension.*at most one of.*<<s>>.*<<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/m s] |foo})
  ; Testutil.assert_raises_exn_pattern "split extension.*forbidden.*: e"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/e] |foo})
  ; Testutil.assert_raises_exn_pattern "split extension.*forbidden.*: g"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/g] |foo})
  ; Testutil.assert_raises_exn_pattern "split extension: must specify one of <<strings>>, <<raw>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "a(b)c"] |foo})

let test_pattern ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%pattern "abc"] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%pattern "abc" / e] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "pattern extension: forbidden option: s"
      (fun () -> PAPR.Implem.pa1 {foo| [%pattern "abc"/s] |foo})
  ; Testutil.assert_raises_exn_pattern "pattern extension: forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%pattern "abc"/pred] |foo})

let test_subst ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%subst "abc" / "def"] |foo} ; ())
  ; Testutil.assert_raises_exn_pattern "subst extension: forbidden option: raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /raw] |foo})
  ; Testutil.assert_raises_exn_pattern "subst extension: forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /pred] |foo})
  ; Testutil.assert_raises_exn_pattern "subst extension: can specify at most one of <<s>>, <<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /s m] |foo})

let suite = "Test pa_ppx_regexp syntax" >::: [
      "match"   >:: test_match
    ; "split"   >:: test_split
    ; "pattern"   >:: test_pattern
    ; "subst"   >:: test_subst
    ; "special chars"   >:: test_special_chars
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

