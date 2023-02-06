(** -syntax camlp5o *)
open OUnit2
open Pa_ppx_testutils
open Papr_util

let matches ~pattern text =
  match Str.search_forward (Str.regexp pattern) text 0 with
    _ -> true
  | exception Not_found -> false


let assert_raises_exn_pattern pattern f =
  Testutil.assert_raises_exn_pred
    (function
       Ploc.Exc( _, Failure msg) when matches ~pattern msg -> true
     | exc when matches ~pattern (Printexc.to_string exc) -> true
     | _ -> false
    )
    f

let test_match ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/exc raw] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/ strings] |foo} ; ())
  ; assert_raises_exn_pattern "match extension.*at most one of.*strings.*raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/raw strings] |foo})
  ; assert_raises_exn_pattern "match extension.*at most one of.*strings.*raw.*pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/raw pred] |foo})
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/m] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/s] |foo} ; ())
  ; assert_raises_exn_pattern "match extension.*at most one of.*<<s>>.*<<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/m s] |foo})
  ; assert_raises_exn_pattern "match extension.*forbidden.*: e"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/e] |foo})
  ; assert_raises_exn_pattern "match extension.*forbidden.*: g"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/g] |foo})

let test_busted_regexps ctxt =
  ()
  ; assert_raises_exn_pattern "Re__Perl.*Parse_error"
      (fun () -> PAPR.Implem.pa1 {foo| [%match {|\n$|} /s] |foo})

let test_split ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/exc raw] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/ strings] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"] |foo} ; ())
  ; assert_raises_exn_pattern "split extension.*at most one of.*strings.*raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/raw strings] |foo})
  ; assert_raises_exn_pattern "split extension.*forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/pred] |foo})
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/m] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%split "abc"/s] |foo} ; ())
  ; assert_raises_exn_pattern "split extension.*at most one of.*<<s>>.*<<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/m s] |foo})
  ; assert_raises_exn_pattern "split extension.*forbidden.*: e"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/e] |foo})
  ; assert_raises_exn_pattern "split extension.*forbidden.*: g"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "abc"/g] |foo})
  ; assert_raises_exn_pattern "split extension: must specify one of <<strings>>, <<raw>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%split "a(b)c"] |foo})

let test_pattern ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%pattern "abc"] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%pattern "abc" / e] |foo} ; ())
  ; assert_raises_exn_pattern "pattern extension: forbidden option: s"
      (fun () -> PAPR.Implem.pa1 {foo| [%pattern "abc"/s] |foo})
  ; assert_raises_exn_pattern "pattern extension: forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%pattern "abc"/pred] |foo})

let test_subst ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%subst "abc" / "def"] |foo} ; ())
  ; assert_raises_exn_pattern "subst extension: forbidden option: raw"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /raw] |foo})
  ; assert_raises_exn_pattern "subst extension: forbidden option: pred"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /pred] |foo})
  ; assert_raises_exn_pattern "subst extension: can specify at most one of <<s>>, <<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%subst "abc" / "def" /s m] |foo})

let suite = "Test pa_ppx_regexp syntax" >::: [
      "match"   >:: test_match
    ; "split"   >:: test_split
    ; "pattern"   >:: test_pattern
    ; "subst"   >:: test_subst
    ; "busted regexps"   >:: test_busted_regexps
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

