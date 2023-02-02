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
     | _ -> false
    )
    f

let test_match ctxt =
  ()
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/exc group] |foo} ; ())
  ; assert_raises_exn_pattern "build_regexp.*at most one of.*strings.*group"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/group strings] |foo})
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/m] |foo} ; ())
  ; assert_equal ()  (PAPR.Implem.pa1 {foo| [%match "abc"/s] |foo} ; ())
  ; assert_raises_exn_pattern "build_regexp.*at most one of.*<<s>>.*<<m>>"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/m s] |foo})
  ; assert_raises_exn_pattern "build_regexp.*forbidden.*: e"
      (fun () -> PAPR.Implem.pa1 {foo| [%match "abc"/e] |foo})

let suite = "Test pa_ppx_perl syntax" >::: [
      "match"   >:: test_match
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

