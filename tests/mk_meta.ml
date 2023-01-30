(** -syntax camlp5o *)

open Pa_ppx_utils

let pkgmap = [
      "pa_ppx_perl_runtime","pa_ppx_perl.runtime"
    ; "pa_ppx_perl","pa_ppx_perl"
  ]

let indent n txt =
  let pfx = String.make n ' ' in
  [%subst {|^|} / {|${pfx}|} / g m] txt

let fix txt =
  let l = [%split {|\s*,\s*|}] txt in
  let f s =
    match List.assoc s pkgmap with
      exception Not_found -> s
    | v -> v in
  let ol =
    l
    |> List.map (fun p ->
           [%subst {|^([^.]+)|} / {| f $1$ |} / e] p
         ) in
  String.concat "," ol

let fix0 txt =
  [%subst {|"([^"]+)"|} / {| "\"" ^ fix($1$) ^ "\"" |} / e] txt


let fixdeps txt =
  [%subst {|^(.*require.*)$|} / {| fix0($1$) |} / m g e] txt

let capturex (cmd, args) =
  let channel = Unix.open_process_args_in cmd args in
  let txt = Std.read_ic_fully ~channel () in
  close_in channel ;
  txt

let perlmeta = indent 2  (fixdeps(capturex("./pa_perl/META.pl",[||]))) ;;
let rtmeta = indent 2 (fixdeps(capturex("./runtime/META.pl",[||]))) ;;
print_string [%pattern {|${perlmeta}

package "runtime" (
${rtmeta}
)
|}]
