(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_passthru
open Ppxutil

let parse_expr s =
  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s)

let string_parts_pattern = Re.Perl.compile_pat {|%%|%\{|\{([^}]+)\}|}

let build_string loc patstr =
  let parts = Re.split_full string_parts_pattern patstr in
  let parts_exps =
    parts |> List.map (function
                   `Text s -> <:expr< $str:s$ >>
                 | `Delim g ->
                    match Re.Group.get g 0 with
                      "%%" -> <:expr< $str:"%"$ >>
                    | "%{" -> <:expr< $str:"{"$ >>
                    | _ ->
                       match Re.Group.get_opt g 1 with
                         None -> Fmt.(raise_failwithf loc "pa_ppx_string: internal error in regexp matching: pattern was %a, group was %a"
                                        Dump.string patstr
                                      Re.Group.pp g
                                 )
                       | Some contents ->
                          match String.split_on_char ':' contents with
                            [e] -> parse_expr e

                          | [e;pad] ->
                             let e = parse_expr e in
                             let pad = parse_expr pad in
                             <:expr< Pa_ppx_string.Runtime.pad $exp:e$ $exp:pad$ >>
                          | _ -> Fmt.(raise_failwithf loc "pa_ppx_string: expression %a had more than one colon `:'" Dump.string contents)) in
  let listexpr = convert_up_list_expr loc parts_exps in
  <:expr< String.concat "" $exp:listexpr$ >>

let rewrite_expr arg = function
  <:expr:< [%string $str:s$ ;] >> -> build_string loc s
| _ -> assert false


let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%string $str:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.(install { name = "pa_string"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
