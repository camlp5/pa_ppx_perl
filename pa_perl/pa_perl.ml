(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let parse_expr s =
  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s)

module Match = struct

type return_type = Strings | Group

let rec build_result loc rty ngroups use_exception =
  let groupnums = Std.range (ngroups-1) in
  let group_exps = groupnums |> List.map (fun n -> <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >>) in
  let group0_exp = <:expr< Re.Group.get __g__ 0 >> in
  let groupl = group0_exp::group_exps in
  let group_tuple = Expr.tuple loc groupl in
  match (rty, use_exception) with
    (Group, false) ->
     <:expr< Re.exec_opt __re__ __subj__ >>
  | (Group, true) ->
     <:expr< Re.exec __re__ __subj__ >>
  | (Strings, true) ->
     let res = build_result loc Group ngroups true in
     <:expr< (fun __g__ -> $exp:group_tuple$ ) $exp:res$ >>
  | (Strings, false) ->
     let res = build_result loc Group ngroups false in
     <:expr< Option.map (fun __g__ -> $exp:group_tuple$ ) $exp:res$ >>

let build_regexp loc ~options restr =
  let case_insensitive = List.mem "i" options in
  let use_exception = List.mem "exc" options in
  let return_type =
    match (List.mem "strings" options, List.mem "group" options) with
      (false, false) -> Group
    | (true, false) -> Strings
    | (false, true) -> Group
    | _ ->
       Fmt.(raise_failwithf loc "Match.build_regexp: can specify at most one of <<strings>>, <<group>>: %a"
            (list Dump.string) options) in

  let re = Re.Perl.compile_pat restr in
  let ngroups = Re.group_count re in
  let compile_opt_expr =
    if case_insensitive then
      <:expr< [`Caseless] >>
    else <:expr< [] >> in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~{opts = $exp:compile_opt_expr$} $str:restr$ >> in
  let result = build_result loc return_type ngroups use_exception in
  <:expr< let __re__ = $exp:regexp_expr$ in
          fun __subj__->
            $exp:result$ >>
end

module Split = struct

type return_type = Nothing | Strings | Group

let rec build_result loc rty ngroups =
  let groupnums = Std.range (ngroups-1) in
  let group_exps = groupnums |> List.map (fun n -> <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >>) in
  let group0_exp = <:expr< Re.Group.get __g__ 0 >> in
  let groupl = group0_exp::group_exps in
  let group_tuple = Expr.tuple loc groupl in
  let converter_fun_exp =
    <:expr< fun [ `Text s -> `Text s
                | `Delim __g__ -> `Delim $exp:group_tuple$ ] >> in
  match rty with
    Nothing ->
     <:expr< Re.split __re__ __subj__ >>
  | Strings ->
     <:expr< List.map $exp:converter_fun_exp$ (Re.split_full __re__ __subj__) >>
  | Group ->
     <:expr< Re.split_full __re__ __subj__ >>

let build_regexp loc ~options restr =
  let re = Re.Perl.compile_pat restr in
  let ngroups = Re.group_count re in
  let case_insensitive = List.mem "i" options in
  let return_type =
    match (List.mem "strings" options, List.mem "group" options) with
      (false, false) when ngroups=1 -> Nothing
    | (false, false) ->
       Fmt.(raise_failwithf loc "Split.build_regexp: must specify one of <<strings>>, <<group>> for regexp with capture groups: %a"
              (list Dump.string) options)
    | (true, false) -> Strings
    | (false, true) -> Group
    | _ ->
       Fmt.(raise_failwithf loc "Split.build_regexp: can specify at most one of <<strings>>, <<group>>: %a"
            (list Dump.string) options) in

  let compile_opt_expr =
    if case_insensitive then
      <:expr< [`Caseless] >>
    else <:expr< [] >> in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~{opts = $exp:compile_opt_expr$} $str:restr$ >> in
  let result = build_result loc return_type ngroups in
  <:expr< let __re__ = $exp:regexp_expr$ in
          fun __subj__->
            $exp:result$ >>
end

let extract_options e =
  let conv1 = function
      <:expr< $lid:s$ >> -> s
    | e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "extract_options: malformed option: <<%a>>" Pp_MLast.pp_expr e) in
  let (f,l) = Expr.unapplist e in
  (conv1 f)::(List.map conv1 l)

let rewrite_match arg = function
  <:expr:< [%"match" $str:s$ ;] >> -> Match.build_regexp loc ~options:[] s
| <:expr:< [%"match" $str:s$ / $exp:optexpr$ ;] >> ->
   let options = extract_options optexpr in
   Match.build_regexp loc ~options s
| _ -> assert false

let rewrite_split arg = function
  <:expr:< [%"split" $str:s$ ;] >> -> Split.build_regexp loc ~options:[] s
| <:expr:< [%"split" $str:s$ / $exp:optexpr$ ;] >> ->
   let options = extract_options optexpr in
   Split.build_regexp loc ~options s
| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%"match" $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_match arg z)
  | <:expr:< [%"split" $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_split arg z)
  ] } in
  Pa_passthru.(install { name = "pa_perl"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
