(** -syntax camlp5o $(MIGRATE_CFLAGS) -package pa_ppx.import,pa_ppx_migrate *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil


exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@add [%%import: 'a Ploc.vala]]
    [@with Ploc.vala := vala]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = MLast
        ; dstmod = MLast
        ; types = [
            class_infos
          ; longid
          ; ctyp
          ; poly_variant
          ; patt
          ; expr
          ; case_branch
          ; module_type
          ; functor_parameter
          ; sig_item
          ; with_constr
          ; module_expr
          ; str_item
          ; type_decl
          ; generic_constructor
          ; extension_constructor
          ; type_extension
          ; class_type
          ; class_sig_item
          ; class_expr
          ; class_str_item
          ; longid_lident
          ; payload
          ; attribute_body
          ; attribute
          ; attributes_no_anti
          ; attributes
          ; type_var
          ; vala
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: MLast.loc]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let parse_expr s =
  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s)

let parse_antiquot_expr s =
    Ploc.call_with Plexer.force_antiquot_loc true
    (Grammar.Entry.parse Pcaml.expr_eoi) (Stream.of_string s)

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
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
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
    <:expr< function `Text s -> `Text s
                | `Delim __g__ -> `Delim $exp:group_tuple$ >> in
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
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
  let result = build_result loc return_type ngroups in
  <:expr< let __re__ = $exp:regexp_expr$ in
          fun __subj__->
            $exp:result$ >>
end


module Pattern = struct

let string_parts_pattern = Re.Perl.compile_pat {|\$\$|\$([0-9]+)|\$\{([0-9]+)\}|\$\{([^}]+)\}|}

let build_string loc patstr =
  let parts = Re.split_full string_parts_pattern patstr in
  let parts_exps =
    parts |> List.map (function
                   `Text s -> <:expr< $str:s$ >>
                 | `Delim g ->
                    match (Re.Group.get_opt g 0, Re.Group.get_opt g 1, Re.Group.get_opt g 2, Re.Group.get_opt g 3) with
                      (Some "$$", _, _, _) -> let dollar = "$" in <:expr< $str:dollar$ >>
                    | (_, Some nstr, _, _)
                    | (_, _, Some nstr, _) -> <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
                    | (_, _, _, Some exps) ->
                       parse_expr exps
                    | _ -> Fmt.(raise_failwithf loc "pa_ppx_perl: unrecognized pattern: <<%a>>" Dump.string patstr)
               ) in
  let listexpr = convert_up_list_expr loc parts_exps in
  <:expr< fun __g__ -> String.concat "" $exp:listexpr$ >>

let build_expr loc patstr =
  let e = parse_antiquot_expr patstr in
  Fmt.(pf stderr "build_expr: <<%a>>\n%!" Pp_MLast.pp_expr e) ;
  let dt = make_dt () in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      ExXtr(loc, antiquot, _) ->
       let (nstr,_) = Std.sep_last (String.split_on_char ':' antiquot) in
       <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
    | e -> old_migrate_expr dt e in
  let dt = { dt with migrate_expr = migrate_expr } in
  let e = dt.migrate_expr dt e in
  <:expr< fun __g__ -> $exp:e$ >>

let build_pattern loc ~options patstr =
  let patstr = Scanf.unescaped patstr in
  Fmt.(pf stderr "build_pattern: <<%a>>\n%!" Dump.string patstr) ;
  if List.mem "e" options then
    build_expr loc patstr
  else
    build_string loc patstr

end

let extract_options e =
  let conv1 = function
      <:expr< $lid:s$ >> -> s
    | e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "extract_options: malformed option: <<%a>>" Pp_MLast.pp_expr e) in
  let (f,l) = Expr.unapplist e in
  (conv1 f)::(List.map conv1 l)

let rewrite_match arg = function
  <:expr:< [%match $str:s$ ;] >> -> Match.build_regexp loc ~options:[] s
| <:expr:< [%match $str:s$ / $exp:optexpr$ ;] >> ->
   let options = extract_options optexpr in
   Match.build_regexp loc ~options s
| _ -> assert false

let rewrite_split arg = function
  <:expr:< [%split $str:s$ ;] >> -> Split.build_regexp loc ~options:[] s
| <:expr:< [%split $str:s$ / $exp:optexpr$ ;] >> ->
   let options = extract_options optexpr in
   Split.build_regexp loc ~options s
| _ -> assert false

let rewrite_pattern arg = function
  <:expr:< [%pattern $str:s$ ;] >> -> Pattern.build_pattern loc ~options:[] s
| <:expr:< [%pattern $str:s$ / $exp:optexpr$ ;] >> ->
   let options = extract_options optexpr in
   Pattern.build_pattern loc ~options s
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "Pa_perl.rewrite_pattern: unsupported extension <<%a>>"
            Pp_MLast.pp_expr e)

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%match $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_match arg z)
  | <:expr:< [%split $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_split arg z)
  | <:expr:< [%pattern $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_pattern arg z)
  ] } in
  Pa_passthru.(install { name = "pa_perl"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
