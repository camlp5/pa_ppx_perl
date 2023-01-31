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

module Options = struct

type t =
  Multi
| Single
| Global
| Insensitive
| Expr
| Group
| Strings
| StringGroups of int list
| Exception [@@deriving show]

let convert e =
  let conv1 e =
    let badarg() = Fmt.(raise_failwithf (MLast.loc_of_expr e) "extract_options: malformed option: <<%a>>" Pp_MLast.pp_expr e) in
    match e with
      <:expr< m >> -> [Multi]
    | <:expr< s >> -> [Single]
    | <:expr< i >> -> [Insensitive]
    | <:expr< e >> -> [Expr]
    | <:expr< g >> -> [Global]
    | <:expr< group >> -> [Group]
    | <:expr< strings >> -> [Strings; StringGroups [0]]
    | <:expr< strings ( $list:l$ ) >> ->
       let l = l |> List.map (function
                          <:expr< $int:n$ >> -> int_of_string n
                        | _ -> badarg ()) in
       [Strings; StringGroups (0::l)]
    | <:expr< exc >> -> [Exception]
    | _ -> badarg() in
  let (f,l) = Expr.unapplist e in
  List.concat_map conv1 (f::l)

let string_groups options =
  match List.find_map (function StringGroups l -> Some l | _ -> None) options with
    Some l -> l
  | None -> failwith "Options.string_groups: internal error"

end

let compile_opts loc options =
  let open Options in
  let case_insensitive = List.mem Insensitive options in
  let dotall = List.mem Single options in
  let multiline = List.mem Multi options in
  let opts = [] in
  let opts = if case_insensitive then <:expr< `Caseless >>::opts else opts in
  let opts = if dotall then <:expr< `Dotall >>::opts else opts in
  let opts = if multiline then <:expr< `Multiline >>::opts else opts in
  convert_up_list_expr loc opts

module Match = struct

let rec build_result loc ~options ngroups use_exception =
  let open Options in
  let groupnums = Std.range (ngroups-1) in
  let group_exps = groupnums |> List.map (fun n -> <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >>) in
  let group0_exp = <:expr< Re.Group.get __g__ 0 >> in
  let groupl = group0_exp::group_exps in
  let group_tuple = Expr.tuple loc groupl in
  if List.mem Group options then
     if use_exception then
       <:expr< Re.exec __re__ __subj__ >>
     else
       <:expr< Re.exec_opt __re__ __subj__ >>
  else
     if use_exception then
       let res = build_result loc ~options:[Group] ngroups true in
       <:expr< (fun __g__ -> $exp:group_tuple$ ) $exp:res$ >>
     else
       let res = build_result loc ~options:[Group] ngroups false in
       <:expr< Option.map (fun __g__ -> $exp:group_tuple$ ) $exp:res$ >>

let build_regexp loc ~options restr =
  let open Options in
  if List.mem Strings options && List.mem Group options then
    Fmt.(raise_failwithf loc "Match.build_regexp: can specify at most one of <<strings>>, <<group>>: %a"
           (list Options.pp) options)
  else
  let use_exception = List.mem Exception options in
  let re = Re.Perl.compile_pat (Scanf.unescaped restr) in
  let ngroups = Re.group_count re in
  let compile_opt_expr = compile_opts loc options in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
  let result = build_result loc ~options ngroups use_exception in
  <:expr< let __re__ = $exp:regexp_expr$ in
          fun __subj__->
            $exp:result$ >>
end

module Split = struct

type return_type = RT_Nothing | RT_Strings | RT_Group

let rec build_result loc ~options ngroups =
  let open Options in
  let groupnums = Std.range (ngroups-1) in
  let group_exps = groupnums |> List.map (fun n -> <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >>) in
  let group0_exp = <:expr< Re.Group.get __g__ 0 >> in
  let groupl = group0_exp::group_exps in
  let group_tuple = Expr.tuple loc groupl in
  let converter_fun_exp =
    <:expr< function `Text s -> `Text s
                | `Delim __g__ -> `Delim $exp:group_tuple$ >> in
  if List.mem Strings options then
    <:expr< List.map $exp:converter_fun_exp$ (Re.split_full __re__ __subj__) >>
  else if List.mem Group options then
    <:expr< Re.split_full __re__ __subj__ >>
  else
    <:expr< Re.split __re__ __subj__ >>

let build_regexp loc ~options restr =
  let open Options in
  if List.mem Strings options && List.mem Group options then
    Fmt.(raise_failwithf loc "Split.build_regexp: can specify at most one of <<strings>>, <<group>>: %a"
           (list Options.pp) options)
  else
  let re = Re.Perl.compile_pat (Scanf.unescaped restr) in
  let ngroups = Re.group_count re in
  if ngroups > 1 && not (List.mem Strings options || List.mem Group options) then
    Fmt.(raise_failwithf loc "Split.build_regexp: must specify one of <<strings>>, <<group>> for regexp with capture groups: %a"
           (list Options.pp) options)
  else

  let compile_opt_expr = compile_opts loc options in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
  let result = build_result loc ~options ngroups in
  <:expr< let __re__ = $exp:regexp_expr$ in
          fun __subj__->
            $exp:result$ >>
end


module Pattern = struct

let string_parts_pattern = Re.Perl.compile_pat {|\$\$|\$([0-9]+)|\$\{([0-9]+)\}|\$\{([^}]+)\}|}

let build_string loc ~force_cgroups patstr =
  let has_cgroups = ref force_cgroups in
  let parts = Re.split_full string_parts_pattern patstr in
  let parts_exps =
    parts |> List.map (function
                   `Text s ->
                    let s = String.escaped s in
                    <:expr< $str:s$ >>
                 | `Delim g ->
                    match (Re.Group.get_opt g 0, Re.Group.get_opt g 1, Re.Group.get_opt g 2, Re.Group.get_opt g 3) with
                      (Some "$$", _, _, _) -> let dollar = "$" in <:expr< $str:dollar$ >>
                    | (_, Some nstr, _, _)
                    | (_, _, Some nstr, _) ->
                       has_cgroups := true ;
                       <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
                    | (_, _, _, Some exps) ->
                       parse_expr exps
                    | _ -> Fmt.(raise_failwithf loc "pa_ppx_perl: unrecognized pattern: <<%a>>" Dump.string patstr)
               ) in
  let listexpr = convert_up_list_expr loc parts_exps in
  if !has_cgroups then
    <:expr< fun __g__ -> String.concat "" $exp:listexpr$ >>
  else
    <:expr< String.concat "" $exp:listexpr$ >>

let build_expr ~force_cgroups loc patstr =
  let has_cgroups = ref force_cgroups in
  let e = parse_antiquot_expr patstr in
  let dt = make_dt () in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      ExXtr(loc, antiquot, _) ->
       let (nstr,_) = Std.sep_last (String.split_on_char ':' antiquot) in
       has_cgroups := true ;
       <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
    | e -> old_migrate_expr dt e in
  let dt = { dt with migrate_expr = migrate_expr } in
  let e = dt.migrate_expr dt e in
  if !has_cgroups then
    <:expr< fun __g__ -> $exp:e$ >>
  else
    e

let build_pattern loc ~force_cgroups ~options patstr =
  let open Options in
  let patstr = Scanf.unescaped patstr in
  if List.mem Expr options then
    build_expr loc ~force_cgroups patstr
  else
    build_string loc ~force_cgroups patstr

end

module Subst = struct
  let build_subst loc ~options restr patstr =
  let open Options in
  let global = List.mem Global options in
  let global = if global then <:expr< true >> else <:expr< false >> in
  let compile_opt_expr = compile_opts loc options in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
  let patexpr = Pattern.build_pattern loc ~force_cgroups:true ~options patstr in
  <:expr< Re.replace ~all:$exp:global$ $exp:regexp_expr$ ~f:$exp:patexpr$ >>

end

let rewrite_match arg = function
  <:expr:< [%match $str:s$ ;] >> -> Match.build_regexp loc ~options:[] s
| <:expr:< [%match $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Match.build_regexp loc ~options s
| _ -> assert false

let rewrite_split arg = function
  <:expr:< [%split $str:s$ ;] >> -> Split.build_regexp loc ~options:[] s
| <:expr:< [%split $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Split.build_regexp loc ~options s
| _ -> assert false

let rewrite_pattern arg = function
  <:expr:< [%pattern $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Pattern.build_pattern loc ~force_cgroups:false ~options s
| <:expr:< [%pattern $str:s$ ;] >> -> Pattern.build_pattern loc ~force_cgroups:false ~options:[] s
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "Pa_perl.rewrite_pattern: unsupported extension <<%a>>"
            Pp_MLast.pp_expr e)

let rewrite_subst arg = function
  <:expr:< [%subst $str:restr$ / $str:patstr$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Subst.build_subst loc ~options restr patstr
| <:expr:< [%subst $str:restr$ / $str:patstr$ ;] >> -> Subst.build_subst loc ~options:[] restr patstr
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "Pa_perl.rewrite_subst: unsupported extension <<%a>>"
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
  | <:expr:< [%subst $exp:_$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_subst arg z)
  ] } in
  Pa_passthru.(install { name = "pa_perl"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
