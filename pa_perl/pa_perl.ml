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
| Raw
| Strings
| StringGroups of (int *bool) list
| Exception
| RePerl
| Pcre
[@@deriving show]

let pp_hum pps = function
  Multi -> Fmt.(pf pps "m")
| Single -> Fmt.(pf pps "s")
| Global -> Fmt.(pf pps "g")
| Insensitive -> Fmt.(pf pps "i")
| Expr -> Fmt.(pf pps "e")
| Raw -> Fmt.(pf pps "raw")
| Strings -> Fmt.(pf pps "strings")
| StringGroups l ->
   let cgnum pps = function
       (n,true) -> Fmt.(pf pps "!%d" n)
     | (n,false) -> Fmt.(pf pps "%d" n) in
   Fmt.(pf pps "strings (%a)" (list ~sep:(const string ",") cgnum) l)
| Exception -> Fmt.(pf pps "exc")
| RePerl -> Fmt.(pf pps "re_perl")
| Pcre -> Fmt.(pf pps "pcre")

let fixed_only l =
  Std.filter (function StringGroups _ -> false | _ -> true) l

let default_string_groups ngroups =
  (Std.interval 0 (ngroups-1))
  |> List.map (fun i -> if i = 0 then (0,true) else (i,false))

let convert e =
  let rec conv l =
    let badarg() = Fmt.(raise_failwithf (MLast.loc_of_expr e) "extract_options: malformed option: <<%a>>" Pp_MLast.pp_expr e) in
    match l with
      <:expr< m >>::l -> Multi::(conv l)
    | <:expr< s >>::l -> Single::(conv l)
    | <:expr< i >>::l -> Insensitive::(conv l)
    | <:expr< e >>::l -> Expr::(conv l)
    | <:expr< g >>::l -> Global::(conv l)
    | <:expr< raw >>::l -> Raw::(conv l)
    | <:expr< strings >>::<:expr< ( $list:gl$ ) >>::l ->
       let gl = gl |> List.map (function
                            <:expr< $int:n$ >> -> (int_of_string n,false)
                          | <:expr< ! $int:n$ >> -> (int_of_string n,true)
                          | _ -> badarg ()) in
       Strings::(StringGroups gl)::(conv l)

    | <:expr< strings >>::<:expr< $int:n$ >>::l ->
       Strings::(StringGroups [(int_of_string n, false)])::(conv l)
    | <:expr< strings >>::<:expr< ! $int:n$ >>::l ->
       Strings::(StringGroups [(int_of_string n, true)])::(conv l)

    | <:expr< strings >>::l -> Strings::(conv l)
    | <:expr< exc >>::l -> Exception::(conv l)
    | <:expr< re_perl >>::l -> RePerl::(conv l)
    | <:expr< pcre >>::l -> Pcre::(conv l)
    | [] -> []
    | _ -> badarg() in
  let (f,l) = Expr.unapplist e in
  let l = Std.uniquize (conv (f::l)) in
  if not (List.mem RePerl l || List.mem Pcre l) then
    RePerl::l
  else l

let string_groups loc options ngroups =
  if not (List.mem Strings  options) && not(List.mem Raw options) then
    default_string_groups ngroups
  else
  match List.find_map (function StringGroups l -> Some l | _ -> None) options with
    Some l -> l
  | None ->
     if List.mem Strings options then
       default_string_groups ngroups
     else
       Fmt.(raise_failwithf loc "Options.string_groups: internal error: <<%a>>" (list pp) options)

let check_oneof ~l options =
  List.length (Std.intersect options l) <= 1

let forbidden_options ~l options =
  let options = fixed_only options in
  Std.subtract options l

end

let compile_opts loc options =
  let open Options in
  let case_insensitive = List.mem Insensitive options in
  let dotall = List.mem Single options in
  let multiline = List.mem Multi options in
  if List.mem Pcre options then
    let opts = [] in
    let opts = if case_insensitive then <:expr< `CASELESS >>::opts else opts in
    let opts = if dotall then <:expr< `DOTALL >>::opts else opts in
    let opts = if multiline then <:expr< `MULTILINE >>::opts else opts in
    convert_up_list_expr loc opts
  else if List.mem RePerl options then
    let opts = [] in
    let opts = if case_insensitive then <:expr< `Caseless >>::opts else opts in
    let opts = if dotall then <:expr< `Dotall >>::opts else opts in
    let opts = if multiline then <:expr< `Multiline >>::opts else opts in
    convert_up_list_expr loc opts
  else assert false

module Match = struct

let re_build_string_converter loc ~options ngroups =
  let open Options in
  let string_groups = Options.string_groups loc options ngroups in
  let group_exp (n,required) =
    if required then
      <:expr< Re.Group.get __g__ $int:string_of_int n$ >>
    else
      <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >> in
  let group_exps = List.map group_exp string_groups in
  let group_tuple = Expr.tuple loc group_exps in
  <:expr< (fun __g__ -> $exp:group_tuple$ ) >>

let rec re_build_result loc ~options ngroups use_exception =
  let open Options in
  if List.mem Raw options then
     if use_exception then
       <:expr< Re.exec __re__ __subj__ >>
     else
       <:expr< Re.exec_opt __re__ __subj__ >>
  else
    let convf = re_build_string_converter loc ~options ngroups in
     if use_exception then
       let res = re_build_result loc ~options:[Raw] ngroups true in
       <:expr< $exp:convf$ $exp:res$ >>
     else
       let res = re_build_result loc ~options:[Raw] ngroups false in
       <:expr< match Option.map $exp:convf$ $exp:res$ with
                 exception Not_found -> None
               | rv -> rv
                 >>

let pcre_build_string_converter loc ~options ngroups =
  let open Options in
  let string_groups = Options.string_groups loc options ngroups in
  let group_exp (n,required) =
    if required then
      <:expr< Pcre.get_substring __g__ $int:string_of_int n$ >>
    else
      <:expr< try Some(Pcre.get_substring __g__ $int:string_of_int n$) with Not_found -> None >> in
  let group_exps = List.map group_exp string_groups in
  let group_tuple = Expr.tuple loc group_exps in
  <:expr< (fun __g__ -> $exp:group_tuple$ ) >>

let rec pcre_build_result loc ~options ngroups use_exception =
  let open Options in
  if List.mem Raw options then
     if use_exception then
       <:expr< Pcre.exec ~rex:__re__ __subj__ >>
     else
       <:expr< try Some (Pcre.exec ~rex:__re__ __subj__) with Not_found -> None >>
  else
    let convf = pcre_build_string_converter loc ~options ngroups in
     if use_exception then
       let res = pcre_build_result loc ~options:[Raw] ngroups true in
       <:expr< $exp:convf$ $exp:res$ >>
     else
       let res = pcre_build_result loc ~options:[Raw] ngroups false in
       <:expr< match Option.map $exp:convf$ $exp:res$ with
                 exception Not_found -> None
               | rv -> rv
                 >>

let validate_options modn loc options =
  let open Options in
  if not (check_oneof ~l:[RePerl; Pcre] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<re>>, <<pcre>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Strings; Raw] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<strings>>, <<raw>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Multi;Single] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<s>>, <<m>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  let fl = forbidden_options  ~l:[Insensitive; Single; Multi; Exception; Raw; Strings; RePerl; Pcre] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

let build_regexp loc ~options restr =
  let open Options in
  validate_options "match" loc options ;
  let use_exception = List.mem Exception options in
  if List.mem Pcre options then
    let re = Pcre.regexp (Scanf.unescaped restr) in
    let ngroups = 1 + Pcre.capturecount re in
    let compile_opt_expr = compile_opts loc options in
    let regexp_expr = <:expr< Pcre.regexp ~flags:$exp:compile_opt_expr$ $str:restr$ >> in
    let result = pcre_build_result loc ~options ngroups use_exception in
    <:expr< let __re__ = $exp:regexp_expr$ in
            fun __subj__->
            $exp:result$ >>
  else if List.mem RePerl options then
    let re = Re.Perl.compile_pat (Scanf.unescaped restr) in
    let ngroups = Re.group_count re in
    let compile_opt_expr = compile_opts loc options in
    let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
    let result = re_build_result loc ~options ngroups use_exception in
    <:expr< let __re__ = $exp:regexp_expr$ in
            fun __subj__->
            $exp:result$ >>
  else Fmt.(raise_failwithf loc "match extension: neither <<re>> nor <<pcre>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options)
end

module Split = struct

let rec re_build_result loc ~options ngroups =
  let open Options in
  if List.mem Raw options then
    <:expr< Re.split_full __re__ __subj__ >>
  else if List.mem Strings options then
    let converter_fun_exp =
      let convf = Match.re_build_string_converter loc ~options ngroups in
      <:expr< function `Text s -> `Text s
                       | `Delim __g__ -> `Delim ($exp:convf$ __g__) >> in
    <:expr< List.map $exp:converter_fun_exp$ (Re.split_full __re__ __subj__) >>
  else
    <:expr< Re.split __re__ __subj__ >>

let rec pcre_build_result loc ~options ngroups =
  let open Options in
  if List.mem Strings options then
    Fmt.(raise_failwithf loc "split extension: <<strings>> is incompatible with <<pcre>>\n")
  else if List.mem Raw options then
    <:expr< Pcre.full_split ~max:(-1) ~rex:__re__ __subj__ >>
  else
    <:expr< Pcre.split ~rex:__re__ __subj__ >>

let build_regexp loc ~options restr =
  let open Options in
  Match.validate_options "split" loc options ;
  if List.mem RePerl options then
    let re = Re.Perl.compile_pat (Scanf.unescaped restr) in
    let ngroups = Re.group_count re in
    if ngroups > 1 && not (List.mem Strings options || List.mem Raw options) then
      Fmt.(raise_failwithf loc "split extension: must specify one of <<strings>>, <<raw>> for regexp with capture groups: %a"
             (list Options.pp) options)
    else
      let compile_opt_expr = compile_opts loc options in
      let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
      let result = re_build_result loc ~options ngroups in
      <:expr< let __re__ = $exp:regexp_expr$ in
              fun __subj__->
              $exp:result$ >>
  else if List.mem Pcre options then
    let re = Pcre.regexp (Scanf.unescaped restr) in
    let ngroups = 1 + Pcre.capturecount re in
    if ngroups > 1 && not (List.mem Strings options || List.mem Raw options) then
      Fmt.(raise_failwithf loc "split extension: must specify one of <<strings>>, <<raw>> for regexp with capture groups: %a"
             (list Options.pp) options)
    else
      let compile_opt_expr = compile_opts loc options in
      let regexp_expr = <:expr< Pcre.regexp ~flags:$exp:compile_opt_expr$ $str:restr$ >> in
      let result = pcre_build_result loc ~options ngroups in
      <:expr< let __re__ = $exp:regexp_expr$ in
              fun __subj__->
              $exp:result$ >>
  else Fmt.(raise_failwithf loc "split extension: neither <<re>> nor <<pcre>> were found in options: %a\n"
              (list ~sep:(const string " ") Options.pp_hum) options)
end


module Pattern = struct

let string_parts_pattern = Re.Perl.compile_pat {|\$\$|\$([0-9]+)|\$\{([0-9]+)\}|\$\{([^}]+)\}|}

let build_string loc ~force_cgroups ~options patstr =
  let open Options in
  let has_cgroups = ref force_cgroups in
  let cgroup_extract_expr nstr =
    if List.mem RePerl options then
      <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>      
    else if List.mem Pcre options then
      <:expr< match Pcre.get_substring __g__ $int:nstr$ with exception Not_found -> "" | s -> s >>      
    else Fmt.(raise_failwithf loc "Pattern.build_string: neither <<re>> nor <<pcre>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options) in
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
                       cgroup_extract_expr nstr
                    | (_, _, _, Some exps) ->
                       parse_expr exps
                    | _ -> Fmt.(raise_failwithf loc "pa_ppx_perl: unrecognized pattern: <<%a>>" Dump.string patstr)
               ) in
  let listexpr = convert_up_list_expr loc parts_exps in
  if !has_cgroups then
    <:expr< fun __g__ -> String.concat "" $exp:listexpr$ >>
  else
    <:expr< String.concat "" $exp:listexpr$ >>

let build_expr loc ~force_cgroups ~options patstr =
  let open Options in
  let has_cgroups = ref force_cgroups in
  let cgroup_extract_expr nstr =
    if List.mem RePerl options then
      <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>      
    else if List.mem Pcre options then
      <:expr< match Pcre.get_substring __g__ $int:nstr$ with exception Not_found -> "" | s -> s >>      
    else Fmt.(raise_failwithf loc "Pattern.build_expr: neither <<re>> nor <<pcre>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options) in
  let e = parse_antiquot_expr patstr in
  let dt = make_dt () in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      ExXtr(loc, antiquot, _) ->
       let (nstr,_) = Std.sep_last (String.split_on_char ':' antiquot) in
       has_cgroups := true ;
       cgroup_extract_expr nstr
    | e -> old_migrate_expr dt e in
  let dt = { dt with migrate_expr = migrate_expr } in
  let e = dt.migrate_expr dt e in
  if !has_cgroups then
    <:expr< fun __g__ -> $exp:e$ >>
  else
    e

let validate_options modn loc options =
  let open Options in
  let fl = forbidden_options  ~l:[Expr; RePerl; Pcre] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

let build_pattern loc ~force_cgroups ~options patstr =
  let open Options in
  validate_options "pattern" loc options ;
  let patstr = Scanf.unescaped patstr in
  if List.mem Expr options then
    build_expr loc ~force_cgroups ~options patstr
  else
    build_string loc ~force_cgroups ~options patstr

end

module Subst = struct

let validate_options modn loc options =
  let open Options in
  if not (check_oneof ~l:[RePerl; Pcre] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<re>>, <<pcre>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Multi;Single] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<s>>, <<m>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  let fl = forbidden_options  ~l:[Global; Multi; Single; Insensitive; Expr; RePerl; Pcre] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

  let build_subst loc ~options restr patstr =
  let open Options in
  validate_options "subst" loc options ;
  let global = List.mem Global options in
  let global = if global then <:expr< true >> else <:expr< false >> in
  let compile_opt_expr = compile_opts loc options in
  let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $str:restr$ >> in
  let patexpr = Pattern.build_pattern loc ~force_cgroups:true ~options:(Std.intersect [Expr;RePerl;Pcre] options) patstr in
  <:expr< Re.replace ~all:$exp:global$ $exp:regexp_expr$ ~f:$exp:patexpr$ >>

end

let rewrite_match arg = function
  <:expr:< [%match $str:s$ ;] >> -> Match.build_regexp loc ~options:[Options.RePerl] s
| <:expr:< [%match $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Match.build_regexp loc ~options s
| _ -> assert false

let rewrite_split arg = function
  <:expr:< [%split $str:s$ ;] >> -> Split.build_regexp loc ~options:[Options.RePerl] s
| <:expr:< [%split $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Split.build_regexp loc ~options s
| _ -> assert false

let rewrite_pattern arg = function
  <:expr:< [%pattern $str:s$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Pattern.build_pattern loc ~force_cgroups:false ~options s
| <:expr:< [%pattern $str:s$ ;] >> -> Pattern.build_pattern loc ~force_cgroups:false ~options:[Options.RePerl] s
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "Pa_perl.rewrite_pattern: unsupported extension <<%a>>"
            Pp_MLast.pp_expr e)

let rewrite_subst arg = function
  <:expr:< [%subst $str:restr$ / $str:patstr$ / $exp:optexpr$ ;] >> ->
   let options = Options.convert optexpr in
   Subst.build_subst loc ~options restr patstr
| <:expr:< [%subst $str:restr$ / $str:patstr$ ;] >> -> Subst.build_subst loc ~options:[Options.RePerl] restr patstr
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
