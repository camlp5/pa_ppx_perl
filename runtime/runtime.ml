(* camlp5o *)
(* runtime.ml,v *)
(* Copyright (c) 2020--2022 Jane Street Group, LLC <opensource@janestreet.com> *)
(* used under the Jane Street MIT License for ppx_string *)

let pad str len =
  let pad_len = max 0 (len - String.length str) in
  let padding = String.make pad_len ' ' in
  padding ^ str

let pcre_full_split rex str =
  let open Pcre in
  let slen = String.length str in
  let rec srec acc ~start ~pos =
    if pos = slen then
      List.rev acc
    else
      match exec ~rex ~pos str with
        exception Not_found ->
         if pos = slen then
           List.rev acc
         else
           List.rev (`Text (String.sub str pos (slen-pos)) :: acc)
      | ss ->
         let (s,e) = get_substring_ofs ss 0 in
         let acc =
           if start = s then acc
           else
             (`Text (String.sub str start (s-start)) :: acc) in
         if pos = s then
           if s = e then
             srec (`Delim ss::acc) ~start:e ~pos:(e+1)
           else srec (`Delim ss :: acc) ~start:e ~pos:e
         else
           srec (`Delim ss :: acc) ~start:e ~pos:e
  in
  srec [] ~start:0 ~pos:0
