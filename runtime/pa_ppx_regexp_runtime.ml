(* camlp5o *)
(* runtime.ml,v *)

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
           if s = 0 then acc
           else (`Text (String.sub str start (s-start)) :: acc) in
         if pos = s then
           if s = e then
             srec (`Delim ss::acc) ~start:e ~pos:(e+1)
           else srec (`Delim ss :: acc) ~start:e ~pos:e
         else
           srec (`Delim ss :: acc) ~start:e ~pos:e
  in
  srec [] ~start:0 ~pos:0

let pcre2_full_split rex str =
  let open Pcre2 in
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
           if s = 0 then acc
           else (`Text (String.sub str start (s-start)) :: acc) in
         if pos = s then
           if s = e then
             srec (`Delim ss::acc) ~start:e ~pos:(e+1)
           else srec (`Delim ss :: acc) ~start:e ~pos:e
         else
           srec (`Delim ss :: acc) ~start:e ~pos:e
  in
  srec [] ~start:0 ~pos:0
