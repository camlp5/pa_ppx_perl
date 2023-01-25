(* camlp5o *)
(* runtime.ml,v *)
(* Copyright (c) 2020--2022 Jane Street Group, LLC <opensource@janestreet.com> *)
(* used under the Jane Street MIT License for ppx_string *)

let pad str len =
  let pad_len = max 0 (len - String.length str) in
  let padding = String.make pad_len ' ' in
  padding ^ str
