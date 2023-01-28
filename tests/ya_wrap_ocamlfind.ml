(** -syntax camlp5o *)
let rec split_args cmd = function
  | "--" :: files -> List.rev cmd, files
  | [file] -> List.rev cmd, [file]
  | arg :: args -> split_args (arg :: cmd) args
  | [] -> failwith "please supply input arguments"
let split_args = split_args []

let envsubst s =
  let envlookup vname =
    match Sys.getenv_opt vname with
      Some v -> v
    | None -> failwith [%pattern {|ya_wrap_ocamlfind: environment variable <<${vname}>> not found|}] in
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false in

  [%subst {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|} / {| f $1$ $2$ |} / g e] s

let discover_args f =
  let f' = open_in f in
  let line1 = input_line f' in
  close_in f';
  match [%match {|^\(\*\*(.*?)\*\)|} / strings] line1 with
  | None -> ""
  | Some (_, Some params) -> envsubst params

let () = 
  let cmd, files =
    Array.to_list Sys.argv |> List.tl |> split_args in
  let cmd = Filename.quote_command (List.hd cmd) (List.tl cmd) in

  List.iter (fun f ->
      let extra = discover_args f in
      let cmd = [%pattern {|${cmd} ${extra} ${f}|}] in
      Printf.fprintf stderr "%s\n%!" cmd;
      ignore (Sys.command cmd))
    files
