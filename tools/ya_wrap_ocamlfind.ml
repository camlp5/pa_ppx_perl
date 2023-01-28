(** -syntax camlp5o *)
let rec split_args cmd =
  function
    "--" :: files -> List.rev cmd, files
  | [file] -> List.rev cmd, [file]
  | arg :: args -> split_args (arg :: cmd) args
  | [] -> failwith "please supply input arguments"
let split_args = split_args []

let envsubst s =
  let envlookup vname =
    match Sys.getenv_opt vname with
      Some v -> v
    | None ->
        failwith
          (String.concat ""
             ["ya_wrap_ocamlfind: environment variable <<"; vname;
              ">> not found"])
  in
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false
  in
  Re.replace ~all:true
    (Re.Perl.compile_pat ~opts:[] "(?:\\$\\(([^)]+)\\)|\\$\\{([^}]+)\\})")
    ~f:(fun __g__ ->
       f
         (match Re.Group.get_opt __g__ 1 with
            None -> ""
          | Some s -> s)
         (match Re.Group.get_opt __g__ 2 with
            None -> ""
          | Some s -> s))
    s

let discover_args f =
  let f' = open_in f in
  let line1 = input_line f' in
  close_in f';
  match
    (let __re__ = Re.Perl.compile_pat ~opts:[] "^\\(\\*\\*(.*?)\\*\\)" in
     fun __subj__ ->
       Option.map
         (fun __g__ -> Re.Group.get __g__ 0, Re.Group.get_opt __g__ 1)
         (Re.exec_opt __re__ __subj__))
      line1
  with
    None -> ""
  | Some (_, Some params) -> envsubst params

let () =
  let (cmd, files) = (Array.to_list Sys.argv |> List.tl) |> split_args in
  let cmd = Filename.quote_command (List.hd cmd) (List.tl cmd) in
  List.iter
    (fun f ->
       let extra = discover_args f in
       let cmd = String.concat "" [cmd; " "; extra; " "; f] in
       Printf.fprintf stderr "%s\n%!" cmd; ignore (Sys.command cmd))
    files
