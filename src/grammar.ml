let trim (s : string) = String.trim s

let split_on_colon (s : string) : (string * string) =
  match String.split_on_char ':' s with
  | [left; right] ->
      let left  = String.trim left
      and right = String.trim right in
      if left = "" || right = "" then
        invalid_arg ("Invalid grammar line (missing name or tokens): " ^ s)
      else
        (left, right)
  | _ ->
      invalid_arg ("Invalid grammar line (missing or too many colons): " ^ s)

let split_tokens (rhs : string) : string list =
  let parts = String.split_on_char ',' rhs |> List.map String.trim in
  if List.exists ((=) "") parts then
    invalid_arg ("Invalid grammar line: empty token in \"" ^ rhs ^ "\"")
  else
    parts

let parse_line (line : string) : (string * string list) option =
  let line = trim line in
  if line = "" then None
  else
    let (name, rhs) = split_on_colon line in
    let toks = split_tokens rhs in
    Some (name, toks)

let load (filename : string) : (string * string list) list =
  let ic =
    try open_in filename with Sys_error _ ->
      prerr_endline ("Error: cannot open grammar file \"" ^ filename ^ "\"");
      exit 1
  in
  let rec loop acc =
    try
      let line = input_line ic in
      let acc =
        match parse_line line with
        | None -> acc
        | Some pair -> pair :: acc
      in
      loop acc
    with End_of_file ->
      close_in_noerr ic;
      List.rev acc
  in
  loop []
