(* src/grammar.ml *)

type token = string
type touch = { label : string; token : token; raw : string }
type combo = { move : string; seq : touch list }
type t = { inputs : touch list; moves : combo list }

let trim s = String.trim s
let lower s = String.lowercase_ascii s

let normalize_token_keep s =
  let s = trim s in
  let n = String.length s in
  if s = "" then Error "empty token"
  else if n >= 3 && s.[0] = '[' && s.[n - 1] = ']' then
    Ok (s, lower (String.sub s 1 (n - 2)))
  else
    Ok (s, lower s)

let split_once_colon s =
  match String.index_opt s ':' with
  | None -> Error "missing ':'"
  | Some i ->
      let l = trim (String.sub s 0 i)
      and r = trim (String.sub s (i + 1) (String.length s - i - 1)) in
      if l = "" || r = "" then Error "empty field around ':'" else Ok (l, r)

let split_csv rhs =
  rhs |> String.split_on_char ',' |> List.map trim |> List.filter ((<>) "")

let find_touch_by_token inputs tok =
  let rec go = function
    | [] -> None
    | t :: ts -> if t.token = tok then Some t else go ts
  in
  go inputs

let parse_input_line l =
  match split_once_colon l with
  | Error e -> Error e
  | Ok (label, rhs) ->
      if String.contains rhs ',' then Error "single token expected"
      else
        match normalize_token_keep rhs with
        | Error e -> Error e
        | Ok (raw, tok) -> Ok { label; token = tok; raw }

let parse_move_line inputs l =
  match split_once_colon l with
  | Error e -> Error e
  | Ok (move, rhs) ->
      let raw = split_csv rhs in
      if raw = [] then Error "empty sequence"
      else
        let rec to_seq acc = function
          | [] -> Ok (List.rev acc)
          | x :: xs -> (
              match normalize_token_keep x with
              | Error e -> Error e
              | Ok (raw, tok) -> (
                  match find_touch_by_token inputs tok with
                  | None -> Error ("unknown token '" ^ tok ^ "'")
                  | Some t -> to_seq (t :: acc) xs))
        in
        to_seq [] raw |> Result.map (fun seq -> { move; seq })

let rec skip_to_inputs ch =
  match input_line ch with
  | exception End_of_file -> Error "expected '# inputs'"
  | l ->
      let l = trim l in
      if l = "# inputs" then Ok ()
      else if l = "" then skip_to_inputs ch
      else skip_to_inputs ch

let rec read_inputs ch acc =
  match input_line ch with
  | exception End_of_file -> Error "expected '# moves'"
  | l ->
      let l = trim l in
      if l = "# moves" then Ok (List.rev acc)
      else if l = "" then read_inputs ch acc
      else
        (match parse_input_line l with
         | Error e -> Error ("input: " ^ e)
         | Ok t -> read_inputs ch (t :: acc))

let rec read_moves ch inputs acc =
  match input_line ch with
  | exception End_of_file -> Ok (List.rev acc)
  | l ->
      let l = trim l in
      if l = "" then read_moves ch inputs acc
      else
        (match parse_move_line inputs l with
         | Error e -> Error ("move: " ^ e)
         | Ok c -> read_moves ch inputs (c :: acc))

let parse file : (t, string) result =
  try
    let ch = open_in file in
    let res =
      match skip_to_inputs ch with
      | Error e -> Error e
      | Ok () ->
          (match read_inputs ch [] with
           | Error e -> Error e
           | Ok inputs ->
               (match read_moves ch inputs [] with
                | Error e -> Error e
                | Ok moves -> Ok { inputs; moves }))
    in
    close_in_noerr ch;
    res
  with Sys_error _ ->
    Error ("cannot open file: " ^ file)

(*Utils print parsed grammar*)
let print g =
  print_endline "# inputs";
  List.iter (fun t -> Printf.printf "- %s : %s\n" t.label t.raw) g.inputs;
  print_endline "\n# moves";
  List.iter
    (fun c ->
      let seq = List.map (fun t -> t.label) c.seq |> String.concat ", " in
      Printf.printf "- %s : %s\n" c.move seq)
    g.moves