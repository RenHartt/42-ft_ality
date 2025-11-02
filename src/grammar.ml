type token = string
type touch = { label : string; token : token; raw : string }
type combo = { move : string; seq : touch list }
type t = { inputs : touch list; moves : combo list }

type error =
  | Empty_token
  | Missing_colon
  | Empty_field
  | Single_token_expected
  | Empty_sequence
  | Unknown_token of string
  | Cannot_open of string
  | Expected_inputs

let string_of_error = function
  | Empty_token -> "empty token"
  | Missing_colon -> "missing ':'"
  | Empty_field -> "empty field around ':'"
  | Single_token_expected -> "single token expected"
  | Empty_sequence -> "empty sequence"
  | Unknown_token tok -> "unknown token '" ^ tok ^ "'"
  | Cannot_open f -> "cannot open file: " ^ f
  | Expected_inputs -> "expected '# inputs'"

let pp (g : t) : string =
  let b = Buffer.create 256 in
  Buffer.add_string b "# inputs\n";
  List.iter
    (fun t -> Buffer.add_string b (Printf.sprintf "- %s : %s\n" t.label t.raw))
    g.inputs;
  Buffer.add_string b "\n# moves\n";
  List.iter
    (fun c ->
      let seq = List.map (fun t -> t.label) c.seq |> String.concat ", " in
      Buffer.add_string b (Printf.sprintf "- %s : %s\n" c.move seq))
    g.moves;
  Buffer.contents b

let print g = pp g |> print_string
let trim s = String.trim s
let lower s = String.lowercase_ascii s

let normalize s =
  let s = trim s in
  let n = String.length s in
  if s = "" then Error Empty_token
  else if n >= 3 && s.[0] = '[' && s.[n - 1] = ']'
  then Ok (s, lower (String.sub s 1 (n - 2)))
  else Ok (s, lower s)

let split_once s =
  match String.index_opt s ':' with
  | None -> Error Missing_colon
  | Some i ->
      let l = trim (String.sub s 0 i)
      and r = trim (String.sub s (i + 1) (String.length s - i - 1)) in
      if l = "" || r = "" then Error Empty_field else Ok (l, r)

let split_csv_trim rhs =
  rhs |> String.split_on_char ',' |> List.map trim |> List.filter ((<>) "")

let parse_input_line line =
  let (let*) = Result.bind in
  let* label, rhs = split_once line in
  if String.contains rhs ',' then Error Single_token_expected
  else
    let* raw, tok = normalize rhs in
    Ok { label; token = tok; raw }

let parse_move_line token_idx line =
  let (let*) = Result.bind in
  let (let+) r f = Result.map f r in
  let* move, rhs = split_once line in
  let raws = split_csv_trim rhs in
  if raws = [] then Error Empty_sequence
  else
    let rec to_seq acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs ->
          let* _raw, tok = normalize x in
          (match List.assoc_opt tok token_idx with
           | None -> Error (Unknown_token tok)
           | Some t -> to_seq (t :: acc) xs)
    in
    let+ seq = to_seq [] raws in
    { move; seq }

let rec skip_inputs ch =
  match input_line ch with
  | exception End_of_file -> Error Expected_inputs
  | l ->
      let l = trim l in
      if l = "# inputs" then Ok ()
      else skip_inputs ch

let rec read_inputs ch acc =
  match input_line ch with
  | exception End_of_file -> Error Missing_colon
  | l ->
      let l = trim l in
      if l = "# moves" then Ok (List.rev acc)
      else if l = "" then read_inputs ch acc
      else
        match parse_input_line l with
        | Ok t -> read_inputs ch (t :: acc)
        | Error e -> Error e

let rec read_moves ch token_idx acc =
  match input_line ch with
  | exception End_of_file -> Ok (List.rev acc)
  | l ->
      let l = trim l in
      if l = "" then read_moves ch token_idx acc
      else
        match parse_move_line token_idx l with
        | Ok c -> read_moves ch token_idx (c :: acc)
        | Error e -> Error e

let parse_channel (ch : in_channel) : (t, error) result =
  let ( let* ) = Result.bind in
  let* () = skip_inputs ch in
  let* inputs = read_inputs ch [] in
  let token_idx = List.map (fun t -> (t.token, t)) inputs in
  let* moves = read_moves ch token_idx [] in
  Ok { inputs; moves }

let parse (file : string) : (t, error) result =
  try
    let ch = open_in file in
    let res = parse_channel ch in
    close_in_noerr ch;
    res
  with Sys_error _ ->
    Error (Cannot_open file)
