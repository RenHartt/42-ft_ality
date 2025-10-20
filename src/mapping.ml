let alphanumeric : string list =
  ["q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p";
   "a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l";
   "z"; "x"; "c"; "v"; "b"; "n"; "m";
   "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]

let arrow : string list = ["left"; "right"; "up"; "down"]

let extract_all_tokens (grammar_entries : (string * string list) list) : string list =
  let rec collect accumulated = function
    | [] -> List.rev accumulated
    | (_rule_name, tokens) :: remaining ->
        collect (List.rev_append tokens accumulated) remaining
  in
  collect [] grammar_entries

let uniq (words : string list) : string list =
  let rec filter_unique seen = function
    | [] -> List.rev seen
    | word :: rest ->
        if List.exists ((=) word) seen then
          filter_unique seen rest
        else
          filter_unique (word :: seen) rest
  in
  filter_unique [] words

let is_arrow (token : string) : bool =
  token = "left" || token = "right" || token = "up" || token = "down"

let map_key_to_token
    (letter_keys : string list)
    (arrow : string list)
    (tokens : string list)
    : (string * string) list =
  let rec associate accumulated letters arrows remaining_tokens =
    match remaining_tokens with
    | [] -> List.rev accumulated
    | token :: rest ->
        if is_arrow token then
          (match arrows with
           | [] -> invalid_arg "No more arrow keys available for mapping"
           | arrow :: remaining_arrows ->
               associate ((arrow, token) :: accumulated) letters remaining_arrows rest)
        else
          (match letters with
           | [] -> invalid_arg "No more alphanumeric keys available for mapping"
           | letter :: remaining_letters ->
               associate ((letter, token) :: accumulated) remaining_letters arrows rest)
  in
  associate [] letter_keys arrow tokens

let make (grammar_entries : (string * string list) list) : (string * string) list =
  let tokens =
    grammar_entries |> extract_all_tokens |> uniq
  in
  map_key_to_token alphanumeric arrow tokens

let print (mapping_table : (string * string) list) : unit =
  print_endline "Key mappings:";
  List.iter
    (fun (key, token) -> print_endline (key ^ " -> " ^ token))
    mapping_table
