type state = int
type token = string

type delta = (state * token * state) list
type finals = (state * string list) list

type t = {
  start  : state;
  delta  : delta;
  finals : finals;
  max_st : state;
}

let empty : t = { start = 0; delta = []; finals = []; max_st = 0 }

let rec find_transition (state : state) (token : token) = function
  | [] -> None
  | (src, tok, dst) :: rest ->
      if src = state && tok = token
      then Some dst
      else find_transition state token rest

let add_transition (state : state) (token : token) (next_state : state) (d : delta) : delta =
  match find_transition state token d with
  | Some _ -> d
  | None -> (state, token, next_state) :: d

let rec add_final_label (state : state) (name : string) (f : finals) : finals =
  match f with
  | [] -> [ (state, [name]) ]
  | (q, labels) :: rest ->
      if q = state then
        let labels' = if List.mem name labels then labels else name :: labels in
        (q, labels') :: rest
      else
        (q, labels) :: add_final_label state name rest

let step (automaton : t) (state : state) (token : token) : state option =
  find_transition state token automaton.delta

let finals_of (automaton : t) (state : state) : string list =
  let rec lookup = function
    | [] -> []
    | (q, labels) :: rest -> if q = state then labels else lookup rest
  in
  lookup automaton.finals

let insert_sequence (automaton : t) (move_name : string) (tokens : token list) : t =
  let rec walk state max_state delta = function
    | [] ->
        { automaton with
          delta;
          finals = add_final_label state move_name automaton.finals;
          max_st = if max_state > automaton.max_st then max_state else automaton.max_st
        }
    | token :: remaining -> (
        match find_transition state token delta with
        | Some next_state ->
            walk next_state max_state delta remaining
        | None ->
            let next_state = max_state + 1 in
            let delta' = add_transition state token next_state delta in
            walk next_state next_state delta' remaining )
  in
  walk automaton.start automaton.max_st automaton.delta tokens

let build (g : Grammar.t) : t =
  let rec insert_all acc = function
    | [] -> acc
    | combo :: rest ->
        let tokens = List.map (fun (t : Grammar.touch) -> t.token) combo.Grammar.seq in
        insert_all (insert_sequence acc combo.Grammar.move tokens) rest
  in
  insert_all empty g.Grammar.moves
