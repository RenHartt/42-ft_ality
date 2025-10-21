(* src/automaton.ml *)

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

let rec assoc_next st tok = function
  | [] -> None
  | (a,b,c)::xs ->
      if a = st && b = tok then Some c else assoc_next st tok xs

let rec add_delta st tok nxt (d:delta) =
  match assoc_next st tok d with
  | Some _ -> d
  | None   -> (st, tok, nxt) :: d

let rec add_final st name (f:finals) =
  match f with
  | [] -> [ (st, [name]) ]
  | (q, lst) :: xs ->
      if q = st then (q, (if List.mem name lst then lst else name::lst)) :: xs
      else (q, lst) :: add_final st name xs

let step (a:t) (st:state) (tok:token) : state option =
  assoc_next st tok a.delta

let finals_of (a:t) (st:state) : string list =
  let rec go = function
    | [] -> []
    | (q, lst) :: xs -> if q = st then lst else go xs
  in
  go a.finals

let insert_seq (a:t) (name:string) (seq:token list) : t =
  let rec walk st max_st delta = function
    | [] ->
        { a with delta; finals = add_final st name a.finals; max_st = max a.max_st max_st }
    | tok :: rest ->
        match assoc_next st tok delta with
        | Some nxt ->
            walk nxt max_st delta rest
        | None ->
            let nxt = max_st + 1 in
            let delta' = add_delta st tok nxt delta in
            walk nxt nxt delta' rest
  in
  walk a.start a.max_st a.delta seq

let build (g:Grammar.t) : (t, string) result =
  let rec insert_all acc = function
    | [] -> Ok acc
    | c :: cs ->
        let seq = List.map (fun (t:Grammar.touch) -> t.token) c.Grammar.seq in
        insert_all (insert_seq acc c.Grammar.move seq) cs
  in
  insert_all empty g.Grammar.moves

  let start_state (a:t) : state = a.start