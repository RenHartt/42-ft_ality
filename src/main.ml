(* src/main.ml *)

let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: ./ft_ality <grammar_file>";
    exit 1
  );
  
  let file = Sys.argv.(1) in
  match Grammar.parse file with
  | Error e -> prerr_endline ("Parse error: " ^ e); exit 1
  | Ok g ->
      (match Mapping.make g with
       | Error e -> prerr_endline ("Mapping error: " ^ e); exit 1
       | Ok map ->
           match Automaton.build g with
           | Error e -> prerr_endline ("Automaton error: " ^ e); exit 1
           | Ok a -> Game_loop.run a map)
