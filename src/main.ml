(* src/main.ml *)

let () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: ./ft_ality <grammar_file>";
    exit 1
  );

  let file = Sys.argv.(1) in
  match Grammar.parse file with
  | Error e ->
      prerr_endline ("Parse error: " ^ Grammar.string_of_error e); exit 1
  | Ok g ->
      (match Mapping.make g with
       | Error e ->
           prerr_endline ("Mapping error: " ^ Mapping.string_of_error e); exit 1
       | Ok map ->
           Mapping.print map; flush stdout;
           let a = Automaton.build g in
           Game_loop.run a map)
