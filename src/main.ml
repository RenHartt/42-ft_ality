let help_msg =
  "Usage: ft_ality [--debug] <grammar_file>\n\
   \n\
   --debug       Run the game in debug mode (shows automaton states)\n\
   <grammar_file>  Path to the grammar file to load"

let run (debug : bool) (file : string) =
  match Grammar.parse file with
  | Error e ->
      prerr_endline ("Parse error: " ^ Grammar.string_of_error e); exit 1
  | Ok g ->
      (match Mapping.make g with
       | Error e ->
           prerr_endline ("Mapping error: " ^ Mapping.string_of_error e); exit 1
       | Ok map ->
           Printf.printf "%s" (Mapping.pp map); flush stdout;
           let a = Automaton.build g debug in
           Game_loop.run a map)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | ["--debug"; file] | [file; "--debug"] ->
      let debug = true in
      run debug file
  | [file] ->
      let debug = false in
      run debug file
  | _ ->
      prerr_endline help_msg;
      exit 1