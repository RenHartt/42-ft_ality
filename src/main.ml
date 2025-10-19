let usage_msg =
    "Usage: ./ft_ality <grammar_file>\n"

let () =
  if Array.length Sys.argv < 2 then (
    print_endline usage_msg;
    exit 1
  );

  let filename = Sys.argv.(1) in
  let combos = Grammar.load filename in

  List.iter
    (fun (name, tokens) ->
      print_endline ("[" ^ name ^ "] -> " ^ String.concat " , " tokens))
    combos
