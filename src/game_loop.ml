(* src/game_loop.ml *)

let init_window () =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
  | Error (`Msg e) -> prerr_endline ("SDL init error: " ^ e); exit 1
  | Ok () ->
      match Tsdl.Sdl.create_window ~w:640 ~h:360 "ft_ality (inputs test)"
              Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) -> prerr_endline ("SDL window error: " ^ e); Tsdl.Sdl.quit (); exit 1
      | Ok w -> w

let pressed sc =
  let ks = Tsdl.Sdl.get_keyboard_state () in
  Bigarray.Array1.get ks sc <> 0

let try_step (a:Automaton.t) (st:Automaton.state) (tok:string) : Automaton.state =
  match Automaton.step a st tok with
  | Some s -> s
  | None ->
      match Automaton.step a a.Automaton.start tok with
      | Some s -> s
      | None   -> st

let handle_key (a:Automaton.t) (map:Mapping.t) (st:Automaton.state) (sc:int)
  : Automaton.state =
  match Mapping.find map sc with
  | None -> a.Automaton.start
  | Some t ->
      Printf.printf "Pressed: %s (%s)\n%!" t.Grammar.label t.token;
      let next =
        match Automaton.step a st t.token with
        | Some s -> s
        | None ->
            match Automaton.step a a.Automaton.start t.token with
            | Some s -> s
            | None   -> a.Automaton.start
      in
      let finals = Automaton.finals_of a next in
      if finals <> [] then (
        List.iter (fun n -> Printf.printf "MOVE: %s\n%!" n) finals;
        a.Automaton.start
      ) else
        next

let rec loop (_w:Tsdl.Sdl.window) (a:Automaton.t) (map:Mapping.t) (st:Automaton.state) (ev:Tsdl.Sdl.event) =
  match Tsdl.Sdl.wait_event (Some ev) with
  | Error (`Msg _) -> loop _w a map st ev
  | Ok () ->
      let typ = Tsdl.Sdl.Event.(enum (get ev typ)) in
      match typ with
      | `Quit -> ()
      | `Key_down ->
          if pressed Tsdl.Sdl.Scancode.escape then ()
          else
            let sc = Tsdl.Sdl.Event.(get ev keyboard_scancode) in
            let st' = handle_key a map st sc in
            loop _w a map st' ev
      | _ -> loop _w a map st ev

let run (a:Automaton.t) (map:Mapping.t) : unit =
  let w = init_window () in
  Mapping.print map;
  print_endline "------";
  print_endline "Press mapped keys (ESC to quit, or close window).";
  let ev = Tsdl.Sdl.Event.create () in
  loop w a map a.Automaton.start ev;
  Tsdl.Sdl.destroy_window w;
  Tsdl.Sdl.quit ()
