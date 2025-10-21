(* src/game_loop.ml *)

let init_window () =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
  | Error (`Msg e) ->
      prerr_endline ("SDL init error: " ^ e); exit 1
  | Ok () ->
      match Tsdl.Sdl.create_window
              ~w:640 ~h:360 "ft_ality (inputs test)"
              Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) ->
          prerr_endline ("SDL window error: " ^ e);
          Tsdl.Sdl.quit (); exit 1
      | Ok w -> w

let pressed sc =
  let ks = Tsdl.Sdl.get_keyboard_state () in
  Bigarray.Array1.get ks sc <> 0

let show_bmp (w:Tsdl.Sdl.window) (path:string) : unit =
  match Tsdl.Sdl.load_bmp path with
  | Error (`Msg e) ->
      prerr_endline ("BMP load error: " ^ e)
  | Ok img ->
      (match Tsdl.Sdl.get_window_surface w with
       | Error (`Msg e) ->
           prerr_endline ("Window surface error: " ^ e)
       | Ok win_surf ->
           begin match Tsdl.Sdl.blit_surface img None win_surf None with
           | Error (`Msg e) -> prerr_endline ("Blit error: " ^ e)
           | Ok () ->
               ignore (Tsdl.Sdl.update_window_surface w)
           end);
      Tsdl.Sdl.free_surface img

let handle_key (w : Tsdl.Sdl.window) (a : Automaton.t) (map : Mapping.t) (st : Automaton.state) (seq : string list) (sc : int)
  : Automaton.state * (string list) =
  match Mapping.find map sc with
  | None ->
      (st, seq)
  | Some t ->
      let from_st = Automaton.step a st t.token in
      let next, started_from_start =
        match from_st with
        | Some s -> (Some s, false)
        | None   -> (Automaton.step a a.Automaton.start t.token, true)
      in
      match next with
      | None ->
          (a.Automaton.start, [])
      | Some s ->
          let seq' =
            if started_from_start then [t.raw] else seq @ [t.raw]
          in
          let line = String.concat ", " seq' in
          Printf.printf "%s\n%!" line;
          show_bmp w "data/black_screen.bmp";
          let finals = Automaton.finals_of a s in
          match finals with
          | [] ->
              (s, seq')
          | names ->
              List.iter (fun n -> Printf.printf "%s !!\n%!" n) names;
              show_bmp w "data/fatality.bmp";
              (s, seq')

let rec loop (w:Tsdl.Sdl.window) (a:Automaton.t) (map:Mapping.t) (st:Automaton.state) (seq:string list) (ev:Tsdl.Sdl.event) =
  match Tsdl.Sdl.wait_event (Some ev) with
  | Error (`Msg _) -> loop w a map st seq ev
  | Ok () ->
      let typ = Tsdl.Sdl.Event.(enum (get ev typ)) in
      match typ with
      | `Quit -> ()
      | `Key_down ->
          if pressed Tsdl.Sdl.Scancode.escape then ()
          else
            let sc = Tsdl.Sdl.Event.(get ev keyboard_scancode) in
            let (st', seq') = handle_key w a map st seq sc in
            loop w a map st' seq' ev
      | _ -> loop w a map st seq ev

let run (a:Automaton.t) (map:Mapping.t) : unit =
  let w = init_window () in
  Mapping.print map;
  let ev = Tsdl.Sdl.Event.create () in
  loop w a map a.Automaton.start [] ev;
  Tsdl.Sdl.destroy_window w;
  Tsdl.Sdl.quit ()