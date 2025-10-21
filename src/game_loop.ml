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

let handle_key (w : Tsdl.Sdl.window) (a : Automaton.t) (map : Mapping.t) (st : Automaton.state) (printing_seq : bool) (sc : int)
  : Automaton.state * bool =
  match Mapping.find map sc with
  | None -> (st, printing_seq)
    | Some t ->
      let from_st = Automaton.step a st t.token in
      let from_start = Automaton.step a a.Automaton.start t.token in
      let (next, reset_now) =
        match from_st with
        | Some s -> (Some s, false)
        | None   -> (from_start, true)
      in
      (if reset_now && printing_seq then Printf.printf "\n%!" else ());
      if not printing_seq || reset_now then Printf.printf "%s%!" t.raw
      else Printf.printf ", %s%!" t.raw;
      show_bmp w "data/black_screen.bmp";
      match next with
      | None ->
          Printf.printf "\n%!";
          (a.Automaton.start, false)
      | Some s ->
          let finals = Automaton.finals_of a s in
          (match finals with
           | [] ->
               (s, true)
           | names ->
               Printf.printf "\n%!";
               List.iter (fun n -> Printf.printf "%s !!\n%!" n) names;
               show_bmp w "data/fatality.bmp";
               (a.Automaton.start, false))


let rec loop (w:Tsdl.Sdl.window) (a:Automaton.t) (map:Mapping.t) (st:Automaton.state) (printing_seq:bool) (ev:Tsdl.Sdl.event) =
  match Tsdl.Sdl.wait_event (Some ev) with
  | Error (`Msg _) -> loop w a map st printing_seq ev
  | Ok () ->
      let typ = Tsdl.Sdl.Event.(enum (get ev typ)) in
      match typ with
      | `Quit -> ()
      | `Key_down ->
          if pressed Tsdl.Sdl.Scancode.escape then ()
          else
            let sc = Tsdl.Sdl.Event.(get ev keyboard_scancode) in
            let (st', printing_seq') = handle_key w a map st printing_seq sc in
            loop w a map st' printing_seq' ev
      | _ -> loop w a map st printing_seq ev

let run (a:Automaton.t) (map:Mapping.t) : unit =
  let w = init_window () in
  Mapping.print map;
  let ev = Tsdl.Sdl.Event.create () in
  loop w a map a.Automaton.start false ev;
  Tsdl.Sdl.destroy_window w;
  Tsdl.Sdl.quit ()