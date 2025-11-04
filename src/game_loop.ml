let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dark_grey s = Printf.sprintf "\027[90m%s\027[0m" s

let window_width = 640
let window_height = 360
let window_title = "ft_ality"

let bmp_black_screen = "data/black_screen.bmp"
let bmp_fatality = "data/fatality.bmp"

let create_window () =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
  | Error (`Msg e) ->
      prerr_endline ("SDL init error: " ^ e); exit 1
  | Ok () ->
      match Tsdl.Sdl.create_window
              ~w:window_width ~h:window_height window_title
              Tsdl.Sdl.Window.windowed with
      | Error (`Msg e) ->
          prerr_endline ("SDL window error: " ^ e);
          Tsdl.Sdl.quit (); exit 1
      | Ok w -> w

let is_key_pressed scancode =
  let keyboard_state = Tsdl.Sdl.get_keyboard_state () in
  Bigarray.Array1.get keyboard_state scancode <> 0

let render_bmp (window : Tsdl.Sdl.window) (path : string) : unit =
  match Tsdl.Sdl.load_bmp path with
  | Error (`Msg e) ->
      prerr_endline ("BMP load error: " ^ e)
  | Ok image ->
      let finally () = Tsdl.Sdl.free_surface image in
      (match Tsdl.Sdl.get_window_surface window with
       | Error (`Msg e) ->
           prerr_endline ("Window surface error: " ^ e); finally ()
       | Ok window_surface -> (
           match Tsdl.Sdl.blit_surface image None window_surface None with
           | Error (`Msg e) ->
               prerr_endline ("Blit error: " ^ e); finally ()
           | Ok () ->
               ignore (Tsdl.Sdl.update_window_surface window);
               finally ()
         )
      )

let advance_automaton (automaton : Automaton.t) (state : Automaton.state) (token : string)
  : (Automaton.state * bool) option =
  match Automaton.step automaton state token with
  | Some s -> Some (s, false)
  | None ->
      if state = automaton.Automaton.start then
        None
      else
        match Automaton.step automaton automaton.Automaton.start token with
        | Some s -> Some (s, true)
        | None -> None

let update_sequence_labels (sequence : string list) (raw_label : string) (restarted : bool)
  : string list =
  if restarted then [raw_label] else sequence @ [raw_label]

let handle_key_event (window : Tsdl.Sdl.window) (automaton : Automaton.t) (mapping : Mapping.t) (state : Automaton.state) (sequence : string list) (scancode : int)
  : Automaton.state * string list =
  match Mapping.find mapping scancode with
  | None -> (state, sequence)
  | Some touch ->
      let print_and_flash s = Printf.printf "%s\n%!" (dark_grey s); render_bmp window bmp_black_screen in
      match advance_automaton automaton state touch.token with
      | None ->
          print_and_flash touch.raw;
          (automaton.Automaton.start, [])
      | Some (next_state, restarted) ->
          let sequence' = if restarted then [touch.raw] else sequence @ [touch.raw] in
          print_and_flash (String.concat ", " sequence');
          (match Automaton.finals_of automaton next_state with
           | [] -> ()
           | final_moves ->
               List.iter (fun name -> Printf.printf "%s\n%!" (bold (name ^ " !!"))) final_moves;
               render_bmp window bmp_fatality);
          (next_state, sequence')

let rec event_loop (window : Tsdl.Sdl.window) (automaton : Automaton.t) (mapping : Mapping.t) (state : Automaton.state) (sequence : string list) (event : Tsdl.Sdl.event)
  : unit =
  match Tsdl.Sdl.wait_event (Some event) with
  | Error (`Msg _) ->
      event_loop window automaton mapping state sequence event
  | Ok () ->
      let event_type = Tsdl.Sdl.Event.(enum (get event typ)) in
      match event_type with
      | `Quit -> ()
      | `Key_down ->
          if is_key_pressed Tsdl.Sdl.Scancode.escape then ()
          else
            let scancode = Tsdl.Sdl.Event.(get event keyboard_scancode) in
            let state', sequence' =
              handle_key_event window automaton mapping state sequence scancode
            in
            event_loop window automaton mapping state' sequence' event
      | _ ->
          event_loop window automaton mapping state sequence event

let run (a : Automaton.t) (map : Mapping.t) : unit =
  let window = create_window () in
  let event = Tsdl.Sdl.Event.create () in
  event_loop window a map a.Automaton.start [] event;
  Tsdl.Sdl.destroy_window window;
  Tsdl.Sdl.quit ()
