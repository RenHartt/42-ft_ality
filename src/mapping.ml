(* src/mapping.ml *)

type key = int
type binding = key * Grammar.touch
type t = binding list

let keys : key list =
  [
    Tsdl.Sdl.Scancode.left; Tsdl.Sdl.Scancode.right;
    Tsdl.Sdl.Scancode.up;   Tsdl.Sdl.Scancode.down;
    Tsdl.Sdl.Scancode.a; Tsdl.Sdl.Scancode.z; Tsdl.Sdl.Scancode.e;
    Tsdl.Sdl.Scancode.r; Tsdl.Sdl.Scancode.t;
    Tsdl.Sdl.Scancode.q; Tsdl.Sdl.Scancode.s; Tsdl.Sdl.Scancode.d;
    Tsdl.Sdl.Scancode.f; Tsdl.Sdl.Scancode.g;
    Tsdl.Sdl.Scancode.w; Tsdl.Sdl.Scancode.x; Tsdl.Sdl.Scancode.c;
    Tsdl.Sdl.Scancode.v; Tsdl.Sdl.Scancode.b;
    Tsdl.Sdl.Scancode.k1; Tsdl.Sdl.Scancode.k2; Tsdl.Sdl.Scancode.k3;
    Tsdl.Sdl.Scancode.k4; Tsdl.Sdl.Scancode.k5;
  ]

let make (g : Grammar.t) : (t, string) result =
  let rec pair acc ks ins =
    match ks, ins with
    | _, [] -> Ok (List.rev acc)
    | [], _ -> Error "not enough SDL scancodes for inputs"
    | k::kt, t::tt -> pair ((k, t) :: acc) kt tt
  in
  pair [] keys g.Grammar.inputs

let key_name (k : key) : string =
  Tsdl.Sdl.get_scancode_name k

let print (m : t) : unit =
  print_endline "SDL2 Key mappings (scancodes):";
  List.iter
    (fun (k, t) -> Printf.printf "  %s -> %s (%s)\n"
      (key_name k) t.Grammar.label t.token)
    m

let find (m : t) (k : key) : Grammar.touch option =
  let rec go = function
    | [] -> None
    | (kk, t) :: rest -> if kk = k then Some t else go rest
  in
  go m
