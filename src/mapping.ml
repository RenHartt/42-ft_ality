(* src/mapping.ml *)

type key = int
type binding = key * Grammar.touch
type t = binding list

let keys : key list =
  [
    Tsdl.Sdl.Scancode.left; Tsdl.Sdl.Scancode.right;
    Tsdl.Sdl.Scancode.up;   Tsdl.Sdl.Scancode.down;
    Tsdl.Sdl.Scancode.q;    Tsdl.Sdl.Scancode.w;
    Tsdl.Sdl.Scancode.e;    Tsdl.Sdl.Scancode.r;
    Tsdl.Sdl.Scancode.a;    Tsdl.Sdl.Scancode.s;
    Tsdl.Sdl.Scancode.z;    Tsdl.Sdl.Scancode.x;
    Tsdl.Sdl.Scancode.e;    Tsdl.Sdl.Scancode.r;
    Tsdl.Sdl.Scancode.d;    Tsdl.Sdl.Scancode.f;
    Tsdl.Sdl.Scancode.c;    Tsdl.Sdl.Scancode.v;
    Tsdl.Sdl.Scancode.t;    Tsdl.Sdl.Scancode.y;
    Tsdl.Sdl.Scancode.g;    Tsdl.Sdl.Scancode.h;
    Tsdl.Sdl.Scancode.b;    Tsdl.Sdl.Scancode.n;
    Tsdl.Sdl.Scancode.u;    Tsdl.Sdl.Scancode.i;
    Tsdl.Sdl.Scancode.j;    Tsdl.Sdl.Scancode.k;
    Tsdl.Sdl.Scancode.m;
    Tsdl.Sdl.Scancode.o;    Tsdl.Sdl.Scancode.p;
    Tsdl.Sdl.Scancode.l;
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
  print_endline "Key mappings :";
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
