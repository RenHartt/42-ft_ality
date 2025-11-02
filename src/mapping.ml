type key = int
type binding = key * Grammar.touch
type t = binding list

type error =
  | Not_enough_scancodes

let string_of_error = function
  | Not_enough_scancodes -> "not enough SDL scancodes for inputs"

let keys : key list =
  [
    Tsdl.Sdl.Scancode.left;  Tsdl.Sdl.Scancode.right;
    Tsdl.Sdl.Scancode.up;    Tsdl.Sdl.Scancode.down;
    Tsdl.Sdl.Scancode.q;     Tsdl.Sdl.Scancode.w;
    Tsdl.Sdl.Scancode.e;     Tsdl.Sdl.Scancode.r;
    Tsdl.Sdl.Scancode.a;     Tsdl.Sdl.Scancode.s;
    Tsdl.Sdl.Scancode.z;     Tsdl.Sdl.Scancode.x;
    Tsdl.Sdl.Scancode.d;     Tsdl.Sdl.Scancode.f;
    Tsdl.Sdl.Scancode.c;     Tsdl.Sdl.Scancode.v;
    Tsdl.Sdl.Scancode.t;     Tsdl.Sdl.Scancode.y;
    Tsdl.Sdl.Scancode.g;     Tsdl.Sdl.Scancode.h;
    Tsdl.Sdl.Scancode.b;     Tsdl.Sdl.Scancode.n;
    Tsdl.Sdl.Scancode.u;     Tsdl.Sdl.Scancode.i;
    Tsdl.Sdl.Scancode.j;     Tsdl.Sdl.Scancode.k;
    Tsdl.Sdl.Scancode.m;     Tsdl.Sdl.Scancode.o;
    Tsdl.Sdl.Scancode.p;     Tsdl.Sdl.Scancode.l;
  ]

let make (g : Grammar.t) : (t, error) result =
  let rec pair acc ks ins =
    match ks, ins with
    | _, [] -> Ok (List.rev acc)
    | [], _ -> Error Not_enough_scancodes
    | k :: kt, t :: tt -> pair ((k, t) :: acc) kt tt
  in
  pair [] keys g.Grammar.inputs

let key_name (k : key) : string =
  Tsdl.Sdl.get_scancode_name k

let find (m : t) (k : key) : Grammar.touch option =
  List.assoc_opt k m

let pp (m : t) : string =
  let b = Buffer.create 128 in
  Buffer.add_string b "Key mappings:\n";
  List.iter
    (fun (k, t) ->
      Buffer.add_string b (Printf.sprintf "%s -> %s\n" (key_name k) t.Grammar.raw))
    m;
  Buffer.add_string b "----------------------\n";
  Buffer.contents b

let print (m : t) : unit =
  pp m |> print_string
