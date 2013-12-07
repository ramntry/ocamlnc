external make_vect : int -> 'a -> 'a array = "caml_make_vect";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

let rec f n = if n < 2 then n else g (n - 1) + 2 * h (n - 2)
    and g n = if n < 2 then n else f (n - 1) +     h (n - 1)
    and h n = if n < 2 then n else f (n - 1) + 2 * g (n - 2)

let () =
  let a = read_int () in
  let res = f a in
  print_int res;
  print_endline ""
