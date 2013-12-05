external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

let rec f = function
  | 0 -> 1.0
  | 2 -> 1.0
  | 4 -> 1.0
  | 6 -> 2.0
  | 8 -> 1.0
  | n -> f (n - 4) +. f (n - 8) +. f (n - 10)

let () =
  print_float (f 40);
  print_endline ""
