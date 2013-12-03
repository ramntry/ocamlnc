external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let f = function
  | 100.0 -> "1100"
  | 200.0 -> "2100"
  | 300.0 -> "3100"
  | 400.0 -> "4100"
  | 500.0 -> "5100"
  | 600.0 -> "6100"

let () =
  print_endline (f (float_of_int 100));
  print_endline (f (float_of_int 300));
  print_endline (f (float_of_int 500))
