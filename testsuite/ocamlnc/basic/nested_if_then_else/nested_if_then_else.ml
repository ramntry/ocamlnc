external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let f x =
  if x < 10 then
    (if x > 5 then "small" else "very small")
  else
    (if x < 15 then "big" else "very big")

let () =
  print_endline (f 0);
  print_endline (f 6);
  print_endline (f 11);
  print_endline (f 20);
