external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let rec fib n =
  if n < 3
  then 1.0
  else fib (n - 1) +. fib (n - 2)

let () =
  print_float (fib 10);
  print_endline ""
