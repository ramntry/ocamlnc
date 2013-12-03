external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let f = function
  | 0 -> print_endline "zero"
  | 1 -> print_endline "unit"
  | 2 -> print_endline "two"
  | 3 -> print_endline "three"
  | 4 -> print_endline "four"
  | 5 -> print_endline "five"
  | _ -> print_endline "some"

let () =
  f 0;
  f 1;
  f 2;
  f 3;
  f 4;
  f 5;
  f 6;
  f 7
