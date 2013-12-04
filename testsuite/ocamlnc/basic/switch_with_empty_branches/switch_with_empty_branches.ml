external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let f = function
  | 0 -> ()
  | 1 -> ()
  | 2 -> print_endline "prime number"
  | 3 -> print_endline "prime number"
  | 4 -> ()
  | 5 -> print_endline "prime number"
  | 6 -> ()
  | 7 -> print_endline "prime number"
  | 8 -> ()
  | 9 -> ()
  | _ -> print_endline "too big number"

let () =
  (f 0);
  (f 1);
  (f 2);
  (f 4);
  (f 5);
  (f 7);
  (f 10)
