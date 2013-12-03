external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let f = function
  | 0 -> 100
  | 1 -> 200
  | 3 -> 400
  | 6 -> 700
  | 10 -> 100
  | 15 -> 200
  | 21 -> 400
  | 28 -> 700
  | 40 -> 1000
  | 100 -> 1001
  | 400 -> 1002
  | _ -> -1

let println_int n =
  print_int n;
  print_endline ""

let () =
  println_int (f (-10));
  println_int (f 0);
  println_int (f 1);
  println_int (f 2);
  println_int (f 3);
  println_int (f 6);
  println_int (f 21);
  println_int (f 41);
  println_int (f 100);
  println_int (f 400);
  println_int (f 500)
