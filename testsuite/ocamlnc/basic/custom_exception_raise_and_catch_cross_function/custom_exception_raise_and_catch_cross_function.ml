external make_vect : int -> 'a -> 'a array = "caml_make_vect";;
external array_sub : 'a array -> int -> int -> 'a array = "caml_array_sub";;
external array_append : 'a array -> 'a array -> 'a array = "caml_array_append";;
external array_blit : 'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

exception Exception1
exception Exception2
exception Exception3

let f0 arg =
  if arg = 0
    then raise Exception1

let f1 arg =
  try
    print_endline "Within f1 before call f0";
    f0 arg;
    print_endline "TEST FAIL0"
  with Exception2 ->
    print_endline "TEST FAIL1"

let f2 arg =
  begin try
    if arg = 0
      then raise Exception2;
    print_endline "TEST FAIL2"
  with Exception2 ->
    print_endline "TEST OK0"
  end;
  print_endline "Within f2 before call f1";
  f1 arg;
  print_endline "TEST FAIL2"

let f3 arg =
  try
    print_endline "Within f3 before call f2";
    f2 arg;
    print_endline "TEST FAIL2"
  with
  | Exception2 ->
      print_endline "TEST FAIL3"
  | Exception1 ->
      print_endline "TEST OK1";
      raise Exception3

let () =
  f3 (read_int ())
