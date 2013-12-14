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

exception Custom_exception1
exception Custom_exception2

let f () =
  try
    let ans = read_int () in
    if ans < 0
      then raise Custom_exception1
    else if ans > 0
      then raise Custom_exception2
    else
      print_endline "It is the zero!"
  with
  | Custom_exception1 ->
      print_endline "It is a negative number!"
  | Custom_exception2 ->
      print_endline "It is a positive number!"

let () =
  f ();
  f ();
  f ()
