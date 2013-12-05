external string_length : string -> int = "%string_length";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

let string_reverse str =
  let length = string_length str in
  let new_string = create_string length in
  for i = 0 to length - 1 do
    new_string.[i] <- str.[length - 1 - i]
  done;
  new_string

let () =
  print_endline (string_reverse ("123456789"))
