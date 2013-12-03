external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let quad a = a * a;;
let quad_of_sum a b = quad (a + b);;
let f a b c = quad_of_sum a b - quad_of_sum b c;;
print_int (f 3 2 4);;
print_endline "";;
