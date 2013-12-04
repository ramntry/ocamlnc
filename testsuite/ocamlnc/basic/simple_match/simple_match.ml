external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

type adt = C1 | C2 | C3 | C4 | C5 | C6 | C7

let name_your_price = function
  | C1 -> 100
  | C2 -> 200
  | C3 -> 300
  | C4 -> 400
  | C5 -> 500
  | C6 -> 600
  | C7 -> 700

let () =
  if name_your_price C1 <> 100 then
    print_endline "FAIL"
  else
    print_endline "OK";
  if name_your_price C3 <> 300 then
    print_endline "FAIL"
  else
    print_endline "OK";
  if name_your_price C7 <> 700 then
    print_endline "FAIL"
  else
    print_endline "OK";
