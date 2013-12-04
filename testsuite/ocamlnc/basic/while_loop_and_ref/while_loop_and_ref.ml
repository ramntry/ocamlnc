external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let sum n =
  let acc = ref 0 in
  let i = ref 1 in
  while !i <= n do
    acc := !acc + !i;
    i := !i + 1
  done;
  !acc

let () =
  print_int (sum 100);
  print_endline ""
