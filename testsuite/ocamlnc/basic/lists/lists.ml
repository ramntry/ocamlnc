external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let list_rev ls =
  let rec rev_helper acc = function
    | [] -> acc
    | (hd :: tl) -> rev_helper (hd :: acc) tl
  in
  rev_helper [] ls

let rec print_list = function
  | [] -> print_endline ""
  | (hd :: tl) ->
      print_int hd;
      print_string " ";
      print_list tl

let () =
  print_list (list_rev [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])
