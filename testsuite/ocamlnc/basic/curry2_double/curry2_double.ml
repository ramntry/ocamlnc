external print_endline : string -> unit = "caml_print_endline"
external print_string : string -> unit = "caml_print_string"
external read_float : unit -> float = "caml_read_float"
external print_float : float -> unit = "caml_print_float"
external read_int : unit -> int = "caml_read_int"
external print_int : int -> unit = "caml_print_int"

module Uncurried = struct
  let uncurried_f a b = a +. b
  let try_to_curry_with _c = uncurried_f
end

module Curried = struct
  let uncurried_f a b c = a +. b +. c
  let try_to_curry_with c = uncurried_f c
end

let read_float_from_user prompt =
  print_string prompt;
  read_float ()

let read_int_from_user prompt =
  print_string prompt;
  read_int ()

let main () =
  let is_used_curried = read_int_from_user "0 to use Uncurried, 1 to use Curried: " in
  let to_curry =
    if is_used_curried = 1
    then Curried.try_to_curry_with
    else Uncurried.try_to_curry_with
  in
  let a = read_float_from_user "a = " in
  let b = read_float_from_user "b = " in
  let c = read_float_from_user "c = " in
  let maybe_curried = to_curry c in
  print_string "result: ";
  print_float (maybe_curried a b);
  print_endline ""

let () =
  main ();
  main ()
