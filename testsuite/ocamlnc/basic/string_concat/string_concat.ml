external string_length : string -> int = "%string_length";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external string_create : int -> string = "caml_create_string";;
external string_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc";;

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

let () =
  let hello = "Hello" in
  let world = " World!" in
  print_endline (hello ^ world)
