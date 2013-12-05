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

let rec fibonacci_string = function
  | 1 -> "a"
  | 2 -> "b"
  | n ->
      let lhs = fibonacci_string (n - 2) in
      let lhs_len = string_length lhs in
      let rhs = fibonacci_string (n - 1) in
      let rhs_len = string_length rhs in
      print_string "Try to concat strings: ";
      print_string lhs;
      print_char '(';
      print_int lhs_len;
      print_char ')';

      print_string ", ";
      print_string rhs;
      print_char '(';
      print_int rhs_len;
      print_char ')';

      print_string " => ";
      let result = lhs ^ rhs in
      let result_len = string_length result in
      print_string result;
      print_char '(';
      print_int result_len;
      print_endline ")";
      result

let () =
  print_endline (fibonacci_string 10)
