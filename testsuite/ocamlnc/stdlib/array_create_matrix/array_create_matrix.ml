external make_vect : int -> 'a -> 'a array = "caml_make_vect";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;
external string_length : string -> int = "%string_length";;
external string_create : int -> string = "caml_create_string";;
external string_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc";;

external length : 'a array -> int = "%array_length"
external get: 'a array -> int -> 'a = "%array_safe_get"
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external make: int -> 'a -> 'a array = "caml_make_vect"
external create: int -> 'a -> 'a array = "caml_make_vect"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

let init l f =
  if l = 0 then [||] else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res

let make_matrix sx sy init =
  let res = create sx [||] in
  for x = 0 to pred sx do
    unsafe_set res x (create sy init)
  done;
  res

let create_matrix = make_matrix

let test init print_function test_name =
  print_endline ("run test '" ^ test_name ^ "':");
  let height = read_int () in
  let width = read_int () in
  print_string "height = ";
  print_int height;
  print_string "\nwidth = ";
  print_int width;
  print_endline "";
  let matrix = create_matrix height width init in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      print_function (get (get matrix i) j);
      print_string ""
    done;
    print_endline ""
  done

let () =
  test 1 print_int "ints";
  test 1.0 print_float "doubles";
  test "1" print_string "strings"
