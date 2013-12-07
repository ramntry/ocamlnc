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

let make_fun () =
  let coeffs =
    [|read_int (); read_int (); read_int (); read_int (); read_int ()|]
  in
  let f a1 a2 a3 a4 a5 =
    a1 * coeffs.(0) +
    a2 * coeffs.(1) +
    a3 * coeffs.(2) +
    a4 * coeffs.(3) +
    a5 * coeffs.(4)
  in
  f

let rec make_funs = function
  | 0 -> []
  | n -> make_fun () :: make_funs (n - 1)

let rec curry_all_1 a1_or_5 = function
  | [] -> []
  | hd :: tl -> hd a1_or_5 :: curry_all_1 a1_or_5 tl

let rec curry_all_3 a2 a3 a4 = function
  | [] -> []
  | hd :: tl -> hd a2 a3 a4 :: curry_all_3 a2 a3 a4 tl

let rec print_int_list = function
  | [] -> ()
  | hd :: tl ->
      print_int hd;
      print_endline "";
      print_int_list tl

let () =
  let numof_funs = read_int () in
  let funs = make_funs numof_funs in
  let xs =
    [|read_int (); read_int (); read_int (); read_int (); read_int ()|]
  in
  let ys = funs
    |> curry_all_1 xs.(0)
    |> curry_all_3 xs.(1) xs.(2) xs.(3)
    |> curry_all_1 xs.(4)
  in
  print_int_list ys

