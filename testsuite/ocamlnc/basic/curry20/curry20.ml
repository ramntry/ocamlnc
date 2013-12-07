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

let f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
  a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
  =
  a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
  + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20
  + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30
  + a31 + a32 + a33 + a34 + a35 + a36 + a37 + a38 + a39 + a40


let curry_it f =
  let a1 = read_int () in
  let a2 = read_int () in
  let a3 = read_int () in
  let a4 = read_int () in
  let a5 = read_int () in
  let a6 = read_int () in
  let a7 = read_int () in
  let a8 = read_int () in
  let a9 = read_int () in
  let a10 = read_int () in
  let a11 = read_int () in
  let a12 = read_int () in
  let a13 = read_int () in
  let a14 = read_int () in
  let a15 = read_int () in
  let a16 = read_int () in
  let a17 = read_int () in
  let a18 = read_int () in
  let a19 = read_int () in
  let a20 = read_int () in
  f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20

let () =
  let curried = curry_it f in
  let a1 = read_int () in
  let a2 = read_int () in
  let a3 = read_int () in
  let a4 = read_int () in
  let a5 = read_int () in
  let a6 = read_int () in
  let a7 = read_int () in
  let a8 = read_int () in
  let a9 = read_int () in
  let a10 = read_int () in
  let a11 = read_int () in
  let a12 = read_int () in
  let a13 = read_int () in
  let a14 = read_int () in
  let a15 = read_int () in
  let a16 = read_int () in
  let a17 = read_int () in
  let a18 = read_int () in
  let a19 = read_int () in
  let a20 = read_int () in
  let res =
    curried a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
  in
  print_int res;
  print_endline ""

