external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

let cube = [|
  [| [| 0;  1;  2|];
     [| 3;  4;  5|];
     [| 6;  7;  8|] |];

  [| [| 9; 10; 11|];
     [|12; 13; 14|];
     [|15; 16; 17|] |];

  [| [|18; 19; 20|];
     [|21; 22; 23|];
     [|24; 25; 26|] |]
|]

let () =
  for i = 0 to 2 do
    for j = 0 to 2 do
      for k = 0 to 2 do
        print_int cube.(i).(j).(k);
        print_string ";"
      done;
      print_endline ""
    done;
    print_endline ""
  done
