external make_vect : int -> 'a -> 'a array = "caml_make_vect";;
external array_sub : 'a array -> int -> int -> 'a array = "caml_array_sub";;
external array_append : 'a array -> 'a array -> 'a array = "caml_array_append";;
external array_blit : 'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;

exception Exception1_1         (* 0 *)
exception Exception2_2         (* 1 *)
exception Exception2t_2t       (* 2 *)
exception Exception2tt_2t      (* 3 *)
exception Exception2tw_2       (* 4 *)
exception Exception2ttt_2ttt   (* 5 *)
exception Exception2tttt_2     (* 6 *)
exception Exception_uncatched  (* o/w *)

let f () =
  let result = ref 0 in
  let while_cond = ref true in
  while !while_cond do
    let arg = read_int () in
    begin try
      (* 1 *)
      result := begin
        try
          if arg = 0 then begin
            raise Exception1_1;
            result := -10;
            raise Exception_uncatched
          end;
          20
        with Exception1_1 ->
          print_endline "Exception1_1";
          10
      end;
      print_int !result;
      print_endline "";
      (* 2 *)
      result := begin
        try (* 2t *)
          result := begin
            try (* 2tt *)
              result := begin
                try (* 2ttt *)
                  result := begin
                    if arg = 6 then begin
                      raise Exception2tttt_2;
                      result := -4;
                      raise Exception_uncatched
                    end;
                    26
                  end;
                  print_int !result;
                  print_endline "";
                  if arg = 5 then begin
                    raise Exception2ttt_2ttt;
                    result := -5;
                    raise Exception_uncatched
                  end;
                  25
                with Exception2ttt_2ttt ->
                  print_endline "Exception2ttt_2ttt";
                  15
              end;
              print_int !result;
              print_endline "";
              if arg = 4 then
                raise Exception_uncatched;
              result := begin
                if arg = 3 then begin
                  raise Exception2tt_2t;
                  result := -7;
                  raise Exception_uncatched
                end;
                23
              end;
              print_int !result;
              print_endline "";
              if arg = 2 then begin
                raise Exception2t_2t;
                result := -8;
                raise Exception_uncatched
              end;
              22
            with (* 2tw *)
            | Exception2t_2t ->
                print_endline "Exception2t_2t";
                12
            | Exception2tt_2t ->
                print_endline "Exception2tt_2t";
                13
            | Exception_uncatched ->
                raise Exception2tw_2
          end;
          print_int !result;
          print_endline "";
          if arg = 1 then begin
            raise Exception2_2;
            result := -9;
            raise Exception_uncatched
          end;
          21
        with (* 2w *)
        | Exception2_2 ->
            print_endline "Exception2_2";
            11
        | Exception2tw_2 ->
            print_endline "Exception2tw_2";
            14
        | Exception2tttt_2 ->
            print_endline "Exception2tttt_2";
            16
      end;
      print_int !result;
      print_endline ""
    with Exception_uncatched ->
      print_endline "Exception_uncatched";
      while_cond := false
    end;
    if arg < 0
      then while_cond := false
  done;
  !result

let () =
  print_int (f ());
  print_endline ""
