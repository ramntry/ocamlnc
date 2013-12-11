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
external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external make: int -> 'a -> 'a array = "caml_make_vect"
external create: int -> 'a -> 'a array = "caml_make_vect"

let invalid_arg s = raise (Invalid_argument s)

let init l f =
  if l = 0 then [||] else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res

let print_array a print_el_fun =
  print_string "[|";
  begin match a with
  | [||] -> ()
  | _ ->
      print_el_fun (get a 0);
      for i = 1 to length a - 1 do
        print_string "; ";
        print_el_fun (get a i)
      done
  end;
  print_endline "|]"

let sub a ofs len =
  if len < 0 || ofs > length a - len
  then invalid_arg "Array.sub"
  else unsafe_sub a ofs len

let test array_gen print_el_fun =
  let src_size = read_int () in
  let sub_start = read_int () in
  let sub_size = read_int () in
  let src_array = init src_size array_gen in
  let sub_array = sub src_array sub_start sub_size in
  print_array src_array print_el_fun;
  print_array sub_array print_el_fun

let () =
  test (fun i -> i) print_int;
  test (fun i -> float_of_int i) print_float
