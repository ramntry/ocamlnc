external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;
external blit_string : string -> int -> string -> int -> int -> unit = "caml_blit_string";;
external make_vect : int -> 'a -> 'a array = "caml_make_vect"
external length : 'a array -> int = "%array_length"

let rec sort arr =
  let len = length arr in
  if len < 2 then arr
  else begin
    let lhs_len = len / 2 in
    let rhs_len = len - lhs_len in
    let lhs = make_vect lhs_len 0 in
    for i = 0 to lhs_len - 1 do
      lhs.(i) <- arr.(i)
    done;
    let rhs = make_vect rhs_len 0 in
    for i = 0 to rhs_len - 1 do
      rhs.(i) <- arr.(i + lhs_len)
    done;
    let sorted_lhs = sort lhs in
    let sorted_rhs = sort rhs in
    let i = ref 0 in
    let j = ref 0 in
    for k = 0 to len - 1 do begin
      if !i = lhs_len then begin
        arr.(k) <- rhs.(!j);
        j := !j + 1
      end
      else if !j = rhs_len || lhs.(!i) < rhs.(!j) then begin
        arr.(k) <- lhs.(!i);
        i := !i + 1
      end
      else begin
        arr.(k) <- rhs.(!j);
        j := !j + 1
      end
    end done;
    arr
  end

let some_array =
  [|61; 59; 78; 95; 78; 97; 99; 13; 13; 46; 23; 63; 25; 70; 69; 24; 88; 95;
    29; 12; 39; 86; 14; 78; 19; 38; 39; 82; 17; 20; 67; 11; 77; 50; 72; 31;
    42; 48; 10; 76; 19; 55; 61; 70; 34; 68; 29; 56; 95; 38; 68; 87; 93; 40;
    73; 98; 74; 84; 31; 96; 83; 80; 63; 63; 37; 22; 85; 59; 25; 45; 43; 60;
    63; 90; 36; 29; 57; 86; 41; 79; 47; 88; 12; 44; 80; 58; 74; 22; 87; 59;
    48; 75; 86; 61; 82; 54; 91; 62; 55; 18|]

let print_array arr =
  let len = length arr in
  for i = 0 to len - 1 do
    print_int arr.(i);
    if (i + 1) mod 18 = 0
    then print_endline ";"
    else print_string "; "
  done

let () =
  let sorted = sort some_array in
  print_array sorted;
  print_endline ""
