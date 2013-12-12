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
external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

let invalid_arg s = raise (Invalid_argument s)

let init l f =
  if l = 0 then [||] else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Array.blit"
  else unsafe_blit a1 ofs1 a2 ofs2 len

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

let cutoff = 5;;
let stable_sort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (get a src1ofs) src2ofs (get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp (get dst !j) e > 0) do
        set dst (!j + 1) (get dst !j);
        decr j;
      done;
      set dst (!j + 1) e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = make l2 (get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end

let fast_sort = stable_sort

let read_array len read_el_fun =
  init len (fun _ -> read_el_fun ())

let test read_el_fun print_el_fun =
  let size = read_int () in
  let input_array =  read_array size read_el_fun in
  print_array input_array print_el_fun;
  fast_sort (fun a b ->
    if a < b then -1
    else if b < a then 1
    else 0
  ) input_array;
  print_array input_array print_el_fun

let () =
  test read_float print_float
