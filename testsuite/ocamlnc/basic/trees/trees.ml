external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;

let testing_list =
  [3.14159; 0.0; 2.71828; 1.0; 6.022; 1.602; 8.314; 6.67; 6.626; 1.38]

module Binary_tree = struct
  type 'a t =
    | Empty
    | Tree of 'a t * 'a * 'a t

  let rec add tree item =
    match tree with
    | Empty -> Tree (Empty, item, Empty)
    | Tree (left_child, root, right_child) ->
        if item < root
        then Tree (add left_child item, root, right_child)
        else Tree (left_child, root, add right_child item)

  let rec of_list = function
    | [] -> Empty
    | (hd :: tl) -> add (of_list tl) hd

  let to_list tree =
    let rec flatten_helper acc = function
    | Empty -> acc
    | Tree (left_child, root, right_child) ->
        flatten_helper (root :: flatten_helper acc right_child) left_child
    in
    flatten_helper [] tree
end

module List = struct
  let sort ls =
    Binary_tree.of_list ls
    |> Binary_tree.to_list
end

let rec print_float_list = function
  | [] -> print_endline ""
  | (hd :: tl) ->
      print_float hd;
      print_string " ";
      print_float_list tl

let () =
  let sorted_list =
    testing_list
    |> List.sort
  in
  print_float_list sorted_list
