external print_endline : string -> unit = "caml_print_endline"
external print_string : string -> unit = "caml_print_string"
external print_float : float -> unit = "caml_print_float"
external read_float : unit -> float = "caml_read_float"
external read_int : unit -> int = "caml_read_int"

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

let rec read_float_list = function
  | 0 -> []
  | n -> read_float () :: read_float_list (n - 1)

let rec print_float_list = function
  | [] -> print_endline ""
  | (hd :: tl) ->
      print_endline "";
      print_float hd;
      print_float_list tl

let () =
  print_string "- Enter length of list:\n> ";
  let length_of_list = read_int () in
  print_string "- Enter elements (space separated list of float numbers):\n> ";
  let sorted_list =
    read_float_list length_of_list
    |> List.sort
  in
  print_string "- Sorted_list:";
  print_float_list sorted_list
