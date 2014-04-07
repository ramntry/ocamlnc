external make_vect : int -> 'a -> 'a array = "caml_make_vect";;
external array_sub : 'a array -> int -> int -> 'a array = "caml_array_sub";;
external array_append : 'a array -> 'a array -> 'a array = "caml_array_append";;
external array_concat : 'a array list -> 'a array = "caml_array_concat";;
external array_blit : 'a array -> int -> 'a array -> int -> int -> unit
                    = "caml_array_blit";;
external print_endline : string -> unit = "caml_print_endline";;
external print_string : string -> unit = "caml_print_string";;
external read_float : unit -> float = "caml_read_float";;
external print_float : float -> unit = "caml_print_float";;
external read_int : unit -> int = "caml_read_int";;
external print_int : int -> unit = "caml_print_int";;
external print_char : char -> unit = "caml_print_char";;
external create_string : int -> string = "caml_create_string";;

external int_of_string : string -> int = "caml_int_of_string"
external string_length : string -> int = "%string_length";;
external string_create : int -> string = "caml_create_string";;
external string_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string";;
external unsafe_fill : string -> int -> int -> char -> unit
                     = "caml_fill_string";;

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

let make n c =
  let s = create_string n in
  unsafe_fill s 0 n c;
  s

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

let string_of_char c = make 1 c

let read_file_succ =
    "pow (k, n) {\n"
  ^ "  r := 1;\n"
  ^ "\n"
  ^ "  while n >> 0 do {\n"
  ^ "    r := r * k;\n"
  ^ "    n := n - 1;\n"
  ^ "  }\n"
  ^ "\n"
  ^ "  return r;\n"
  ^ "}\n"
  ^ "\n"
  ^ "fact (n) {\n"
  ^ "  if n <= 1 then return 1;\n"
  ^ "  else return n * fact (n-1);\n"
  ^ "}\n"
  ^ "\n"
  ^ "main () {\n"
  ^ "  read (n);\n"
  ^ "  write (fact (n));\n"
  ^ "}"

let read_file_fail =
    "pow (k, n) {\n"
  ^ "  r := 1;\n"
  ^ "\n"
  ^ "  while n >> 0 do {\n"
  ^ "    r := r * k;\n"
  ^ "    n := n - 1;\n"
  ^ "  }\n"
  ^ "\n"
  ^ "  return r;\n"
  ^ "}\n"
  ^ "\n"
  ^ "fact (n) {\n"
  ^ "  if n <= 1 return 1;\n"
  ^ "  else return n * fact (n-1);\n"
  ^ "}\n"
  ^ "\n"
  ^ "main () {\n"
  ^ "  read (n);\n"
  ^ "  write (fact (n));\n"
  ^ "}"

(* ========= Prerequisites ======= *)

(* let string_length = String.length *)
(* let fold_left = List.fold_left *)
(* let (^) = (^) *)
(* let string_of_char c = String.make 1 c *)
(* let int_of_string = int_of_string *)
(* let print_string = print_string *)
(* let read_file name = 
     let inch = open_in_bin name in
     let len  = in_channel_length inch in
     let buf  = String.make len ' ' in
     really_input inch buf 0 len;
     close_in inch;
     buf *)

(* ========= Stream type ========= *)
type stream = char list

(* Stream constructor *)
let of_string s = 
  let n = string_length s in
  let rec inner i =
    if i < n then s.[i] :: inner (i+1) else []
  in
  inner 0

(* String conversion *)
let rec to_string = function
| [] -> ""
| x::xs -> (string_of_char x) ^ to_string xs
  
(* ===========Parser type ======== *)
type 'a p = stream -> (stream * 'a) option

(* -------- Basic parsers: ------- *)

let void _   = ()
let return x = fun _ -> x

(* End of file *)
let eof = function
| [] -> Some ([], ())
| _  -> None

(* Empty *)
let empty s = Some (s, ())

(* Check one symbol *)
let check p = function
| x::xs when p x -> Some (xs, x)
| _ -> None

(* Boolean operators on predicates *)
let not_p p   = fun c -> not (p c)
let and_p p q = fun c -> p c && q c
let or_p  p q = fun c -> p c or q c

(* One symbol, interval *)
let one (c : char)                = fun x -> x = c
let between (l : char) (u : char) = fun x -> x >= l && x <= u

(* ----------- Basic combinators ------ *)

(* Alternative *)
let alt  p q s = match p s with (Some _) as x -> x | _ -> q s
let (<|>) = alt

(* Optional *)
let opt p s = match p s with Some (s, x) -> Some (s, Some x) | _ -> Some (s, None)
let (<?>) = opt

(* Monadic sequence *)
let seq  p q s = match p s with Some (s, x) -> q x s | _ -> None
let (|>) = seq

(* Semantic map *)
let map f p s = match p s with Some (s', x) -> Some (s', f x) | _ -> None
let (-->) p f = map f p

(* Iterations *)
let rec manyFold f init p s =
match p s with
| Some (s', x) -> manyFold f (f init x) p s'
| _            -> Some (s, init)

let many p = 
(manyFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])
let (<*>) = many

let someFold f p = p |> (fun h -> manyFold f h p)
let some p = someFold (fun acc x -> acc @ x) p
let (<+>) = some

let lookahead p s = match p s with Some (_, x) -> Some (s, x) | _ -> None

(* ------------- Lexical predicates  ------------ *)

let digit   = between '0' '9'
let letter  = or_p (one '_') (or_p (between 'a' 'z') (between 'A' 'Z'))
let space   = or_p (one ' ') (or_p (one '\n') (or_p (one '\t') (one '\r')))
let word    = or_p letter digit
let nonword = not_p (or_p digit letter)

(* ------------- Lexical parsers -------------- *)

let ws       = many (check space) --> void
let break    = lookahead (eof <|> check nonword --> void)
let spaced p = p |> (fun x -> many (check space) --> return x)

let image s   = fold_left (fun acc c -> acc |> return (check (one c) --> void)) empty (of_string s)
let ident     = spaced ((check letter) |> (fun x -> many (check word) |> (fun xs -> break --> return (to_string (x::xs)))))
let const     = spaced (some (check digit --> (fun x -> [x])) --> (fun x -> int_of_string (to_string x)))
let keyword s = spaced (image s |> (fun x -> break --> return x))
let string  s = spaced (image s)
let list    p = p |> (fun x -> many (string "," |> return p) --> (fun xs -> x::xs)) <|> empty --> return []

(* --------------- Expressions --------------- *)

type expr = 
Ident of string 
| Const of int 
| Call  of string * expr list
| Add   of expr * expr
| Sub   of expr * expr
| Mul   of expr * expr
| Div   of expr * expr
| Rem   of expr * expr
| Neg   of expr
| Eq    of expr * expr
| Ne    of expr * expr
| Le    of expr * expr
| Lt    of expr * expr
| Ge    of expr * expr
| Gt    of expr * expr
| And   of expr * expr
| Or    of expr * expr
| Not   of expr 

let primary expr = 
const --> (fun c -> Const c) <|> 
(ident |> (fun x -> opt (string "(" |> return (list expr |> (fun p -> string ")" --> return p))) --> (fun p -> match p with None -> Ident x | Some p -> Call (x, p)))) <|>
(string "(" |> return (expr |> (fun e -> string ")" --> return e)))

let add = string "+" --> return (fun x y -> Add (x, y))
let sub = string "-" --> return (fun x y -> Sub (x, y))
let mul = string "*" --> return (fun x y -> Mul (x, y))
let div = string "/" --> return (fun x y -> Div (x, y))
let rem = string "%" --> return (fun x y -> Rem (x, y))

let eq = string "==" --> return (fun x y -> Eq (x, y))
let ne = string "!=" --> return (fun x y -> Ne (x, y))
let le = string "<=" --> return (fun x y -> Le (x, y))
let lt = string "<<" --> return (fun x y -> Lt (x, y))
let ge = string ">=" --> return (fun x y -> Ge (x, y))
let gt = string ">>" --> return (fun x y -> Gt (x, y))

let conj = string "&&" --> return (fun x y -> And (x, y))
let disj = string "||" --> return (fun x y -> Or  (x, y))

let neg  = string "-" --> return (fun x -> Neg x)
let lnot = string "!" --> return (fun x -> Not x)

let compi = eq  <|> ne <|> le <|> lt <|> ge <|> gt
let addi  = add <|> sub
let multi = mul <|> div <|> rem 
let uni   = neg <|> lnot

let lefta op opnd = 
  opnd |> (fun l -> manyFold (fun l s -> s l) l (op |> (fun s -> opnd --> (fun y -> (fun x -> s x y)))))

let nona op opnd = 
  opnd |> (fun l -> opt (op |> (fun s -> opnd --> (fun r -> fun l -> s l r))) --> (function None -> l | Some s -> s l))

let level0 opnd = lefta disj  opnd
let level1 opnd = lefta conj  opnd
let level2 opnd = nona  compi opnd
let level3 opnd = lefta addi  opnd
let level4 opnd = lefta multi opnd
let level5 opnd = opt uni |> (fun u -> opnd --> fun p -> match u with None -> p | Some s -> s p)

let rec expression s = 
  level0 (level1 (level2 (level3 (level4 (level5 (primary expression)))))) s

(* --------------------- Statements --------------------- *)

type stmt =
  Assign of string * expr
| Cond   of expr * stmt * stmt
| Read   of string
| Write  of expr
| While  of expr * stmt
| Seq    of stmt list
| Return of expr
 
let assign     = ident |> (fun x -> string ":=" |> return (expression |> (fun e -> string ";" --> return (Assign (x, e)))))
let compound s = string "{" |> return (many s |> (fun s -> string "}" --> return (Seq s)))
let cond     s = keyword "if" |> return (expression |> (fun c -> 
                 keyword "then" |> (return (s |> (fun t -> 
                 keyword "else" |> return (s --> (fun e -> Cond (c, t, e))))))))
let read       = keyword "read"  |> return (string "(" |> return (ident |> (fun x -> string ")" |> return (string ";" --> return (Read x)))))
let write      = keyword "write" |> return (string "(" |> return (expression |> (fun e -> string ")" |> return (string ";" --> return (Write e)))))
let ret        = keyword "return" |> return (expression |> (fun e -> string ";" --> return (Return e)))
let loop s     = keyword "while" |> return (expression |> (fun e -> keyword "do" |> return (s --> (fun s -> While (e, s)))))

let rec statement s =
  (ret <|> read <|> write <|> cond statement <|> loop statement <|> compound statement <|> assign) s

(* -------------- Functions and program ---------------- *)

let fdecl   = ident |> (fun fname -> string "(" |> return (list ident |> (fun fargs -> string ")" |> return (statement --> (fun fbody -> (fname, fargs, fbody))))))
let program = many fdecl |> (fun p -> eof --> return p)

(* --------------- testing --------------- 

let inspect f = function
| Some (s, x) -> Printf.printf "Success: %s, %s\n" (to_string s) (f x)
| _ -> Printf.printf "Failure\n"

let _ =
  let s = of_string "{y := f (a, b, c) ; read (x);  x:=234 + 3 * 8 == x; write (x+3); }" in
  inspect show_stmt (statement s) 
*)

let _ = 
  begin match program (of_string read_file_succ) with
  | Some _ -> print_string "Success.\n"
  | None   -> print_string "Failure.\n"
  end;
  begin match program (of_string read_file_fail) with
  | Some _ -> print_string "Success.\n"
  | None   -> print_string "Failure.\n"
  end
