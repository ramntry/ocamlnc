(* Implicit unpack allows to omit the signature in (val ...) expressions.
   It also adds (module M : S) and (module M) patterns, relying on
   implicit (val ...) for the implementation. *)

module type S = sig type t val x : t end;;
let f (module M : S with type t = int) = M.x;;
let f (module M : S with type t = 'a) = M.x;; (* Error *)
let f (type a) (module M : S with type t = a) = M.x;;

type 'a s = {s: (module S with type t = 'a)};;
let f {s=(module M)} = M.x;;
let f (type a) ({s=(module M)} : a s) = M.x;;

type s = {s: (module S with type t = int)};;
let f {s=(module M)} = M.x;;
let f {s=(module M)} {s=(module N)} = M.x + N.x;;

module type S = sig val x : int end;;
let f (module M : S) y (module N : S) = M.x + y + N.x;;

(* GADTs from the manual *)
(* the only modification is in to_string *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
  let refl = (fun x -> x), (fun x -> x)
  let apply (f, _) x = f x
  let sym (f, g) = (g, f)
end

module rec Typ : sig
  module type PAIR = sig
    type t and t1 and t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = Typ

let int = Typ.Int TypEq.refl

let str = Typ.String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  let pair = (module P : Typ.PAIR with type t = s1 * s2) in
  Typ.Pair pair

open Typ
let rec to_string: 'a. 'a Typ.typ -> 'a -> string =
  fun (type s) t x ->
    match (t : s typ) with
    | Int eq -> string_of_int (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair (module P) ->
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (to_string P.t1 x1) (to_string P.t2 x2)
