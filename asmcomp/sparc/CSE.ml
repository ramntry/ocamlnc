(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: split.ml 11156 2011-07-27 14:17:02Z doligez $ *)

(* CSE for Sparc *)

open Arch
open Mach
open CSEgen

class cse = object (self)

inherit cse_generic as super

method! is_cheap_operation op =
  match op with
  | Iconst_int n -> n <= 4095n && n >= -4096n
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f

