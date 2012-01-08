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

(* CSE for the i386 *)

open Arch
open Mach

class cse = object (self)

inherit CSEgen.cse_generic as super

(* Don't factor operations that affect the floating-point stack! *)

method! is_factorable_operation op =
  match op with
  | Iconst_float _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Iintoffloat | Ifloatofint | Iload((Single | Double | Double_u), _) -> false
  | Ispecific(Ilea _) -> true
  | Ispecific _ -> false
  | _ -> super#is_factorable_operation op

method! is_store_operation op =
  match op with
  | Ispecific(Istore_int _ | Istore_symbol _ | Ioffset_loc _) -> true
  | _ -> super#is_store_operation op

end

method! is_cheap_operation op =
  match op with
  | Iconst_int _ -> true
  | Iconst_symbol _ -> true
  | _ -> false

let fundecl f =
  (new cse)#fundecl f

