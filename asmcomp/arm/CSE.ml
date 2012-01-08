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

(* CSE for ARM *)

open Arch
open Mach

class cse = object (self)

inherit CSEgen.cse_generic as super

method! is_factorable_operation op =
  match op with
  | Ispecific _ -> true
  | _ -> super#is_factorable_operation op

method! is_checkbound_operation op =
  match op with
  | Ispecific(Ishiftcheckbound _) -> true
  | _ -> super#is_checkbound_operation op

end

let fundecl f =
  (new cse)#fundecl f

