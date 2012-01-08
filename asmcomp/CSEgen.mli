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

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

class cse_generic : object
  (* The following methods can be overriden to handle processor-specific
     operations. *)

  method is_factorable_operation: Mach.operation -> bool
    (* Operations that can be factored: must be pure and produce at
       most one result.  As a special exception, bound checks can be
       factored, even though they can raise an exception.  *)

  method is_cheap_operation: Mach.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

  method is_load_operation: Mach.operation -> bool
    (* Operations that perform a memory read *)

  method is_store_operation: Mach.operation -> bool
    (* Operations that perform a memory store *)

  method is_checkbound_operation: Mach.operation -> bool
    (* Operations that perform a checkbound *)

  (* The following method is the entry point and should not be overridden *)
  method fundecl: Mach.fundecl -> Mach.fundecl

end



