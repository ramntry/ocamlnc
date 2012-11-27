(*************************************************************************)
(*                                                                       *)
(*                                 OCaml                                 *)
(*                                                                       *)
(*         Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         *)
(*                                                                       *)
(*   Copyright 2007 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

val bindir : string
val libdir : string
val bytecc : string
val bytecclibs : string
val exe : string
val arch : string
val model : string
val system : string
val nativecc : string
val mklib : string -> string -> string -> string
val mkexe : string
val mkdll : string
val ext_dll : string
val o : string
val a : string
val so : string
val ccomptype : string
val supports_shared_libraries : bool

val mksharedlibrpath : string
val nativeccrpath : string

(* Useful only to bootstrap ocaml
val bytecccompopts : string
val bytecclinkopts : string
val sharedcccompopts : string
val nativecccompopts : string
val syslib : string -> string
val sharpbangscripts : bool
val bng_arch : string
val bng_asm_level : string
val pthread_link : string
val x11_includes : string
val x11_link : string
val tk_link : string
val extralibs : string
val tk_defs : string
*)