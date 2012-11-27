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

let bindir = "$BINDIR"
let libdir = "$LIBDIR"
let bytecc = "$BYTECC"
let bytecclibs = "$BYTECCLIBS"
let exe = "$EXE"
let arch = "$ARCH"
let model = "$MODEL"
let system = "$SYSTEM"
let nativecc = "$NATIVECC"
let mklib  out files opts =
  Printf.sprintf "MKLIB %s %s %s %s" out opts files out
let mkexe = "$MKEXE"
let mkdll = "$MKDLL"
let ext_dll = "$EXT_DLL"
let o = "$EXT_OBJ"
let a = "$EXT_LIB"
let so = "$EXT_SO"
let ccomptype = "$CCOMPTYPE"
let supports_shared_libraries = true

let mksharedlibrpath = "$MKSHAREDLIBRPATH"
let nativeccrpath = "$NATIVECCRPATH"

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
