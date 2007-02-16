(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Dynamic loading of native object files. *)

(** {6 Initialization} *)

val init : unit -> unit
(** Initialize the [Dynlink] library.
    Must be called before any other function in this module. *)

(** {6 Dynamic loading of compiled bytecode files} *)

val loadfile : string -> unit
(** Load the given bytecode object file ([.cmx] file) or
    bytecode library file ([.cmxa] file), and link it with the running program.
    All toplevel expressions in the loaded compilation units
    are evaluated. No facilities are provided to
    access value names defined by the unit. Therefore, the unit
    must register itself its entry points with the main program,
    e.g. by modifying tables of functions. *)

val loadfile_private : string -> unit
(** Same as [loadfile], except that the compilation units just loaded
    are hidden (cannot be referenced) from other modules dynamically
    loaded afterwards. *)

(*

(** {6 Access control} *)

val allow_only: string list -> unit
(** [allow_only units] restricts the compilation units that dynamically-linked
    units can reference: it only allows references to the units named in
    list [units].  References to any other compilation unit will cause
    a [Unavailable_unit] error during [loadfile] or [loadfile_private].

    Initially (just after calling [init]), all compilation units composing
    the program currently running are available for reference from
    dynamically-linked units.  [allow_only] can be used to grant access
    to some of them only, e.g. to the units that compose the API for
    dynamically-linked code, and prevent access to all other units,
    e.g. private, internal modules of the running program. *)

val prohibit: string list -> unit
(** [prohibit units] prohibits dynamically-linked units from referencing
    the units named in list [units].  This can be used to prevent
    access to selected units, e.g. private, internal modules of
    the running program. *)

val default_available_units: unit -> unit
(** Reset the set of units that can be referenced from dynamically-linked
    code to its default value, that is, all units composing the currently
    running program. *)

val allow_unsafe_modules : bool -> unit
(** Govern whether unsafe object files are allowed to be
    dynamically linked. A compilation unit is ``unsafe'' if it contains
    declarations of external functions, which can break type safety.
    By default, dynamic linking of unsafe object files is
    not allowed. *)

*)

(** {6 Error reporting} *)

type error

exception Error of error
(** Errors in dynamic linking are reported by raising the [Error]
    exception with a description of the error. *)

val error_message : error -> string
(** Convert an error description to a printable message. *)

