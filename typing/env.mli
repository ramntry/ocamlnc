(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Environment handling *)

open Types

module EnvLazy : sig
  type ('a,'b) t

  val force : ('a -> 'b) -> ('a,'b) t -> 'b
  val create : 'a -> ('a,'b) t
end

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_exception of summary * Ident.t * exception_declaration
  | Env_module of summary * Ident.t * module_type
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t

module EnvTbl : sig
  type 'a t

  val find_same_not_using : Ident.t -> 'a t -> 'a
  val keys : 'a t -> Ident.t list
end

type t = {
  values: (Path.t * value_description) EnvTbl.t;
  annotations: (Path.t * Annot.ident) EnvTbl.t;
  constrs: (Path.t * constructor_description) EnvTbl.t;
  labels: (Path.t * label_description) EnvTbl.t;
  constrs_by_path: (Path.t * (constructor_description list)) EnvTbl.t;
  types: (Path.t * type_declaration) EnvTbl.t;
  modules: (Path.t * module_type) EnvTbl.t;
  modtypes: (Path.t * modtype_declaration) EnvTbl.t;
  components: (Path.t * module_components) EnvTbl.t;
  classes: (Path.t * class_declaration) EnvTbl.t;
  cltypes: (Path.t * class_type_declaration) EnvTbl.t;
  summary: summary;
  local_constraints: bool;
  gadt_instances: (int * Btype.TypeSet.t ref) list;
  in_signature: bool;
}

and module_components = (t * Subst.t * Path.t * Types.module_type,module_components_repr) EnvLazy.t

and module_components_repr =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_annotations: (string, (Annot.ident * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
  mutable comp_labels: (string, (label_description * int)) Tbl.t;
  mutable comp_constrs_by_path:
      (string, (constructor_description list * int)) Tbl.t;
  mutable comp_types: (string, (type_declaration * int)) Tbl.t;
  mutable comp_modules: (string, ( (Subst.t * Types.module_type,module_type) EnvLazy.t * int)) Tbl.t;
  mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
  mutable comp_components: (string, (module_components * int)) Tbl.t;
  mutable comp_classes: (string, (class_declaration * int)) Tbl.t;
  mutable comp_cltypes: (string, (class_type_declaration * int)) Tbl.t
}

and functor_components = {
  fcomp_param: Ident.t;                 (* Formal parameter *)
  fcomp_arg: module_type;               (* Argument signature *)
  fcomp_res: module_type;               (* Result signature *)
  fcomp_env: t;     (* Environment in which the result signature makes sense *)
  fcomp_subst: Subst.t;  (* Prefixing substitution for the result signature *)
  fcomp_cache: (Path.t, module_components) Hashtbl.t  (* For memoization *)
}


val empty: t
val initial: t
val diff: t -> t -> Ident.t list

(* Lookup by paths *)

val find_value: Path.t -> t -> value_description
val find_annot: Path.t -> t -> Annot.ident
val find_type: Path.t -> t -> type_declaration
val find_constructors: Path.t -> t -> constructor_description list
val find_module: Path.t -> t -> module_type
val find_modtype: Path.t -> t -> modtype_declaration
val find_class: Path.t -> t -> class_declaration
val find_cltype: Path.t -> t -> class_type_declaration

val find_type_expansion:
    ?level:int -> Path.t -> t -> type_expr list * type_expr * int option
val find_type_expansion_opt:
    Path.t -> t -> type_expr list * type_expr * int option
(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion: Path.t -> t -> Types.module_type

val has_local_constraints: t -> bool
val add_gadt_instance_level: int -> t -> t
val gadt_instance_level: t -> type_expr -> int option
val add_gadt_instances: t -> int -> type_expr list -> unit
val add_gadt_instance_chain: t -> int -> type_expr -> unit

(* Lookup by long identifiers *)

val lookup_value: Longident.t -> t -> Path.t * value_description
val lookup_annot: Longident.t -> t -> Path.t * Annot.ident
val lookup_constructor: Longident.t -> t -> Path.t * constructor_description
val lookup_label: Longident.t -> t -> Path.t * label_description
val lookup_type: Longident.t -> t -> Path.t * type_declaration
val lookup_module: Longident.t -> t -> Path.t * module_type
val lookup_modtype: Longident.t -> t -> Path.t * modtype_declaration
val lookup_class: Longident.t -> t -> Path.t * class_declaration
val lookup_cltype: Longident.t -> t -> Path.t * class_type_declaration

(* Insertion by identifier *)

val add_value: ?check:(string -> Warnings.t) -> Ident.t -> value_description -> t -> t
val add_annot: Ident.t -> Annot.ident -> t -> t
val add_type: Ident.t -> type_declaration -> t -> t
val add_exception: Ident.t -> exception_declaration -> t -> t
val add_module: Ident.t -> module_type -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_class: Ident.t -> class_declaration -> t -> t
val add_cltype: Ident.t -> class_type_declaration -> t -> t
val add_local_constraint: Ident.t -> type_declaration -> int -> t -> t

(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
val add_signature: signature -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. *)

val open_signature: ?loc:Location.t -> Path.t -> signature -> t -> t
val open_pers_signature: string -> t -> t

(* Insertion by name *)

val enter_value: ?check:(string -> Warnings.t) -> string -> value_description -> t -> Ident.t * t
val enter_type: string -> type_declaration -> t -> Ident.t * t
val enter_exception: string -> exception_declaration -> t -> Ident.t * t
val enter_module: string -> module_type -> t -> Ident.t * t
val enter_modtype: string -> modtype_declaration -> t -> Ident.t * t
val enter_class: string -> class_declaration -> t -> Ident.t * t
val enter_cltype: string -> class_type_declaration -> t -> Ident.t * t

(* Initialize the cache of in-core module interfaces. *)
val reset_cache: unit -> unit
val reset_missing_cmis: unit -> unit

(* Remember the name of the current compilation unit. *)
val set_unit_name: string -> unit

(* Read, save a signature to/from a file *)

val read_signature: string -> string -> signature
        (* Arguments: module name, file name. Results: signature. *)
val save_signature: signature -> string -> string -> unit
        (* Arguments: signature, module name, file name. *)
val save_signature_with_imports:
            signature -> string -> string -> (string * Digest.t) list -> unit
        (* Arguments: signature, module name, file name,
           imported units with their CRCs. *)

(* Return the CRC of the interface of the given compilation unit *)

val crc_of_unit: string -> Digest.t

(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list

(* Direct access to the table of imported compilation units with their CRC *)

val crc_units: Consistbl.t

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

val summary: t -> summary

(* Error report *)

type error =
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string

exception Error of error

open Format

val report_error: formatter -> error -> unit


val mark_value_used: string -> value_description -> unit
val mark_type_used: string -> type_declaration -> unit

type constructor_usage = [`Positive|`Pattern|`Privatize]
val mark_constructor_used: constructor_usage -> string -> type_declaration -> string -> unit
val mark_constructor: constructor_usage -> t -> string -> constructor_description -> unit
val mark_exception_used: constructor_usage -> exception_declaration -> string -> unit

val in_signature: t -> t

val set_value_used_callback: string -> value_description -> (unit -> unit) -> unit
val set_type_used_callback: string -> type_declaration -> ((unit -> unit) -> unit) -> unit

(* Forward declaration to break mutual recursion with Includemod. *)
val check_modtype_inclusion:
      (t -> module_type -> Path.t -> module_type -> unit) ref
(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
