exception TODO

module AST = struct


module Asttypes = struct

    open Asttypes
    module T = V3120_types.Asttypes


    let constant c =
      match c with
        T.Const_int int ->  Const_int int
      | T.Const_char char -> Const_char char
      | T.Const_string  string -> Const_string string
      | T.Const_float string -> Const_float string
      | T.Const_int32 int32 -> Const_int32 int32
      | T.Const_int64 int64 -> Const_int64 int64
      | T.Const_nativeint nativeint -> Const_nativeint nativeint


    let virtual_flag vf =
      match vf with
        T.Virtual -> Virtual
      | T.Concrete -> Concrete

    let private_flag pf =
      match pf with
        T.Private -> Private
      | T.Public -> Public

    let rec_flag r =
      match r with
        T.Nonrecursive -> Nonrecursive
      | T.Recursive -> Recursive
      | T.Default -> Default

    let mutable_flag mf =
      match mf with
        T.Immutable -> Immutable
      | T.Mutable -> Mutable

    let direction_flag d =
      match d with
        T.Upto -> Upto
      | T.Downto -> Downto

    let label s = s

end

module Lexing = struct

    open Lexing
    module T = V3120_types.Lexing

    let position p =
      { pos_fname = p.T.pos_fname;
        pos_lnum = p.T.pos_lnum;
        pos_bol = p.T.pos_bol;
        pos_cnum = p.T.pos_cnum;
      }

  end

module Location = struct

    open Location
    module T = V3120_types.Location

    let t loc =
      { loc_start = Lexing.position loc.T.loc_start;
        loc_end = Lexing.position loc.T.loc_end;
        loc_ghost = loc.T.loc_ghost;
      }

  end

module Longident = struct

    open Longident
    module T = V3120_types.Longident

  let rec t l =
    match l with
	T.Lident s -> Lident s
      | T.Ldot (ll, s) -> Ldot (t ll, s)
      | T.Lapply (l1, l2) -> Lapply (t l1, t l2)

end


module Parsetree : sig
  val signature :
    V3120_types.Parsetree.signature -> Parsetree.signature

end = struct

  open Asttypes

  open Parsetree
  module T = V3120_types.Parsetree

  let signature s = raise TODO

  let rec core_type c =
    { ptyp_desc = core_type_desc c.T.ptyp_desc;
      ptyp_loc = Location.t c.T.ptyp_loc }

  and core_type_desc c =
    match c with
    T.Ptyp_any -> Ptyp_any
  | T.Ptyp_var string -> Ptyp_var string
  | T.Ptyp_arrow (l, c1, c2) ->
      Ptyp_arrow (label l, core_type c1, core_type c2)
  | T.Ptyp_tuple list -> Ptyp_tuple (List.map core_type list)
  | T.Ptyp_constr (l, list) ->
      Ptyp_constr (Longident.t l, List.map core_type list)
  | T.Ptyp_object list -> Ptyp_object (List.map core_field_type list)
  | T.Ptyp_class (l, clist, llist) ->
      Ptyp_class (Longident.t l, List.map core_type clist, List.map label llist)
  | T.Ptyp_alias (c1, string) -> Ptyp_alias (core_type c1, string)
  | T.Ptyp_variant (list, bool, option) ->
      Ptyp_variant (List.map row_field list, bool,
		    match option with
			None -> None
		      | Some list -> Some (List.map label list))
  | T.Ptyp_poly (list, c1) -> Ptyp_poly (list, core_type c1)
  | T.Ptyp_package p -> Ptyp_package (package_type p)

and package_type (l, list) =
    (Longident.t l,List.map (fun (s,c) -> (s, core_type c)) list)

and core_field_type c =
  { pfield_desc = core_field_desc c.T.pfield_desc;
    pfield_loc = Location.t c.T.pfield_loc;
  }

and core_field_desc c =
    match c with
	T.Pfield (s, c1) -> Pfield (s, core_type c1)
      | T.Pfield_var -> Pfield_var

and row_field r =
    match r with
    T.Rtag (l, bool, list) ->
      Rtag (label l, bool, List.map core_type list)
  | T.Rinherit c -> Rinherit (core_type c)

(*
(* Type expressions for the class language *)

type 'a class_infos =
  { pci_virt: virtual_flag;
    pci_params: string list * Location.t;
    pci_name: string;
    pci_expr: 'a;
    pci_variance: (bool * bool) list;
    pci_loc: Location.t }

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | T.Ppat_var of string
  | T.Ppat_alias of pattern * string
  | T.Ppat_constant of constant
  | T.Ppat_tuple of pattern list
  | T.Ppat_construct of Longident.t * pattern option * bool
  | T.Ppat_variant of label * pattern option
  | T.Ppat_record of (Longident.t * pattern) list * closed_flag
  | T.Ppat_array of pattern list
  | T.Ppat_or of pattern * pattern
  | T.Ppat_constraint of pattern * core_type
  | T.Ppat_type of Longident.t
  | T.Ppat_lazy of pattern

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t
  | T.Pexp_constant of constant
  | T.Pexp_let of rec_flag * (pattern * expression) list * expression
  | T.Pexp_function of label * expression option * (pattern * expression) list
  | T.Pexp_apply of expression * (label * expression) list
  | T.Pexp_match of expression * (pattern * expression) list
  | T.Pexp_try of expression * (pattern * expression) list
  | T.Pexp_tuple of expression list
  | T.Pexp_construct of Longident.t * expression option * bool
  | T.Pexp_variant of label * expression option
  | T.Pexp_record of (Longident.t * expression) list * expression option
  | T.Pexp_field of expression * Longident.t
  | T.Pexp_setfield of expression * Longident.t * expression
  | T.Pexp_array of expression list
  | T.Pexp_ifthenelse of expression * expression * expression option
  | T.Pexp_sequence of expression * expression
  | T.Pexp_while of expression * expression
  | T.Pexp_for of string * expression * expression * direction_flag * expression
  | T.Pexp_constraint of expression * core_type option * core_type option
  | T.Pexp_when of expression * expression
  | T.Pexp_send of expression * string
  | T.Pexp_new of Longident.t
  | T.Pexp_setinstvar of string * expression
  | T.Pexp_override of (string * expression) list
  | T.Pexp_letmodule of string * module_expr * expression
  | T.Pexp_assert of expression
  | T.Pexp_assertfalse
  | T.Pexp_lazy of expression
  | T.Pexp_poly of expression * core_type option
  | T.Pexp_object of class_structure
  | T.Pexp_newtype of string * expression
  | T.Pexp_pack of module_expr * package_type
  | T.Pexp_open of Longident.t * expression

(* Value descriptions *)

and value_description =
  { pval_type: core_type;
    pval_prim: string list }

(* Type declarations *)

and type_declaration =
  { ptype_params: string list;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_kind: type_kind;
    ptype_private: private_flag;
    ptype_manifest: core_type option;
    ptype_variance: (bool * bool) list;
    ptype_loc: Location.t }

and type_kind =
    Ptype_abstract
  | T.Ptype_variant of (string * core_type list * Location.t) list
  | T.Ptype_record of
      (string * mutable_flag * core_type * Location.t) list

and exception_declaration = core_type list

(* Type expressions for the class language *)

and class_type =
  { pcty_desc: class_type_desc;
    pcty_loc: Location.t }

and class_type_desc =
    Pcty_constr of Longident.t * core_type list
  | T.Pcty_signature of class_signature
  | T.Pcty_fun of label * core_type * class_type

and class_signature = core_type * class_type_field list

and class_type_field =
    Pctf_inher of class_type
  | T.Pctf_val of (string * mutable_flag * virtual_flag * core_type * Location.t)
  | T.Pctf_virt  of (string * private_flag * core_type * Location.t)
  | T.Pctf_meth  of (string * private_flag * core_type * Location.t)
  | T.Pctf_cstr  of (core_type * core_type * Location.t)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
  { pcl_desc: class_expr_desc;
    pcl_loc: Location.t }

and class_expr_desc =
    Pcl_constr of Longident.t * core_type list
  | T.Pcl_structure of class_structure
  | T.Pcl_fun of label * expression option * pattern * class_expr
  | T.Pcl_apply of class_expr * (label * expression) list
  | T.Pcl_let of rec_flag * (pattern * expression) list * class_expr
  | T.Pcl_constraint of class_expr * class_type

and class_structure = pattern * class_field list

and class_field =
    Pcf_inher of override_flag * class_expr * string option
  | T.Pcf_valvirt of (string * mutable_flag * core_type * Location.t)
  | T.Pcf_val of (string * mutable_flag * override_flag * expression * Location.t)
  | T.Pcf_virt  of (string * private_flag * core_type * Location.t)
  | T.Pcf_meth of (string * private_flag *override_flag * expression * Location.t)
  | T.Pcf_cstr  of (core_type * core_type * Location.t)
  | T.Pcf_let   of rec_flag * (pattern * expression) list * Location.t
  | T.Pcf_init  of expression

and class_declaration = class_expr class_infos

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t
  | T.Pmty_signature of signature
  | T.Pmty_functor of string * module_type * module_type
  | T.Pmty_with of module_type * (Longident.t * with_constraint) list
  | T.Pmty_typeof of module_expr

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * value_description
  | T.Psig_type of (string * type_declaration) list
  | T.Psig_exception of string * exception_declaration
  | T.Psig_module of string * module_type
  | T.Psig_recmodule of (string * module_type) list
  | T.Psig_modtype of string * modtype_declaration
  | T.Psig_open of Longident.t
  | T.Psig_include of module_type
  | T.Psig_class of class_description list
  | T.Psig_class_type of class_type_declaration list

and modtype_declaration =
    Pmodtype_abstract
  | T.Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | T.Pwith_module of Longident.t
  | T.Pwith_typesubst of type_declaration
  | T.Pwith_modsubst of Longident.t

(* value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t
  | T.Pmod_structure of structure
  | T.Pmod_functor of string * module_type * module_expr
  | T.Pmod_apply of module_expr * module_expr
  | T.Pmod_constraint of module_expr * module_type
  | T.Pmod_unpack of expression * package_type

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | T.Pstr_value of rec_flag * (pattern * expression) list
  | T.Pstr_primitive of string * value_description
  | T.Pstr_type of (string * type_declaration) list
  | T.Pstr_exception of string * exception_declaration
  | T.Pstr_exn_rebind of string * Longident.t
  | T.Pstr_module of string * module_expr
  | T.Pstr_recmodule of (string * module_type * module_expr) list
  | T.Pstr_modtype of string * module_type
  | T.Pstr_open of Longident.t
  | T.Pstr_class of class_declaration list
  | T.Pstr_class_type of class_type_declaration list
  | T.Pstr_include of module_expr

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | T.Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | T.Pdir_string of string
  | T.Pdir_int of int
  | T.Pdir_ident of Longident.t
  | T.Pdir_bool of bool
*)

end

end

let input_intf_file ic magic =
  if magic <> V3120_types.ast_intf_magic_number then
    V3112_ast.input_intf_file ic magic
  else begin
    let v = (input_value ic : V3120_types.Parsetree.signature) in
      AST.Parsetree.signature v
    end
