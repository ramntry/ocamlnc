module Current_cmi_format = Cmi_format

exception TODO

module Ident : sig 
  
  val reset : unit -> unit
  val ident : V3120_types.Ident.t -> Ident.t

end = struct

  module T = V3120_types.Ident
  let tbl = Hashtbl.create 13
    
  let reset () = Hashtbl.clear tbl

  let ident id = 
    let key =  (id.T.name, id.T.stamp) in
    try
      Hashtbl.find tbl key
  with Not_found ->
    let t = Ident.magic id.T.stamp id.T.name id.T.flags in
      Hashtbl.add tbl key t;
      t

end

module Primitive : sig

  val description : V3120_types.Primitive.description -> Primitive.description

end = struct

  let description prim = raise TODO

end

module Types : sig

  val reset : unit -> unit

  val signature_item : 
    V3120_types.Types.signature_item -> Types.signature_item 

end = struct

  module T = V3120_types.Types
  open Types

  let reset () = raise TODO

  let rec type_expr ty = raise TODO

  let ident = Ident.ident



  let rec signature list = List.map signature_item list

  and signature_item item =
    match item with
	T.Tsig_value (id, v) ->
	  Tsig_value (ident id, value_description v)
      | T.Tsig_type (id, t, r) ->
	  Tsig_type (ident id, type_declaration t, rec_status r)
      | T.Tsig_exception (id, decl) ->
	  Tsig_exception (ident id, exception_declaration decl)
      | T.Tsig_module (id, m, r) ->
	  Tsig_module (ident id,  module_type m, rec_status r)
      | T.Tsig_modtype (id, m) ->
	  Tsig_modtype (ident id, modtype_declaration m)
      | T.Tsig_class (id, cl, r) ->
	  Tsig_class (ident id, class_declaration cl, rec_status r)
      | T.Tsig_cltype (id, cl, r) ->
	  Tsig_cltype (ident id, cltype_declaration cl, rec_status r)

  and value_description v =
    { val_type = type_expr v.T.val_type;
      val_kind = value_kind v.T.val_kind; }

  and value_kind v =
    match v with
	T.Val_reg -> Val_reg
      | T.Val_prim prim -> Val_prim (Primitive.description prim)
      | T.Val_ivar (m,s) ->
	  Val_ivar (mutable_flag m, s)
      | T.Val_self (meths, vars, s, t) ->
	  (*
	    of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * Asttypes.mutable_flag *
                 Asttypes.virtual_flag * type_expr) Vars.t ref *
                string * type_expr
	  *)
	  raise TODO
                                        (* Self *)
      | T.Val_anc _ -> raise TODO
(* of (string * Ident.t) list * string *)
      | T.Val_unbound -> Val_unbound

  and type_declaration decl = raise TODO
  and rec_status r = raise TODO
  and exception_declaration decl = raise TODO
  and module_type decl = raise TODO
  and modtype_declaration decl = raise TODO
  and class_declaration decl = raise TODO
  and cltype_declaration decl = raise TODO
  and mutable_flag v = raise TODO


end
;;

module Cmi_format : sig

  val pers_flags : 
    V3120_types.Cmi_format.pers_flags -> Cmi_format.pers_flags
  ;;

  end = struct
  
  module T = V3120_types.Cmi_format
  open Cmi_format

  let pers_flags flag =
    match flag with
	T.Rectypes -> Rectypes
end

let input_cmi_file ic magic =
    if magic <> V3120_types.cmi_magic_number then
      raise Current_cmi_format.No_such_magic;

  Ident.reset ();
  Types.reset ();

    let (cmi_name, cmi_sign) = (input_value ic : string *  V3120_types.Types.signature_item list) in
    let cmi_crcs = (input_value ic : (string * Digest.t) list) in
    let cmi_flags = (input_value ic : V3120_types.Cmi_format.pers_flags list) in

    let cmi_sign = List.map Types.signature_item cmi_sign in
    let cmi_flags = List.map Cmi_format.pers_flags cmi_flags in
      { Current_cmi_format.cmi_name ; cmi_sign; cmi_crcs; cmi_flags }
