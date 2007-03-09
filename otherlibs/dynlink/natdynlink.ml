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

(* Dynamic loading of .cmx files *)

external ndl_open: bool -> string -> string list -> 
  string = "caml_natdynlink_open"
external ndl_getmap : unit -> string = "caml_natdynlink_getmap"
external ndl_globals_inited : unit -> int = "caml_natdynlink_globals_inited"

(** {6 Error reporting} *)

type linking_error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | File_not_found of string
  | Cannot_open_dll of string

(*
    Not_a_cmx_file of string
  | Corrupted_unit_info of string
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Unavailable_unit of string
  | Not_yet_available_unit of string
  | Exception of exn
  | Reloading_symbol of string * string * string

  | Unsafe_file
  | Linking_error of string * string
  | File_not_found of string
  | Cannot_open_dll of string
*)


exception Error of error

(* Copied from other places to avoid dependencies *)

let cmx_magic_number = "Caml1999Y011"
and cmxa_magic_number = "Caml1999Z010"

type value_approximation

type subunit = bool * string

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: subunit list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_send_fun: int list;              (* Send functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;  (* List of unit infos w/ CRCs *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)

type kind = Unit of unit_infos * Digest.t | Library of library_infos

let global_infos_table =
  (Hashtbl.create 17 : (string, (unit_infos * Digest.t) option) Hashtbl.t)

let read_file filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer = cmx_magic_number then begin
      let ui = (input_value ic : unit_infos) in
      let crc = Digest.input ic in
      close_in ic;
      Unit (ui, crc)
    end else if buffer = cmxa_magic_number then begin
      let infos = (input_value ic : library_infos) in
      close_in ic;
      Library infos
    end else begin
      close_in ic;
      raise(Error(Not_a_bytecode_file filename))
    end;
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Not_a_bytecode_file filename))

let cmx_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"


(* Management of interface and implementation CRCs *)

module StrMap = Map.Make(String)

type implem_state =
  | Loaded
  | Check_inited of int

type state = {
  ifaces: (string*string) StrMap.t;
  implems: (string*string*implem_state) StrMap.t;
(*  loaded_symbols: string StrMap.t; *)
}

let allow_extension = ref true

let add_check_ifaces allow_ext filename ui ifaces =
  List.fold_left
    (fun ifaces (name, crc) ->
       if name = ui.ui_name 
       then StrMap.add name (crc,filename) ifaces
       else 
	 try
	   let (old_crc,old_src) = StrMap.find name ifaces in
	   if old_crc <> crc 
	   then raise(Error(Inconsistent_import(name)))
	   else ifaces
	 with Not_found ->
	   if allow_ext then StrMap.add name (crc,filename) ifaces
	   else raise (Error(Unavailable_unit name))
    ) ifaces ui.ui_imports_cmi

let check_implems filename ui implems =
  List.iter
    (fun (name, crc) ->
       match name with
	 |"Out_of_memory"
	 |"Sys_error"
	 |"Failure"
	 |"Invalid_argument"
	 |"End_of_file"
	 |"Division_by_zero"
	 |"Not_found"
	 |"Match_failure"
	 |"Stack_overflow"
	 |"Sys_blocked_io"
	 |"Assert_failure"
	 |"Undefined_recursive_module" -> ()
	 | _ ->
       try
	 let (old_crc,old_src,state) = StrMap.find name implems in
	 if crc <> cmx_not_found_crc && old_crc <> crc 
	 then 
	   raise(Error(Inconsistent_import(name)))
	 else match state with
	   | Check_inited i -> 
	       if ndl_globals_inited() < i 
	       then raise(Error(Unavailable_unit name))
	   | Loaded -> ()
       with Not_found ->
	 raise (Error(Unavailable_unit name))
    ) ui.ui_imports_cmx

(* Prevent redefinition of a unit symbol *)

(* TODO: make loaded_symbols a global variable, otherwise could
   break safety with load_private (or not?) *)
(*
let check_symbols filename ui symbols =
  List.fold_left
    (fun syms name ->
       try
	 let old_src = StrMap.find name symbols in
	 raise (Error(Reloading_symbol(name,old_src,filename)))
       with Not_found -> 
	 StrMap.add name filename syms
    )
    symbols
    ui.ui_defines
*)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let dll_filename filename = 
  let s = chop_extension_if_any filename ^ ".so" in
  if Filename.is_implicit s then Filename.concat (Sys.getcwd ()) s
  else s

let empty_state = {
  ifaces = StrMap.empty;
  implems = StrMap.empty;
(*  loaded_symbols = StrMap.empty; *)
}

let global_state = ref empty_state

let loadunits priv filename units state =
(*  let new_symbols = 
    List.fold_left (fun accu (ui,_) -> check_symbols filename ui accu)
      state.loaded_symbols units in *)
  let new_ifaces = 
    List.fold_left 
      (fun accu (ui,_) -> add_check_ifaces !allow_extension filename ui accu)
      state.ifaces units in
  let new_implems =
    List.fold_left
      (fun accu (ui,crc) -> 
	 check_implems filename ui accu;
	 StrMap.add ui.ui_name (crc,filename,Loaded) accu)
      state.implems units in

  let dll = dll_filename filename in
  if not (Sys.file_exists dll) then raise (Error (File_not_found dll));
  
  let defines = 
    List.map snd (
      List.flatten (List.map (fun (ui,_) -> ui.ui_defines) units)) in
  let s = ndl_open priv dll ("_shared_startup"::defines) in
  if Obj.repr s <> Obj.repr () then raise (Error (Cannot_open_dll s));
  
  { implems = new_implems; ifaces = new_ifaces; 
    (*loaded_symbols = new_symbols*) }

let load priv filename state = match read_file filename with
  | Unit (ui,crc) -> loadunits priv filename [(ui,crc)] state
  | Library infos -> loadunits priv filename infos.lib_units state

let loadfile filename = global_state := load false filename !global_state
let loadfile_private filename = ignore (load true filename !global_state)
      
let add_builtin_map st =
  let map : (string*Digest.t*Digest.t*string list) list = 
    Marshal.from_string (ndl_getmap ()) 0 in
  let exe = Sys.executable_name in
  let rank = ref 0 in
  List.fold_left
    (fun st (name,crc_intf,crc_impl,syms) -> 
       rank := !rank + List.length syms; {
	 ifaces = StrMap.add name (crc_intf,exe) st.ifaces;
	 implems =StrMap.add name (crc_impl,exe,Check_inited !rank) st.implems;
(*
	 loaded_symbols =
	   List.fold_left (fun l s -> StrMap.add s exe l) 
	     st.loaded_symbols syms
*)
       }
    )
    st
    map


(* is it ok to restrict only the accessible interfaces? *)
let allow_only names =
  let old = !global_state.ifaces in
  let ifaces = 
    List.fold_left
      (fun ifaces name ->
	 try StrMap.add name (StrMap.find name old) ifaces
	 with Not_found -> ifaces)
      StrMap.empty names in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let prohibit names =
  let ifaces = List.fold_right StrMap.remove names !global_state.ifaces in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let default_available_units () =
  global_state := add_builtin_map empty_state;
  allow_extension := true

let init () =
  default_available_units ()

let digest_interface _ _ = 
  failwith "Dynlink.digest_interface: not implemented in native code"
let add_interfaces _ _ = 
  failwith "Dynlink.add_interfaces: not implemented in native code"
let add_available_units _ =
  failwith "Dynlink.add_available_units: not implemented in native code"
let clear_available_units _ =
  failwith "Dynlink.clear_available_units: not implemented in native code"
let allow_unsafe_modules _ =
  ()
(*  failwith "Dynlink.allow_unsafe_modules: not implemented in native code" *)


(* Error report *)

(* Error report *)

let error_message = function
    Not_a_bytecode_file name ->
      name ^ " is not an object file"
  | Inconsistent_import name ->
      "interface or implementation mismatch on " ^ name
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
      "corrupted interface file " ^ name
  | File_not_found name ->
      "cannot find file " ^ name ^ " in search path"
  | Cannot_open_dll reason ->
      "error loading shared library: " ^ reason

let is_native = true
