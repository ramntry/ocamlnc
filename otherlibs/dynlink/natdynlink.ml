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

type error =
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


exception Error of error

(* Copied from other places to avoid dependencies *)

let cmx_magic_number = "Caml1999Y011"

type value_approximation

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_send_fun: int list;              (* Send functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)

let global_infos_table =
  (Hashtbl.create 17 : (string, (unit_infos * Digest.t) option) Hashtbl.t)

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_cmx_file filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info filename))

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
  loaded_symbols: string StrMap.t;
}

let add_check_ifaces allow_ext filename ui ifaces =
  List.fold_left
    (fun ifaces (name, crc) ->
       if name = ui.ui_name 
       then StrMap.add name (crc,filename) ifaces
       else 
	 try
	   let (old_crc,old_src) = StrMap.find name ifaces in
	   if old_crc <> crc 
	   then raise(Error(Inconsistent_interface(name, old_src, filename)))
	   else ifaces
	 with Not_found ->
	   if allow_ext then StrMap.add name (crc,filename) ifaces
	   else raise (Error(Unavailable_unit name))
    ) ifaces ui.ui_imports_cmi

let check_implems filename ui implems =
  List.iter
    (fun (name, crc) ->
       try
	 let (old_crc,old_src,state) = StrMap.find name implems in
	 if crc <> cmx_not_found_crc && old_crc <> crc 
	 then 
	   raise(Error(Inconsistent_implementation(name, old_src, filename)))
	 else match state with
	   | Check_inited i -> 
	       if ndl_globals_inited() < i 
	       then raise(Error(Not_yet_available_unit name))
	   | Loaded -> ()
       with Not_found ->
	 raise (Error(Unavailable_unit name))
    ) ui.ui_imports_cmx

(* Prevent redefinition of a unit symbol *)

(* TODO: make loaded_symbols a global variable, otherwise could
   break safety with load_private (or not?) *)

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


let dll_filename filename = 
  let s = Filename.chop_suffix filename ".cmx" ^ ".so" in
  if Filename.is_implicit s then Filename.concat (Sys.getcwd ()) s
  else s

let global_state = ref {
  ifaces = StrMap.empty;
  implems = StrMap.empty;
  loaded_symbols = StrMap.empty;
}

let loadcmx priv filename state =
  let (ui, crc) = read_unit_info filename in

  let new_symbols = check_symbols filename ui state.loaded_symbols in
  let new_ifaces = add_check_ifaces false filename ui state.ifaces in
  check_implems filename ui state.implems;

  let dll = dll_filename filename in
  if not (Sys.file_exists dll) 
  then raise (Error (File_not_found dll));
  
  let s = ndl_open priv dll ui.ui_defines in
  if Obj.repr s <> Obj.repr () 
  then raise (Error (Linking_error (filename,s)));
  
  { 
    implems = StrMap.add ui.ui_name (crc,filename,Loaded) state.implems;
    ifaces = new_ifaces;
    loaded_symbols = new_symbols;
  }


let loadfile filename = global_state := loadcmx false filename !global_state
let loadfile_private filename = ignore (loadcmx true filename !global_state)
      
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
	 loaded_symbols =
	   List.fold_left (fun l s -> StrMap.add s exe l) 
	     st.loaded_symbols syms
       }
    )
    st
    map
    

let init () =
  global_state := add_builtin_map !global_state

(* Error report *)

let error_message = function
    Not_a_cmx_file name | Corrupted_unit_info name ->
      name ^ " is not a valid cmx file"
  | Inconsistent_interface (name,source1,source2) ->
      "interface mismatch on " ^ name ^ " between " ^ source1
      ^ " and " ^ source2
  | Inconsistent_implementation (name,source1,source2) ->
      "implementation mismatch on " ^ name ^ " between " ^ source1
      ^ " and " ^ source2
  | Reloading_symbol (name,source1,source2) ->
      "module " ^ name ^ " defined in both " ^ source1
      ^ " and " ^ source2
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Not_yet_available_unit name ->
      "no implementation available for " ^ name ^ " yet"
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, msg) ->
      "error while linking " ^ name ^ ".\n" ^ msg
  | File_not_found name ->
      "cannot find file " ^ name ^ " in search path"
  | Cannot_open_dll reason ->
      "error loading shared library: " ^ reason
  | Exception exn ->
      "exception raised: " ^ Printexc.to_string exn
