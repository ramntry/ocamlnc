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

type error =
    Not_a_cmx_file of string
  | Corrupted_unit_info of string
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Unavailable_unit of string
  | Cyclic_loading of string
  | Exception of exn

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

let ifaces = ref StrMap.empty
let implems = ref StrMap.empty

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

let reraise = function
  | None -> ()
  | Some exn -> raise (Error (Exception exn))

let check_implems filename ui implems =
  List.iter
    (fun (name, crc) ->
       try
	 let (old_crc,old_src,old_exn) = StrMap.find name implems in
	 if crc <> cmx_not_found_crc && old_crc <> crc 
	 then raise(Error(Inconsistent_implementation(name, old_src, filename)))
	 else reraise old_exn
       with Not_found ->
	 raise (Error(Unavailable_unit name))
    ) ui.ui_imports_cmx

external ndl_open: bool -> string -> string -> string = "caml_natdynlink_open"
external ndl_getmap : unit -> string = "caml_natdynlink_getmap"

let loaded = Hashtbl.create 8

let loadfile filename =
  let (ui, crc) = read_unit_info filename in
  try 
    let (old_crc,old_src,old_exn) = StrMap.find ui.ui_name !implems in
    if old_crc <> crc 
    then raise(Error(Inconsistent_implementation(ui.ui_name, old_src, filename)))
    else reraise old_exn
  with Not_found ->
    check_implems filename ui !implems;
    let new_ifaces = add_check_ifaces true filename ui !ifaces in
    let dll_filename = 
      let s = Filename.chop_suffix filename ".cmx" ^ ".so" in
      if Filename.is_implicit s then Filename.concat (Sys.getcwd ()) s
      else s in
    if not (Sys.file_exists dll_filename) then
      raise (Error (File_not_found dll_filename));

    let old_ifaces = !ifaces in
    try 
      ifaces := new_ifaces;
      implems := StrMap.add ui.ui_name 
	(crc,filename,Some(Error(Cyclic_loading ui.ui_name)))
	!implems;

      let s = ndl_open false dll_filename ui.ui_symbol in
      if Obj.repr s <> Obj.repr () then 
	raise (Error (Linking_error (filename,s)));
      
      implems := StrMap.add ui.ui_name (crc,filename,None) !implems;
    with exn -> 
      ifaces := old_ifaces;
      implems := StrMap.add ui.ui_name (crc,filename,Some exn) !implems;
      raise exn
  

let init () =
  let map : (string*Digest.t*Digest.t) list = 
    Marshal.from_string (ndl_getmap ()) 0 in
  let exe = Sys.executable_name in
  List.iter
    (fun (name,crc_intf,crc_impl) -> 
       ifaces := StrMap.add name (crc_intf,exe) !ifaces;
       implems := StrMap.add name (crc_impl,exe,None) !implems;
       (* TODO: Should check which of these units have been already
	  initialized! *)
    )
    map

(* Error report *)

let error_message = function
    Not_a_cmx_file name | Corrupted_unit_info name ->
      name ^ " is not a valid cmx file"
  | Cyclic_loading name ->
      "cyclic loading of " ^ name
  | Inconsistent_interface (name,source1,source2) ->
      "interface mismatch on " ^ name ^ " between " ^ source1
      ^ " and " ^ source2
  | Inconsistent_implementation (name,source1,source2) ->
      "implementation mismatch on " ^ name ^ " between " ^ source1
      ^ " and " ^ source2
  | Unavailable_unit name ->
      "no implementation available for " ^ name
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
