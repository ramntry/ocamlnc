(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

exception Error

(* Optionally preprocess a source file *)

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise Error;
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ ->
      if inputfile <> !Location.input_name then Misc.remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version

let input_intf_file ic magic =
  if magic = Config.ast_intf_magic_number then
    (input_value ic : Parsetree.signature)
  else
    raise Cmi_format.No_such_magic

let input_impl_file ic magic =
  if magic = Config.ast_impl_magic_number then
    (input_value ic : Parsetree.structure)
  else
    raise Cmi_format.No_such_magic

let file ppf inputfile parse_fun input_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
	really_input ic buffer 0 (String.length ast_magic);
	if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
	  Some buffer
	else None
    with
    | _ -> None
  in
  let ast =
    try
      match is_ast_file with
	  Some magic ->
            if !Clflags.fast then
              fprintf ppf "@[Warning: %s@]@."
		"option -unsafe used with a preprocessor returning a syntax tree";
            Location.input_name := input_value ic;
            input_fun ic magic
	| None ->
            seek_in ic 0;
            Location.input_name := inputfile;
            let lexbuf = Lexing.from_channel ic in
              Location.init lexbuf inputfile;
              parse_fun lexbuf
    with
      | Cmi_format.No_such_magic -> (* replace Outdated_version *)
	  close_in ic;
          Misc.fatal_error "Ocaml and preprocessor have incompatible versions"
      | x -> close_in ic; raise x
  in
  close_in ic;
  ast
