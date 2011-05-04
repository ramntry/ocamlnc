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

let rewrite_ast magic ast ppx =
  (* TODO: be more clever, and do not read/write intermediate files
     when several ppx processors are chained. *)
  let fn_in = Filename.temp_file "camlppx_in" "" in
  let fn_out = Filename.temp_file "camlppx_in" "" in
  let oc = open_out_bin fn_in in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc;

  let comm = Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out) in
  Misc.remove_file fn_in;
  if Ccomp.command comm <> 0 then begin
    Misc.remove_file fn_out;
    raise Error;
  end;
  let ic = open_in_bin fn_out in
  begin try
    let buffer = String.create (String.length magic) in
    really_input ic buffer 0 (String.length magic);
    if buffer <> magic then
      Misc.fatal_error "Ocaml and preprocessor have incompatible versions";
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn_out;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn_out;
    raise exn
  end

let file ppf inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  List.fold_left (rewrite_ast ast_magic) ast !Clflags.ppx
