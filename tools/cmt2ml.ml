(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet OCamlPro, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2010 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Typedtree

let arg_list = []
let arg_usage = " ...<file>.cmt : generates <file>.cmt.ml from cmt file"

let load_cmt_or_exit filename =
  try
    let ic = open_in filename in
    let (types : saved_type array) = input_value ic in
    close_in ic;
    match types with
        [| Saved_implementation typedtree |] -> typedtree
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
        exit 2
  with e ->
    Printf.fprintf stderr "Error %s while loading %s\n%!" (Printexc.to_string e) filename;
    exit 2

let load_cmti_or_exit filename =
  try
    let ic = open_in filename in
    let (types : saved_type array) = input_value ic in
    close_in ic;
    match types with
        [| Saved_signature typedtree |] -> typedtree
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
        exit 2
  with e ->
    Printf.fprintf stderr "Error %s while loading %s\n%!" (Printexc.to_string e) filename;
    exit 2

let _ =
  Clflags.annotations := true;

  Arg.parse arg_list  (fun filename ->
    if Filename.check_suffix filename ".cmt" then begin
      let ic = open_in filename in
      let (types : saved_type array) = input_value ic in
      close_in ic;
      match types with
          [| Saved_implementation typedtree |] ->
	    let filename = filename ^ ".ml" in
	    let oc = open_out filename in
            let ppf = Format.formatter_of_out_channel oc in
            Pprintast.print_structure ppf (Untypeast.untype_structure typedtree);
            Format.pp_print_flush ppf ();
            close_out oc;
	| _ ->
          Printf.fprintf stderr "File was generated with an error\n%!";
          exit 2
    end else
    if Filename.check_suffix filename ".cmti" then begin
      let ic = open_in filename in
      let (types : saved_type array) = input_value ic in
      close_in ic;
      match types with
          [| Saved_signature typedtree |] ->
	    let filename = filename ^ ".mli" in
	    let oc = open_out filename in
            let ppf = Format.formatter_of_out_channel oc in
            Pprintast.print_signature ppf (Untypeast.untype_signature typedtree);
            Format.pp_print_flush ppf ();
            close_out oc;
	| _ ->
          Printf.fprintf stderr "File was generated with an error\n%!";
          exit 2
    end else
      Arg.usage arg_list arg_usage
  ) arg_usage
