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
(*
Generate .annot file from a .types files.
*)

open Typedtree


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

let directories = ref []

let arg_list = [
  "-I", Arg.String (fun filename -> directories := filename :: !directories),
  "<dir>  Add <dir> to the list of include directories";
  ]

let arg_usage = " ... : generates some information from .cmt/.cmti files"

let _ =
  Arg.parse arg_list  (fun filename -> Arg.usage arg_list arg_usage) arg_usage

module StringMap = Map.Make(struct type t = string let compare = compare end)

type cmt = {
  cmt_modname : string;
  cmt_filename : string;
  cmt_typedtree : Typedtree.structure;
}

type cmti = {
  cmti_modname : string;
  cmti_filename : string;
  cmti_typedtree : Typedtree.signature;
}

let cmts = ref StringMap.empty
let cmtis = ref StringMap.empty

let first_cmt = ref None

let add_cmt filename =
  let typedtree = load_cmt_or_exit filename in
  Printf.printf "%s loaded\n%!" filename;
  let modname = String.capitalize (Filename.basename (Filename.chop_suffix filename ".cmt")) in
  let cmt = {
    cmt_modname = modname;
    cmt_filename = filename;
    cmt_typedtree = typedtree;
  } in
  first_cmt := Some cmt;
  cmts := StringMap.add modname cmt !cmts

let add_cmti filename =
  let typedtree = load_cmti_or_exit filename in
  Printf.printf "%s loaded\n%!" filename;
  let modname = String.capitalize (Filename.basename (Filename.chop_suffix filename ".cmti")) in
  let cmti = {
    cmti_modname = modname;
    cmti_filename = filename;
    cmti_typedtree = typedtree;
  } in
  cmtis := StringMap.add modname cmti !cmtis

let _ =
    List.iter (fun dirname ->
      let dir = Unix.opendir dirname in
      begin
	try
	  while true do
	    let file = Unix.readdir dir in
	    if Filename.check_suffix file ".cmt" then
	      let filename = Filename.concat dirname file in
	      add_cmt filename
	    else
	      if Filename.check_suffix file ".cmti" then
		let filename = Filename.concat dirname file in
		add_cmti filename
	  done
	with End_of_file -> ()
      end;
      Unix.closedir dir;

    ) !directories

let cmts = !cmts
let cmtis = !cmtis


let current_cmt = ref (match !first_cmt with None -> assert false | Some cmt -> cmt)

let get_path path = ()

module ForIterator = struct
    open Asttypes
    open Types
    open Typedtree

    include DefaultIteratorArgument

    let enter_expression e =
      match e.exp_desc with
	  Texp_ident (path, { val_kind = Val_reg}) ->
	    ()
	| _ -> ()
  end

module Iterator = MakeIterator(ForIterator)

let set_iterator cmt =
  current_cmt := cmt

let _ =
  StringMap.iter (fun _ cmt ->
    set_iterator cmt;
    Iterator.iter_structure cmt.cmt_typedtree
  ) cmts
