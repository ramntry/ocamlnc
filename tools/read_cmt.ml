(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let gen_annot = ref false
let gen_ml = ref false

let arg_list = [
  "-annot", Arg.Set gen_annot, " : generate the corresponding .annot file";
  "-src", Arg.Set gen_ml, " : generate the original source file (without comments)";
  ]

let arg_usage = "read_cmt [OPTIONS] FILE.cmt : read FILE.cmt and print related information"

let _ =
  Clflags.annotations := true;

  Arg.parse arg_list  (fun filename ->
    if
      Filename.check_suffix filename ".cmt" ||
      Filename.check_suffix filename ".cmti"
    then begin
(*      init_path(); *)
      let cmt = Cmt_format.read_cmt filename in
      if !gen_annot then Cmt2annot.gen_annot filename cmt;
      if !gen_ml then Cmt2annot.gen_ml filename cmt;
      if not !gen_annot && not !gen_ml then begin
        Printf.fprintf stderr "Error: You must at least specify -annot or -src\n%!";
        Arg.usage arg_list arg_usage;
        exit 2
      end;
    end else begin
      Printf.fprintf stderr "Error: the file must have an extension in .cmt or .cmti.\n%!";
      Arg.usage arg_list arg_usage
    end
  ) arg_usage

