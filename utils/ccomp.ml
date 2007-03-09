(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

let run_command cmdline = ignore(command cmdline)

(* Build @responsefile to work around Windows limitations on 
   command-line length *)
let build_diversion lst =
  let (responsefile, oc) = Filename.open_temp_file "camlresp" "" in
  List.iter
    (fun f ->
      if f <> "" then begin
        output_string oc (Filename.quote f); output_char oc '\n'
      end)
    lst;
  close_out oc;
  at_exit (fun () -> Misc.remove_file responsefile);
  "@" ^ responsefile

let quote_files lst =
  let s =
    String.concat " "
      (List.map (fun f -> if f = "" then f else Filename.quote f) lst) in
  if Sys.os_type = "Win32" && String.length s >= 256
  then build_diversion lst
  else s

let compile_file name =
     command
       (Printf.sprintf
         "%s -c %s %s %s %s"
         !Clflags.c_compiler
         (String.concat " " (List.rev !Clflags.ccopts))
         (quote_files
             (List.rev_map (fun dir -> "-I" ^ dir) !Clflags.include_dirs))
         (Clflags.std_include_flag "-I")
         (Filename.quote name))

let create_archive archive file_list =
  Misc.remove_file archive;
  let quoted_archive = Filename.quote archive in
  match Config.ccomp_type with
    "msvc" ->
      command(Printf.sprintf "link /lib /nologo /out:%s %s"
                             quoted_archive (quote_files file_list))
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                quoted_archive (quote_files file_list)) in
      if r1 <> 0 || String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ quoted_archive)

let expand_libname name =
  if String.length name < 2 || String.sub name 0 2 <> "-l"
  then name
  else begin
    let libname =
      "lib" ^ String.sub name 2 (String.length name - 2) ^ Config.ext_lib in
    try
      Misc.find_in_path !Config.load_path libname
    with Not_found ->
      libname
  end

(* Handling of msvc's /link options *)

let make_link_options optlist =
  let rec split linkopts otheropts = function
  | [] -> String.concat " " otheropts
	  ^ " /link /subsystem:console "
          ^ String.concat " " linkopts
  | opt :: rem ->
      if String.length opt >= 5 && String.sub opt 0 5 = "/link"
      then split (String.sub opt 5 (String.length opt - 5) :: linkopts)
                 otheropts rem
      else split linkopts (opt :: otheropts) rem
  in split [] [] optlist

(* Handling of Visual C++ 2005 manifest files *)

let merge_manifest exefile =
  let manfile = exefile ^ ".manifest" in
  if not (Sys.file_exists manfile) then 0 else begin
    let retcode =
      command (Printf.sprintf "mt -nologo -outputresource:%s -manifest:%s"
                              (Filename.quote exefile)
                              (Filename.quote manfile)) in
    Misc.remove_file manfile;
    retcode
  end



(* Extract the list of external symbols in a object/library file.
   First result is the list of defined symbols.
   Second result is the list of undefined symbols. *)

module CoffSymbolsReader = struct
  module StringSet = Set.Make(String)

  exception Error

  let read ic len =
    let buf = String.create len in
    really_input ic buf 0 len;
    buf

  let int32 buf loc =
    Char.code buf.[loc] 
    + (Char.code buf.[loc + 1]) lsl 8
    + (Char.code buf.[loc + 2]) lsl 16
    + (Char.code buf.[loc + 3]) lsl 24 

  let int16 buf loc =
    Char.code buf.[loc] 
    + (Char.code buf.[loc + 1]) lsl 8

  let int8 buf loc = Char.code buf.[loc] 

  let strz buf loc c =
    let i = 
      try String.index_from buf loc c
      with Not_found -> String.length buf in
    String.sub buf loc (i - loc)
    
  let read_obj ic defs undefs =
    let base = pos_in ic in
    let buf = read ic 20 in
    (match int16 buf 0 with
       | 0x14c -> ()  (* i386 or later *)
       | _ -> raise Error);
    let symtable = int32 buf 8 in
    let symcount = int32 buf 12 in

    seek_in ic (base + symtable + 18 * symcount);
    let buf = read ic 4 in
    let strtbl_len = int32 buf 0 in
    let strtbl = read ic (strtbl_len - 4) in
    
    let name buf = 
      if int32 buf 0 <> 0 then strz (String.sub buf 0 8) 0 '\000'
      else strz strtbl (int32 buf 4 - 4) '\000' in

    let rec read_symbols strtbl rest =
      if rest = 0 then ()
      else let buf = read ic 18 in
      let sect = int8 buf 12 and cl = int8 buf 16 and aux = int8 buf 17
						  and v = int32 buf 8 in
      if cl = 2 then
	if sect <> 0 || v <> 0 then defs := StringSet.add (name buf) !defs
	else undefs := StringSet.add (name buf) !undefs
      else ();
      seek_in ic (pos_in ic + 18 * aux);
      read_symbols strtbl (rest - aux - 1) in
    
    seek_in ic (base + symtable);
    read_symbols strtbl symcount
      
  let magic_lib = "!<arch>\n"

  let read_lib ic defs undefs =
    let rec read_member () =
      let buf = read ic 60 in
      let base = pos_in ic in
      let size = int_of_string (strz (String.sub buf 48 10) 0 ' ') in
      let name = strz (String.sub buf 0 16) 0 ' '  in
      begin match name with
	| "/" | "" | "//" -> ()
	| _ -> read_obj ic defs undefs
      end;
      seek_in ic (base + size + size mod 2); 
      read_member ()
    in
    (try read_member () with End_of_file -> ())

  let read defs undefs filename =
    if filename = "" then ()
    else let filename = expand_libname filename in
    try
      let ic = open_in_bin filename in
      try
	if !Clflags.verbose
	then Printf.printf "(read symbols from %s)\n" filename;
	if in_channel_length ic > String.length magic_lib
	  && read ic (String.length magic_lib) = magic_lib 
	then read_lib ic defs undefs
	else (seek_in ic 0; read_obj ic defs undefs);
	close_in ic
      with exn ->
	close_in ic;
	raise Error
    with Sys_error _ ->
      if !Clflags.verbose
      then Printf.printf "(ignore %s)\n" filename

  let read_files files =
    match Config.system with
      | "mingw" | "win32" | "cygwin" ->
	  let defs = ref StringSet.empty and undefs = ref StringSet.empty in
	  List.iter (read defs undefs) files;
	  StringSet.elements !defs,
	  StringSet.elements (StringSet.diff !undefs !defs)
      | _ ->
	  [],[]
    
end

let coff_symbols = CoffSymbolsReader.read_files
