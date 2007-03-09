let mkdll files =
  let defs,undefs = Ccomp.coff_symbols files in
  let syms = ref [] in
  List.iter
    (fun s -> 
       if (String.length s > 12 && String.sub s 0 12 = "__imp__caml_") then
	 syms := String.sub s 6 (String.length s - 6) :: !syms
    )
    undefs;

  List.iter (fun s -> Printf.printf "extern void *_imp_%s = 0;\n" s) !syms;
  Printf.printf "#pragma pack(1)\n";
  Printf.printf "extern struct reloctable {\n  char done;\n";
  List.iter
    (fun s -> 
       Printf.printf "  struct { char name[%i]; void **ptr; char abs; char stop; } s%s;\n"
	 (String.length s) s
    )
    !syms;
  Printf.printf " } s = { 0\n";
  List.iter (fun s -> Printf.printf "  ,{ \"%s\", &_imp_%s, 1, 0 }\n" s s)
    !syms;
  Printf.printf " };\n"

let () =
  let files = ref [] in
  for i = 1 to Array.length Sys.argv - 1 do 
    files := Sys.argv.(i) :: !files
  done;
  mkdll !files
