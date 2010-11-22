
val output_cmx_file :
  string -> string * (out_channel -> Cmx_format.unit_infos -> unit)
  
val output_cmxa_file :
  string -> string * (out_channel -> Cmx_format.library_infos -> unit)
