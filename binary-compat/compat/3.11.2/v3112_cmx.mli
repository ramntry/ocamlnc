
module CMX : functor(M : sig end) -> sig end
  
val input_cmx_file : in_channel -> string -> Cmo_format.cmo_unit
