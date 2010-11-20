
(*
(* module CMX : functor(M : sig end) -> sig end *)
  
val input_cmx_file : in_channel -> string -> Cmx_format.unit_infos
val input_cmxa_file : in_channel -> string -> Cmx_format.library_infos
*)