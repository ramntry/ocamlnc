
module CMO :  sig
           module Lambda :
             sig
               val structured_constant :
                 V3112_types.Lambda.structured_constant ->
                 Lambda.structured_constant
               val primitive :
                 V3112_types.Lambda.primitive -> Lambda.primitive
               val meth_kind :
                 V3112_types.Lambda.meth_kind -> Lambda.meth_kind
             end
           module Cmo_format :
             sig
               val compilation_unit :
                 V3112_types.Cmo_format.compilation_unit ->
                 Cmo_format.compilation_unit
               val library :
                 V3112_types.Cmo_format.library -> Cmo_format.library
             end
         end

  (* functor(M : sig end) -> sig end *)
  
val input_cmo_file : in_channel -> string -> Cmo_format.cmo_unit
