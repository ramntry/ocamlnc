
module CMO :  sig
                 module Lambda :
             sig
               val structured_constant :
                 Lambda.structured_constant ->
                 V3120_types.Lambda.structured_constant
               val primitive :
                 Lambda.primitive -> V3120_types.Lambda.primitive
               val meth_kind :
                 Lambda.meth_kind -> V3120_types.Lambda.meth_kind
             end
           module Cmo_format :
             sig
               val compilation_unit :
                 Cmo_format.compilation_unit ->
                 V3120_types.Cmo_format.compilation_unit
               val library :
                 Cmo_format.library -> V3120_types.Cmo_format.library
             end
         end

val output_cmo_file :
           string ->
           string * (out_channel -> Cmo_format.compilation_unit -> unit)

val output_cma_file :
           string ->
           string * (out_channel -> Cmo_format.library -> unit)
