
module CMI : sig
           module Ident :
             sig
               val reset : unit -> unit
        val t : Ident.t -> V3120_types.Ident.t
      end
                 module Path : sig val t : Path.t -> V3120_types.Path.t end
           module Primitive :
             sig
               val description :
                 Primitive.description -> V3120_types.Primitive.description
             end
           module Types :
             sig
               val reset : unit -> unit
               val signature : Types.signature -> V3120_types.Types.signature
               val record_representation :
                 Types.record_representation ->
                 V3120_types.Types.record_representation
             end
           module Cmi_format :
             sig
               val pers_flags :
                 Cmi_format.pers_flags -> V3120_types.Cmi_format.pers_flags
             end
  end

val output_cmi_file :
  out_channel -> string -> string -> Cmi_format.cmi_file -> Digest.t
