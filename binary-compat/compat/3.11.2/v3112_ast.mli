
module AST : sig
    
    module Asttypes : sig 
           val constant : V3112_types.Asttypes.constant -> Asttypes.constant
           val virtual_flag : V3112_types.Asttypes.virtual_flag -> Asttypes.virtual_flag
           val private_flag : V3112_types.Asttypes.private_flag -> Asttypes.private_flag
           val rec_flag : V3112_types.Asttypes.rec_flag -> Asttypes.rec_flag
           val mutable_flag : V3112_types.Asttypes.mutable_flag -> Asttypes.mutable_flag
           val direction_flag : V3112_types.Asttypes.direction_flag -> Asttypes.direction_flag
      end
      
(*      functor(S: sig end) -> sig end *)
  end

val input_intf_file : in_channel -> string -> Parsetree.signature
val input_impl_file : in_channel -> string -> Parsetree.structure
 