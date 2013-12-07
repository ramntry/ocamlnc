val phrase : Cmm.phrase -> unit
val dump_module : unit -> unit
val dispose_module : unit -> unit
val finalize_module : unit -> unit

exception Not_implemented_yet of string

val context : Llvm.llcontext
val the_module : Llvm.llmodule
val builder : Llvm.llbuilder

val lltype_of_word : Llvm.lltype
val lltype_of_block : Llvm.lltype

val caml_alloc_closure_f : Llvm.llvalue

val make_int : int -> Llvm.llvalue
