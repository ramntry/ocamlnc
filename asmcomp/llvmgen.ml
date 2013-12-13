exception Not_implemented_yet of string
exception Compiler_error of string
exception Debug_checkpoint of string

let dump_value value =
  if !Clflags.dump_llvm then
    Llvm.dump_value value

let gc_name = "jblab-gc"

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "simple module"
let builder = Llvm.builder context
let basicblocks_history : Llvm.llbasicblock option Stack.t = Stack.create ()
let symbol_table : (Ident.t, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16
let exit_targets : (int, Llvm.llbasicblock) Hashtbl.t = Hashtbl.create 16

let branches_counter = ref 0
let get_next_branch_name_infix () =
  let infix = string_of_int !branches_counter in
  branches_counter := !branches_counter + 1;
  infix

let lltype_of_word = Llvm.i64_type context
let lltype_of_block = Llvm.pointer_type lltype_of_word

let lltype_of_mlsize_t = lltype_of_word
let lltype_of_tag_t = Llvm.i32_type context

let lltype_of_byte = Llvm.i8_type context
let lltype_of_string = Llvm.pointer_type lltype_of_byte
let lltype_of_generic_ptr = Llvm.pointer_type lltype_of_byte
let lltype_of_root = lltype_of_generic_ptr

let lltype_of_unboxed_float = Llvm.double_type context

let lltype_of_boxed_float =
  let lltypes = [|lltype_of_word; lltype_of_unboxed_float|] in
  Llvm.struct_type context lltypes

let lltype_of_float = Llvm.pointer_type lltype_of_unboxed_float

let make_word n = Llvm.const_int lltype_of_word n
let branch_stub = make_word 1
let make_byte b = Llvm.const_int lltype_of_byte b
let make_ufloat x = Llvm.const_float lltype_of_unboxed_float x
let make_null t = Llvm.const_null t
let make_sidx i = Llvm.const_int (Llvm.i32_type context) i
let make_bidx i = Llvm.const_int (Llvm.i64_type context) i

let i1_to_word i1 =
  Llvm.build_zext i1 lltype_of_word "word_of_i1_" builder

let word_to_i1 word =
  Llvm.build_trunc word (Llvm.i1_type context) "i1_of_word" builder

let i1_to_bool i1 =
  let word = i1_to_word i1 in
  let word_one = Llvm.const_int lltype_of_word 1 in
  let doubled = Llvm.build_shl word word_one "doubled_i1_" builder in
  Llvm.build_add doubled word_one "bool" builder

let inttoptr_unsafe value new_type name =
  Llvm.build_inttoptr value new_type name builder

let inttoptr value new_type =
  let curr_type = Llvm.type_of value in
  if curr_type <> new_type && curr_type = lltype_of_word
  then inttoptr_unsafe value new_type "inttoptr"
  else value

let bitcast_unsafe value new_type name =
  Llvm.build_bitcast value new_type name builder

let ptrtoword_unsafe value name =
  Llvm.build_ptrtoint value lltype_of_word name builder

let ptrcast value new_type =
  let curr_type = Llvm.type_of value in
  if curr_type <> new_type && curr_type <> lltype_of_word
  then Llvm.build_pointercast value new_type "ptrcast" builder
  else value

let recast value new_type =
  ptrcast (inttoptr value new_type) new_type

let make_generic_fun_type numof_args =
  let fun_arg_types = Array.make numof_args lltype_of_word in
  Llvm.function_type lltype_of_word fun_arg_types

let make_external_decl name fun_arg_types =
  let fun_type = Llvm.function_type lltype_of_word fun_arg_types in
  Llvm.declare_function name fun_type the_module

let make_external_noreturn_decl name fun_arg_types =
  let fun_declaration = make_external_decl name fun_arg_types in
  Llvm.add_function_attr fun_declaration Llvm.Attribute.Noreturn;
  fun_declaration

let make_external_void_decl name fun_arg_types =
  let fun_type = Llvm.function_type (Llvm.void_type context) fun_arg_types in
  Llvm.declare_function name fun_type the_module

let caml_alloc_small_f =
  make_external_decl "caml_alloc_small" [|lltype_of_mlsize_t; lltype_of_tag_t|]

let caml_alloc_tuple_f =
  make_external_decl "caml_alloc_tuple" [|lltype_of_mlsize_t|]

let caml_alloc_closure_f =
  make_external_decl "caml_alloc_closure" [|lltype_of_mlsize_t|]

let caml_out_of_bounds_handler_f =
  make_external_noreturn_decl "caml_out_of_bounds_handler" [||]

let caml_exception_handler_f =
  make_external_noreturn_decl "caml_exception_handler" [||]

let llvm_gcroot_f =
  make_external_void_decl "llvm.gcroot"
      [|Llvm.pointer_type lltype_of_root; lltype_of_generic_ptr|]

let llvm_setjmp_f =
  let fun_type =
    Llvm.function_type (Llvm.i32_type context) [|lltype_of_generic_ptr|]
  in
  Llvm.declare_function "llvm.eh.sjlj.setjmp" fun_type the_module

let llvm_longjmp_f =
  let fun_declaration =
    make_external_void_decl "llvm.eh.sjlj.longjmp" [|lltype_of_generic_ptr|]
  in
  Llvm.add_function_attr fun_declaration Llvm.Attribute.Noreturn;
  fun_declaration

let insertion_block_exn () =
  try
    Llvm.insertion_block builder
  with Not_found ->
    raise (Compiler_error "Couldn't get the insertion block")

let insertion_block () =
  try
    Some (insertion_block_exn ())
  with Compiler_error _ -> None

let jmp_basicblock basicblock =
  Llvm.position_at_end basicblock builder

let push_basicblock () =
  Stack.push (insertion_block ()) basicblocks_history

let pop_basicblock () =
  try
    Stack.pop basicblocks_history
  with Stack.Empty ->
    raise (Compiler_error
        "Couldn't pop last basic block from empty history")

let call_basicblock basicblock =
  push_basicblock ();
  jmp_basicblock basicblock

let ret_basicblock () =
  match pop_basicblock () with
  | Some bb -> jmp_basicblock bb
  | None -> ()

let curr_function_exn () =
  Llvm.block_parent (insertion_block_exn ())

let curr_function () =
  match insertion_block () with
  | Some bb -> Some (Llvm.block_parent bb)
  | None -> None

let entry_block () =
  Llvm.entry_block (curr_function_exn ())

let fun_checkpoint message =
  begin match curr_function () with
  | Some f -> dump_value f
  | None -> ()
  end;
  raise (Debug_checkpoint message)

let build_gep base_ptr indexes name =
  Llvm.build_gep base_ptr indexes name builder

let build_load ptr name =
  Llvm.build_load ptr name builder

let build_store value ptr =
  ignore (Llvm.build_store value ptr builder)

let build_call fun_value arg_values call_name =
  if !Clflags.dump_llvm then Printf.fprintf stderr "Try to call =>\n%!";
  dump_value fun_value;
  Llvm.build_call fun_value arg_values call_name builder

let build_cond_br cond true_block false_block =
  ignore (Llvm.build_cond_br cond true_block false_block builder)

let build_br basicblock =
  ignore (Llvm.build_br basicblock builder)

let build_ret result_value =
  ignore (Llvm.build_ret result_value builder)

let unreachable () =
  ignore (Llvm.build_unreachable builder)


module Closure = struct
  let closure_field_ptr closure = function
    | 0 -> closure
    | offset -> build_gep closure [|make_bidx offset|]
        ("closure_offset_" ^ string_of_int offset ^ "_")

  let closure_field_at closure offset field_name =
    build_load (closure_field_ptr closure offset) field_name

  let closure_field_as_block closure offset field_name = inttoptr_unsafe
      (closure_field_at closure offset (field_name ^ "_untyped"))
      lltype_of_block field_name

  let store_at closure offset value =
    build_store value (closure_field_ptr closure offset)

  let store_as_word closure offset value = store_at closure offset
      (ptrtoword_unsafe value ("casted_" ^ Llvm.value_name value))

  let applicator_type numof_args =
    make_generic_fun_type (numof_args + 1)

  let part_applicator_type = make_generic_fun_type 2

  let gen_apply_name numof_args _ = "caml_apply" ^ string_of_int numof_args

  let gen_curry_name total_numof_args numof_bind_args =
    "caml_curry" ^ string_of_int total_numof_args
        ^ (if numof_bind_args = 0 then ""
                                  else "_" ^ string_of_int numof_bind_args)

  let gen_curry_app_name total_numof_args numof_bind_args =
    gen_curry_name total_numof_args numof_bind_args ^ "_app"

  let arg_name n = "a" ^ string_of_int n

  let gen_applicator_head total_numof_args numof_bind_vars to_bind name_gen =
    let app_name = name_gen total_numof_args numof_bind_vars in
    let app_type = applicator_type to_bind in
    let app_def = Llvm.define_function app_name app_type the_module in
    let app_args = Llvm.params app_def in
    Array.iteri (fun i arg -> Llvm.set_value_name
      (arg_name (numof_bind_vars + i + 1)) arg) app_args;
    Llvm.set_value_name "closure" app_args.(to_bind);
    jmp_basicblock (Llvm.entry_block app_def);
    (app_def, app_args)

  let gen_applicator_body total_numof_args free_vars_and_closure =
    let fun_args = Array.make (total_numof_args + 1) (make_word 0) in
    let numof_free_vars = (Array.length free_vars_and_closure) - 1 in
    let numof_bind_vars = total_numof_args - numof_free_vars in
    Array.blit free_vars_and_closure 0
               fun_args numof_bind_vars (numof_free_vars + 1);
    for i = numof_bind_vars - 1 downto 0 do
      let offset_corr =
        if (i = numof_bind_vars - 1) && (numof_free_vars = 1) then -1 else 0
      in
      let closure_ptr = inttoptr_unsafe fun_args.(total_numof_args)
          lltype_of_block "closure_ptr"
      in
      fun_args.(i) <- closure_field_at
          closure_ptr (3 + offset_corr) (arg_name (i + 1));
      fun_args.(total_numof_args) <- closure_field_at
          closure_ptr (4 + offset_corr)
          (gen_curry_name total_numof_args i ^ "_block")
    done;
    let closure_ptr = inttoptr_unsafe
        fun_args.(total_numof_args) lltype_of_block "closure_ptr"
    in
    let fun_untyped =
      closure_field_at closure_ptr 2 "function_untyped"
    in
    let fun_type = Llvm.pointer_type (applicator_type total_numof_args) in
    let fun_typed =
      inttoptr_unsafe fun_untyped fun_type "function" in
    build_call fun_typed fun_args "result"

  let get name_gen app_gen total_numof_args numof_bind_args =
    let app_name = name_gen total_numof_args numof_bind_args in
    match Llvm.lookup_function app_name the_module with
    | Some f -> f
    | None -> app_gen total_numof_args numof_bind_args

  let gen_apply numof_args =
    (* i64 caml_applyM(i64 a1, i64 a2, ..., i64 aM, i64* fc) *)
    let (apply_def, apply_args) =
      gen_applicator_head numof_args 0 numof_args gen_apply_name
    in
    let closure = apply_args.(numof_args) in
    let closure_ptr = inttoptr_unsafe closure lltype_of_block "closure_ptr" in
    let numof_free_vars = closure_field_at closure_ptr 1 "numof_free_vars" in
    let is_full_application =
      Llvm.build_icmp Llvm.Icmp.Eq numof_free_vars
          (make_word (2 * numof_args + 1)) "is_full_application" builder
    in
    let full_app_block = Llvm.append_block context "full_application" apply_def
    in
    let part_app_block =
      Llvm.append_block context "partial_application" apply_def
    in
    build_cond_br is_full_application full_app_block part_app_block;
    jmp_basicblock full_app_block;
    let result = gen_applicator_body numof_args apply_args in
    build_ret result;
    jmp_basicblock part_app_block;
    let part_result = ref closure in
    for i = 0 to numof_args - 1 do
      let suffix = string_of_int (i + 1) in
      let part_applicator_ptr =
        inttoptr_unsafe !part_result lltype_of_block "part_applicator_ptr"
      in
      let part_applicator = inttoptr_unsafe (build_load part_applicator_ptr
          ("part_applicator_untyped" ^ suffix)) (Llvm.pointer_type
          part_applicator_type) ("part_applicator" ^ suffix)
      in
      part_result := build_call part_applicator
          [|apply_args.(i); !part_result|] ("part_result" ^ suffix);
    done;
    build_ret !part_result;
    apply_def

  (* full applicator *)
  (* i64 caml_curryN_K_app(i64 a(K + 1), i64 a(K + 2), ..., i64 aN, i64* fc *)
  let rec gen_curry_app total_numof_args numof_bind_args =
    let (curry_app_def, curry_app_args) =
      gen_applicator_head total_numof_args numof_bind_args
      (total_numof_args - numof_bind_args) gen_curry_app_name
    in
    let result = gen_applicator_body total_numof_args curry_app_args in
    build_ret result;
    curry_app_def

  and get_curry_app total_numof_args numof_bind_args =
    get gen_curry_app_name gen_curry_app total_numof_args numof_bind_args

  (* partial applicator *)
  (* i64 caml_curryN[_K](i64 a(K + 1), i64* fc *)
  let rec gen_curry total_numof_args numof_bind_args =
    let (curry_def, curry_args) =
      gen_applicator_head total_numof_args numof_bind_args 1 gen_curry_name
    in
    let new_numof_free_vars = total_numof_args - numof_bind_args - 1 in
    let new_llnumof_free_vars = make_word (new_numof_free_vars * 2 + 1) in
    let create_new_closure size =
      let new_closure =
        build_call caml_alloc_closure_f [|make_word size|] "new_closure"
      in
      let new_closure_ptr =
        inttoptr_unsafe new_closure lltype_of_block "new_closure_ptr"
      in
      let next_part_applicator =
        get_curry total_numof_args (numof_bind_args + 1)
      in
      jmp_basicblock (Llvm.entry_block curry_def);
      store_as_word new_closure_ptr 0 next_part_applicator;
      store_at new_closure_ptr 1 new_llnumof_free_vars;
      store_at new_closure_ptr (size - 2) curry_args.(0);
      store_as_word new_closure_ptr (size - 1) curry_args.(1);
      (new_closure, new_closure_ptr)
    in
    let result =
      if numof_bind_args = total_numof_args - 1 then
        gen_applicator_body total_numof_args curry_args
      else if numof_bind_args = total_numof_args - 2 then
        fst (create_new_closure 4)
      else begin
        let (new_closure, new_closure_ptr) = create_new_closure 5 in
        let next_full_applicator =
          get_curry_app total_numof_args (numof_bind_args + 1)
        in
        jmp_basicblock (Llvm.entry_block curry_def);
        store_as_word new_closure_ptr 2 next_full_applicator;
        new_closure
      end
    in
    build_ret result;
    curry_def

  and get_curry total_numof_args numof_bind_args =
    get gen_curry_name gen_curry total_numof_args numof_bind_args
end


module Global_memory = struct
  type position =
    { llglobal_name : string
    ; index : int }

  type label = int
  type symbol = string

  let global_name_counter = ref 0

  let new_global_name () =
    let new_name = "__data" ^ string_of_int !global_name_counter ^ "_" in
    global_name_counter := !global_name_counter + 1;
    new_name

  let labels : (label, position) Hashtbl.t = Hashtbl.create 16
  let symbols : (symbol, position) Hashtbl.t = Hashtbl.create 16
  let label_requests : (position * label) list ref = ref []
  let symbol_requests : (position * symbol) list ref = ref []

  let add_label label position = Hashtbl.add labels label position
  let add_symbol symbol position = Hashtbl.add symbols symbol position

  let add_label_request position label =
    label_requests := (position, label) :: !label_requests

  let add_symbol_request position symbol =
    symbol_requests := (position, symbol) :: !symbol_requests

  let find_global llglobal_name for_link =
    match Llvm.lookup_global llglobal_name the_module with
    | Some global_struct -> global_struct
    | None ->
        raise (Compiler_error ("Can not find global name " ^ llglobal_name
               ^ " for symbol/label " ^ for_link ^ " in the module"))

  let make_idxs index = [|make_bidx 0; make_sidx index|]

  let normalize_symbol_type t =
    if t <> lltype_of_block && t <> lltype_of_float
    then lltype_of_string
    else t

  let normalized_symbol_cast v =
    let actual_type = Llvm.type_of v in
    let normalized_type = normalize_symbol_type actual_type in
    if actual_type <> normalized_type
    then Llvm.const_bitcast v normalized_type
    else v

  let try_to_generate_builtin name =
    push_basicblock ();
    let apply_re = Str.regexp "caml_apply\\([1-9][0-9]*\\)" in
    let curry_re = Str.regexp "caml_curry\\([1-9][0-9]*\\)" in
    let fun_value =
      if Str.string_match apply_re name 0 then begin
        let num = int_of_string (Str.matched_group 1 name) in
        Some (Closure.gen_apply num)
      end
      else if Str.string_match curry_re name 0 then
        let num = int_of_string (Str.matched_group 1 name) in
        Some (Closure.gen_curry num 0)
      else
        None
    in
    ret_basicblock ();
    fun_value

  let find_symbol ?numof_args symbol =
    if !Clflags.dump_llvm
      then Printf.fprintf stderr "Try to find symbol: %s =>\n%!" symbol;
    let value =
      try begin
        let {llglobal_name; index} = Hashtbl.find symbols symbol in
        let ptr =
          Llvm.const_gep (find_global llglobal_name symbol) (make_idxs index)
        in
        normalized_symbol_cast ptr
      end
      with Not_found ->
      (* Didn't find it among data symbols? Don't worry, maybe it is a function!
       * We are in a functional language, man. Everything may be a function, it
       * is cool! Look it up! *)
        match Llvm.lookup_function symbol the_module with
        | Some f -> f
        | None ->
      (* Sad story. It is not ordinary function, matter of fact. Does this tricky
       * language tries to hide something from us? So, what about that thing: maybe
       * it is some deeply-meeply builtin-intrin function? *)
            begin match try_to_generate_builtin symbol with
            | Some f -> f
            | None ->
      (* Calm down. Calm down. Just let it go. No warranty, no promises,
       * only the simple stupid declaration. That is ok. *)
                begin match numof_args with
                | Some num ->
                    let fun_type = make_generic_fun_type num in
                    Llvm.declare_function symbol fun_type the_module
                | None -> make_null lltype_of_block
                end
            end
    in
    dump_value value;
    value

  let find_label label =
      begin try
        let {llglobal_name; index} = Hashtbl.find labels label in
        Llvm.const_gep
            (find_global llglobal_name (string_of_int label)) (make_idxs index)
      with Not_found ->
        raise (Compiler_error ("Can not find label " ^ string_of_int label
               ^ " in Global_memory.labels"))
      end

  let handle_requests requests insert_f =
    List.iter (fun ({llglobal_name; index}, value) ->
      insert_f llglobal_name index value
    ) !requests;
    requests := []

  let handle_label_requests () =
    handle_requests label_requests (fun name idx label ->
      let global_struct = find_global name (string_of_int label) in
      let value = Llvm.const_ptrtoint (find_label label) lltype_of_word in
      let curr_initializer = Llvm.global_initializer global_struct in
      let new_initializer =
        Llvm.const_insertvalue curr_initializer value [|idx|]
      in
      Llvm.set_initializer new_initializer global_struct)

  let handle_symbol_requests () =
    handle_requests symbol_requests (fun name idx symbol ->
      let global_struct = find_global name symbol in
      let value = Llvm.const_ptrtoint (find_symbol symbol) lltype_of_word in
      let curr_initializer = Llvm.global_initializer global_struct in
      let new_initializer =
        Llvm.const_insertvalue curr_initializer value [|idx|]
      in
      Llvm.set_initializer new_initializer global_struct)

  let handle_requests () =
    handle_label_requests ();
    handle_symbol_requests ()
end


let eh_setjmp_buf_type = Llvm.array_type lltype_of_byte 144

let eh_buf_struct_type = Llvm.struct_type context
    [|eh_setjmp_buf_type;      (* active setjmp buffer *)
      lltype_of_generic_ptr|]  (* pointer to parent setjmp buffer *)

let eh_active_setjmp_buf = Llvm.define_global "eh_active_setjmp_buf"
    (make_null lltype_of_generic_ptr) the_module

let eh_active_exception = Llvm.define_global "eh_active_exception"
    (make_word 0) the_module


let make_basicblocks n bb_name_prefix bb_name_gen =
  let curr_fun = curr_function_exn () in
  let actual_bb_name_prefix =
    bb_name_prefix ^ get_next_branch_name_infix () ^ "_"
  in
  (Array.init n (fun i ->
    let bb_full_name = actual_bb_name_prefix ^ bb_name_gen i ^ "_" in
    Llvm.append_block context bb_full_name curr_fun),
  actual_bb_name_prefix)

let make_basicblock name =
  Llvm.append_block context name (curr_function_exn ())

let add_exit_target exit_label basic_block =
  if Hashtbl.mem exit_targets exit_label
  then raise (Compiler_error "Unexpected duplicate of exit label was encountered!")
  else Hashtbl.add exit_targets exit_label basic_block

let get_exit_target exit_label =
  try
    Hashtbl.find exit_targets exit_label
  with Not_found ->
    raise (Compiler_error "Can not find exit target")

let alloca_within_entry typeof_var name =
  call_basicblock (entry_block ());
  let var_ptr = Llvm.build_alloca typeof_var name builder in
  ret_basicblock ();
  var_ptr

let handle_gcroot root_value =
  call_basicblock (entry_block ());
  let root = Llvm.build_alloca lltype_of_root "root" builder in
  let (_ : Llvm.llvalue) =
    build_call llvm_gcroot_f [|root; make_null lltype_of_generic_ptr|] ""
  in
  ret_basicblock ();
  let casted = recast root_value lltype_of_root in
  build_store casted root;
  root_value

let build_gccall fun_value arg_values call_name =
  handle_gcroot (build_call fun_value arg_values call_name)

let build_gcload addr_value load_name =
  handle_gcroot (build_load addr_value load_name)

let ident_name ident =
  Ident.unique_name ident

let add_symbol ident v =
  let name = "local_" ^ ident_name ident in
  let local = alloca_within_entry (Llvm.type_of v) name in
  Hashtbl.add symbol_table ident local;
  build_store v local

let get_symbol ident =
  begin try
    Hashtbl.find symbol_table ident
  with Not_found ->
     raise (Compiler_error ("Local identifier " ^ ident_name ident
            ^ " not found in symbol table"))
  end

let get_symbol_value ident =
  build_load (get_symbol ident) (ident_name ident)

let is_terminated bb =
  match Llvm.instr_end bb with
  | Llvm.After last_instruction ->
      begin match Llvm.instr_opcode last_instruction with
      | Llvm.Opcode.Br
      | Llvm.Opcode.Switch
      | Llvm.Opcode.Unreachable -> true
      | _ -> false
      end
  | Llvm.At_start _bb -> false


let rec gen_expression expr =
  match expr with
  | Cmm.Cconst_int int_value -> make_word int_value
  | Cmm.Cconst_float float_string ->
      Llvm.const_float lltype_of_unboxed_float (float_of_string float_string)
  | Cmm.Cvar ident -> get_symbol_value ident
  | Cmm.Cconst_symbol symbol ->
      recast (Global_memory.find_symbol symbol) lltype_of_word
  | Cmm.Cconst_pointer value -> make_word value
  | Cmm.Ctuple [] -> branch_stub
  | Cmm.Ctuple _ ->
      raise (Not_implemented_yet "Ctuple with non-empty list of fields")

  | Cmm.Cop (Cmm.Calloc, Cmm.Cconst_natint header :: fields) ->
      let header_int = Nativeint.to_int header in
      let wosize = header_int lsr 10 in
      let wosize_value = Llvm.const_int lltype_of_mlsize_t wosize in
      let tag = 0xFF land header_int in
      let tag_value = Llvm.const_int lltype_of_tag_t tag in
      let field_values = List.fold_right (fun field_expr acc ->
        gen_expression field_expr :: acc) fields []
      in
      let fill_array array_type elem_type_name =
        let alloc_args = [|wosize_value|] in
        let block = build_gccall caml_alloc_tuple_f
           alloc_args ("allocated_" ^ elem_type_name)
        in
        let casted_block = recast block array_type in
        List.iteri (fun i field_value ->
          let store_addr = build_gep casted_block
              [|make_word i|] (elem_type_name ^ "_at_" ^ string_of_int i ^ "_ptr")
          in
          build_store field_value store_addr
        ) field_values;
        block
      in
      begin match (tag, field_values) with
      | (253 (* Double_tag *), value :: []) ->
          let alloc_args = [|wosize_value; tag_value|] in
          let float_block =
            build_gccall caml_alloc_small_f alloc_args "allocated_float"
          in
          build_store value (recast float_block lltype_of_float);
          float_block
      | (254 (* Double_array_tag *), double_values) ->
          fill_array lltype_of_float "double"
      | (0, fields_of_tuple) | (247, fields_of_tuple) ->
          fill_array lltype_of_block "field"
      | _ -> raise (Not_implemented_yet ("I don't know what to do with"
                    ^ " allocation of a block with tag = "
                    ^ string_of_int tag ^ " and number of fields = "
                    ^ string_of_int (List.length fields)
                    ^ " (in gen_expression)"))
      end

  | Cmm.Cop (Cmm.Capply (_machtype, _debuginfo), func :: args_list) ->
      let numof_args = List.length args_list in
      let fun_ptr_type = Llvm.pointer_type (make_generic_fun_type numof_args) in
      let fun_untyped =
        match func with
        | Cmm.Cconst_symbol fun_name ->
            Global_memory.find_symbol ~numof_args fun_name
        | _ -> gen_expression func
      in
      let fun_value = recast fun_untyped fun_ptr_type in
      dump_value fun_value;
      let args = Array.of_list (List.fold_right (fun arg acc ->
        gen_expression arg :: acc) args_list [])
      in
      build_gccall fun_value args "application"

  | Cmm.Cop (Cmm.Craise _debuginfo, exception_expr :: []) ->
      let exception_value = gen_expression exception_expr in
      build_store exception_value eh_active_exception;
      let active_setjmp_buf =
        build_load eh_active_setjmp_buf "active_setjmp_buf"
      in
      let (_ : Llvm.llvalue) =
        build_call llvm_longjmp_f [|active_setjmp_buf|] ""
      in
      unreachable ();
      branch_stub

  | Cmm.Cop (Cmm.Craise _debuginfo, _exception_exprs) ->
      raise (Not_implemented_yet "raise operator with more than one arg")

  | Cmm.Cop (Cmm.Ccheckbound _debuginfo, [size; index]) ->
      let size_value = recast (gen_expression size) lltype_of_word in
      let index_value = recast (gen_expression index) lltype_of_word in
      let is_too_big = Llvm.build_icmp Llvm.Icmp.Sge index_value size_value
           "is_out_of_bounds_too_big" builder
      in
      let is_negative = Llvm.build_icmp Llvm.Icmp.Slt index_value
          (Llvm.const_null lltype_of_word) "is_out_of_bounds_negative" builder
      in
      let is_out_of_bounds =
        Llvm.build_or is_too_big is_negative "is_out_of_bounds" builder
      in
      let handler_block = make_basicblock "out_of_bounds_handler" in
      let in_bounds_block = make_basicblock "in_bounds" in
      build_cond_br is_out_of_bounds handler_block in_bounds_block;
      jmp_basicblock handler_block;
      ignore (build_call caml_out_of_bounds_handler_f [||] "noreturn");
      unreachable ();
      jmp_basicblock in_bounds_block;
      branch_stub

  | Cmm.Cop (Cmm.Cextcall (fun_name, _machtype, _some_flag, _debuginfo),
             args_list) ->
      let numof_args = List.length args_list in
      let fun_value = Global_memory.find_symbol ~numof_args fun_name in
      let args = Array.of_list (List.fold_right (fun arg acc ->
        gen_expression arg :: acc) args_list [])
      in
      build_gccall fun_value args "extcall"

  | Cmm.Cop ((Cmm.Cload chunk_kind | Cmm.Cstore chunk_kind as op_kind)
            , address_expr :: value_exprs) ->
      let rec base_addr_and_offset = function
        | Cmm.Cop ((Cmm.Cadda | Cmm.Caddi), [addr_expr; offset_expr]) ->
            let (base_addr, offset_acc) = base_addr_and_offset addr_expr in
            let offset = recast (gen_expression offset_expr) lltype_of_word in
            let new_offset_acc =
              Llvm.build_add offset_acc offset "offset_acc" builder
            in
            (base_addr, new_offset_acc)
        | base_addr_expr ->
            (gen_expression base_addr_expr, make_null lltype_of_word)
      in
      let (base_addr, offset) = base_addr_and_offset address_expr in
      let (value_type, size_shift) =
        begin match chunk_kind with
        | Cmm.Byte_unsigned -> (lltype_of_byte, 0)
        | Cmm.Word -> (lltype_of_word, 3)
        | Cmm.Double_u -> (lltype_of_unboxed_float, 3)
        | _ -> raise (Not_implemented_yet ("Unsupported memory chunk kind in"
                     ^ " load / store operation"))
        end
      in
      let typed_base_addr =  recast base_addr (Llvm.pointer_type value_type) in
      let size_shift_value = Llvm.const_int lltype_of_word size_shift in
      let typed_offset =
        Llvm.build_ashr offset size_shift_value "typed_offset" builder
      in
      let effective_addr =
        build_gep typed_base_addr [|typed_offset|] "elem_addr"
      in
      begin match (op_kind, value_exprs) with
      | (Cmm.Cload Cmm.Word, []) -> build_gcload effective_addr "indexed_word"
      | (Cmm.Cload Cmm.Byte_unsigned, []) ->
          let byte = build_load effective_addr "indexed_byte" in
          Llvm.build_zext byte lltype_of_word "indexed_char" builder
      | (Cmm.Cload Cmm.Double_u, []) ->
          build_load effective_addr "indexed_double"
      | (Cmm.Cstore chunk_kind, [value_expr]) ->
          let value = gen_expression value_expr in
          if !Clflags.dump_llvm then Printf.fprintf stderr "Store %!";
          dump_value value;
          if !Clflags.dump_llvm then Printf.fprintf stderr "   in %!";
          dump_value effective_addr;
          let value_to_store =
            begin match chunk_kind with
            | Cmm.Byte_unsigned ->
                Llvm.build_trunc value lltype_of_byte "char_to_store" builder
            | _ -> value
            end
          in
          build_store value_to_store effective_addr;
          branch_stub
      | _ ->
          raise (Not_implemented_yet ("Unsupported load / store operation"
                 ^ " (non-empty argument list for load or non-equal to 1 "
                 ^ " number of arguments for store operation?"))
      end

  | Cmm.Cop (op, [lhs; rhs]) ->
      let lhs_value = gen_expression lhs in
      let rhs_value = gen_expression rhs in
      let gen_op generator name = generator lhs_value rhs_value name builder in
      begin match op with
      | Cmm.Cadda -> gen_op Llvm.build_add  "adda"
      | Cmm.Caddi -> gen_op Llvm.build_add  "addi"
      | Cmm.Csubi -> gen_op Llvm.build_sub  "subi"
      | Cmm.Cmuli -> gen_op Llvm.build_mul  "muli"
      | Cmm.Cdivi -> gen_op Llvm.build_udiv "divi"
      | Cmm.Cmodi -> gen_op Llvm.build_urem "modi"
      | Cmm.Cand  -> gen_op Llvm.build_and  "and"
      | Cmm.Cor   -> gen_op Llvm.build_or   "or"
      | Cmm.Cxor  -> gen_op Llvm.build_xor  "xor"
      | Cmm.Clsl  -> gen_op Llvm.build_shl  "lsl"
      | Cmm.Clsr  -> gen_op Llvm.build_lshr "lsr"
      | Cmm.Casr  -> gen_op Llvm.build_ashr "asr"
      | Cmm.Caddf -> gen_op Llvm.build_fadd "addf"
      | Cmm.Csubf -> gen_op Llvm.build_fsub "subf"
      | Cmm.Cmulf -> gen_op Llvm.build_fmul "mulf"
      | Cmm.Cdivf -> gen_op Llvm.build_fdiv "divf"
      | Cmm.Ccmpi cmp_flavor ->
          let llvm_cmp_flavor =
            begin match cmp_flavor with
            | Cmm.Ceq -> Llvm.Icmp.Eq
            | Cmm.Cne -> Llvm.Icmp.Ne
            | Cmm.Clt -> Llvm.Icmp.Slt
            | Cmm.Cle -> Llvm.Icmp.Sle
            | Cmm.Cgt -> Llvm.Icmp.Sgt
            | Cmm.Cge -> Llvm.Icmp.Sge
            end
          in
          let i1 = gen_op (Llvm.build_icmp llvm_cmp_flavor) "cmpi" in
          i1_to_word i1

      | Cmm.Ccmpa cmp_flavor ->
          let llvm_cmp_flavor =
            begin match cmp_flavor with
            | Cmm.Ceq -> Llvm.Icmp.Eq
            | Cmm.Cne -> Llvm.Icmp.Ne
            | Cmm.Clt -> Llvm.Icmp.Ult
            | Cmm.Cle -> Llvm.Icmp.Ule
            | Cmm.Cgt -> Llvm.Icmp.Ugt
            | Cmm.Cge -> Llvm.Icmp.Uge
            end
          in
          let i1 = gen_op (Llvm.build_icmp llvm_cmp_flavor) "cmpa" in
          i1_to_word i1

      | Cmm.Ccmpf cmp_flavor ->
          let llvm_cmp_flavor =
            begin match cmp_flavor with
            | Cmm.Ceq -> Llvm.Fcmp.Oeq
            | Cmm.Cne -> Llvm.Fcmp.Une
            | Cmm.Clt -> Llvm.Fcmp.Olt
            | Cmm.Cle -> Llvm.Fcmp.Ole
            | Cmm.Cgt -> Llvm.Fcmp.Ogt
            | Cmm.Cge -> Llvm.Fcmp.Oge
            end
          in
          let i1 = gen_op (Llvm.build_fcmp llvm_cmp_flavor) "cmpf" in
          i1_to_word i1

      | _ -> raise (Not_implemented_yet ("Binary operation matching in gen_expression"
                    ^ " (nontrivial address arithmetic?)"))
      end

  | Cmm.Cop (op, [arg]) ->
      let arg_value = gen_expression arg in
      begin match op with
      | Cmm.Cnegf ->
          Llvm.build_fneg arg_value "cnegf" builder
      | Cmm.Cfloatofint ->
          Llvm.build_sitofp arg_value lltype_of_unboxed_float "fofi" builder
      | Cmm.Cintoffloat ->
          Llvm.build_fptosi arg_value lltype_of_word "ioff" builder
      | _ -> raise (Not_implemented_yet ("Unary operation matching in"
                    ^ " gen_expression"))
      end

  | Cmm.Cexit (exit_label, []) ->
      build_br (get_exit_target exit_label);
      branch_stub

  | Cmm.Cexit (exit_label, _expr_list) ->
      raise (Not_implemented_yet "Cexit with nonempty expression list")

  | Cmm.Cswitch (control_expr, cases, exprs) ->
      let numof_cases = Array.length cases in
      if numof_cases < 2 then
        raise (Not_implemented_yet "Too few cases in switch (< 2)");
      let numof_exprs = Array.length exprs in
      if numof_exprs < 2 then
        raise (Not_implemented_yet "Too few exprs in switch (< 2)");
      let control = gen_expression control_expr in
      let (expr_blocks, actual_name_prefix) =
        make_basicblocks numof_exprs "switch" (fun i ->
          "caseexpr" ^ string_of_int cases.(i))
      in
      let switch = Llvm.build_switch
          control expr_blocks.(cases.(numof_cases - 1)) (numof_cases - 1) builder
      in
      for i = 0 to numof_cases - 2 do
        Llvm.add_case switch (make_word i) expr_blocks.(cases.(i))
      done;
      build_branches exprs expr_blocks actual_name_prefix

  | Cmm.Cifthenelse (predicate, if_true, if_false) ->
      let cond = word_to_i1 (gen_expression predicate) in
      let (branch_bbs, actual_name_prefix) =
        make_basicblocks 2 "if" (fun i ->
          [|"true"; "false"|].(i))
      in
      build_cond_br cond branch_bbs.(0) branch_bbs.(1);
      build_branches [|if_true; if_false|] branch_bbs actual_name_prefix

  | Cmm.Ccatch (exit_label, _ident_list, body, handler) ->
      let (branch_bbs, actual_name_prefix) = make_basicblocks
         2 "catch" (fun i -> [|"body"; "handler"|].(i))
      in
      build_br branch_bbs.(0);
      add_exit_target exit_label branch_bbs.(1);
      build_branches [|body; handler|] branch_bbs actual_name_prefix

  | Cmm.Ctrywith (try_expr, exn_ident, with_expr) ->
      let (branch_bbs, actual_name_prefix) =
        make_basicblocks 2 "trywith" (fun i ->
          [|"try"; "with"|].(i))
      in
      let (active_setjmp_buf, parent_setjmp_buf_ptr) =
        let new_eh_buf =
          alloca_within_entry eh_buf_struct_type (actual_name_prefix ^ "eh_buf")
        in
        (build_gep new_eh_buf [|make_bidx 0; make_sidx 0; make_bidx 0|]
            (actual_name_prefix ^ "setjmp_buf")
       , build_gep new_eh_buf [|make_bidx 0; make_sidx 1|]
            (actual_name_prefix ^ "parent_setjmp_buf_ptr"))
      in
      let parent_setjmp_buf =
        build_load eh_active_setjmp_buf "active_setjmp_buf"
      in
      build_store parent_setjmp_buf parent_setjmp_buf_ptr;
      build_store active_setjmp_buf eh_active_setjmp_buf;
      let setjmp_status = build_call llvm_setjmp_f
          [|active_setjmp_buf|] (actual_name_prefix ^ "setjmp_status")
      in
      let setjmp_cond =
        Llvm.build_is_null setjmp_status "is_normal_execution" builder
      in
      build_cond_br setjmp_cond branch_bbs.(0) branch_bbs.(1);
      jmp_basicblock branch_bbs.(1);
      build_store parent_setjmp_buf eh_active_setjmp_buf;
      Hashtbl.add symbol_table exn_ident eh_active_exception;
      let resval =
        build_branches [|try_expr; with_expr|] branch_bbs actual_name_prefix
      in
      build_store parent_setjmp_buf eh_active_setjmp_buf;
      resval

  | Cmm.Cloop expr ->
      let loop_entry_block = insertion_block_exn () in
      let (_ : Llvm.llvalue) = gen_expression expr in
      build_br loop_entry_block;
      branch_stub

  | Cmm.Clet (ident, value, expr) ->
      add_symbol ident (gen_expression value);
      gen_expression expr

  | Cmm.Cassign (ident, expr) ->
      let value = gen_expression expr in
      let local = get_symbol ident in
      build_store value local;
      branch_stub

  | Cmm.Csequence (fst, snd) ->
      let _ = gen_expression fst in
      gen_expression snd

  | _ -> raise (Not_implemented_yet "Outer matching in gen_expression")


and build_branches branch_exprs branch_bbs result_bb_name_prefix =
  let (_, branch_phi_pairs) =
    Array.fold_left (fun (i, acc) expr ->
      jmp_basicblock branch_bbs.(i);
      let value = gen_expression expr in
      let last_bb = insertion_block_exn () in
      (i + 1, (value, last_bb) :: acc))
    (0, [])
    branch_exprs
  in
  let nonterminated_branches = List.filter (fun (_v, last_bb) ->
    not (is_terminated last_bb)) branch_phi_pairs
  in
  match nonterminated_branches with
  | [] -> branch_stub
  | (value, last_bb) :: [] ->
      jmp_basicblock last_bb;
      value
  | phi_pairs ->
      let result_block = make_basicblock (result_bb_name_prefix ^ "result") in
      List.iter (fun (_v, last_bb) ->
        jmp_basicblock last_bb;
        build_br result_block)
      phi_pairs;
      jmp_basicblock result_block;
      Llvm.build_phi phi_pairs (result_bb_name_prefix ^ "resval") builder


let gen_fundecl { Cmm.fun_name; fun_args; fun_body; _ } =
  let numof_args = List.length fun_args in
  let fun_type = make_generic_fun_type numof_args in
  let fun_def = Llvm.declare_function fun_name fun_type the_module in
  Llvm.set_gc (Some gc_name) fun_def;
  let arg_llvalues = Llvm.params fun_def in
  ignore (Llvm.append_block context "entry" fun_def);
  let after_gcroots_and_vars =
    Llvm.append_block context "after_gcroots_and_vars" fun_def
  in
  jmp_basicblock after_gcroots_and_vars;
  (* Iterating over function parameters and assigning them verbose names and
   * adding them in symbol table *)
  List.iteri (fun arg_num (arg_ident, _machtype) ->
    Llvm.set_value_name (ident_name arg_ident) arg_llvalues.(arg_num);
    add_symbol arg_ident arg_llvalues.(arg_num)
  ) fun_args;
  if !Clflags.dump_llvm then dump_value fun_def;
  let ret_val = gen_expression fun_body in
  build_ret ret_val;
  jmp_basicblock (entry_block ());
  build_br after_gcroots_and_vars;
  fun_def

let gen_data items =
  let open Global_memory in
  let llglobal_name = new_global_name () in
  let make_pos index = {llglobal_name; index} in
  let (fields, fields_counter) =
    List.fold_left (fun (fls, i) item ->
      match item with
      | Cmm.Cint word ->
          (make_word (Nativeint.to_int word) :: fls, i + 1)
      | Cmm.Cint8 b -> (make_byte b :: fls, i + 1)
      | Cmm.Cstring s -> (Llvm.const_string context s :: fls, i + 1)
      | Cmm.Cdouble fs -> (make_ufloat (float_of_string fs) :: fls, i + 1)
      | Cmm.Cskip size ->
          let values = Array.make size (Llvm.const_int lltype_of_byte 0) in
          (Llvm.const_array lltype_of_byte values :: fls, i + 1)

      | Cmm.Cint16 _int -> raise (Not_implemented_yet "gen_data: Cint16")
      | Cmm.Cint32 _nativeint -> raise (Not_implemented_yet "gen_data: Cint32")
      | Cmm.Csingle _str -> raise (Not_implemented_yet "gen_data: Csingle")
      | Cmm.Calign _int -> raise (Not_implemented_yet "gen_data: Calign")

      | Cmm.Cdefine_symbol symbol ->
          Global_memory.add_symbol symbol (make_pos i);
          (fls, i)
      | Cmm.Cdefine_label label ->
          Global_memory.add_label label (make_pos i);
          (fls, i)

      | Cmm.Csymbol_address symbol ->
          add_symbol_request (make_pos i) symbol;
          (make_word 0 :: fls, i + 1)
      | Cmm.Clabel_address label ->
          add_label_request (make_pos i) label;
          (make_word 0 :: fls, i + 1)

      | Cmm.Cglobal_symbol _str -> (fls, i)

    ) ([], 0) items
  in
  let fields = Llvm.const_array lltype_of_word [||] :: fields in
  let global_value = Llvm.const_struct context (Array.of_list (List.rev fields)) in
  Llvm.define_global llglobal_name global_value the_module

let phrase = function
  | Cmm.Cfunction fundecl ->
      dump_value (gen_fundecl fundecl)
  | Cmm.Cdata items ->
      dump_value (gen_data items)

let dump_module () =
  Llvm.dump_module the_module

let finalize_module () =
  Global_memory.handle_requests ()

let dispose_module () =
  Llvm.dispose_module the_module
