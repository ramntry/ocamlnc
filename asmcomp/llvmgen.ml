exception Cmm_type_inference_error of string
exception Not_implemented_yet of string
exception Compile_error of string
exception Debug_assumption of string

let strict_symbols_mode = false
let gc_name = "jblab-gc"

let dump_value value =
  if !Clflags.dump_llvm then
    Llvm.dump_value value

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "simple module"
let builder = Llvm.builder context
let type_table : (Ident.t, Llvm.lltype) Hashtbl.t = Hashtbl.create 16
let fun_type_table : (string, Llvm.lltype) Hashtbl.t = Hashtbl.create 16
let symbol_table : (Ident.t, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16
let param_idents_table : (string, Ident.t array) Hashtbl.t = Hashtbl.create 16
let exit_targets : (int, Llvm.llbasicblock) Hashtbl.t = Hashtbl.create 16

let ifthenelse_counter = ref 0
let get_next_ifthenelse_name_suffix () =
  let suffix = string_of_int !ifthenelse_counter in
  ifthenelse_counter := !ifthenelse_counter + 1;
  suffix

let lltype_of_word = Llvm.i64_type context
let lltype_of_block = Llvm.pointer_type lltype_of_word
let lltype_of_block_header = lltype_of_word

let lltype_of_mlsize_t = lltype_of_word
let lltype_of_tag_t = Llvm.i32_type context

let lltype_of_byte = Llvm.i8_type context
let lltype_of_string = Llvm.pointer_type lltype_of_byte
let lltype_of_generic_ptr = Llvm.pointer_type lltype_of_byte
let lltype_of_root = lltype_of_generic_ptr

let lltype_of_unboxed_float = Llvm.double_type context

let lltype_of_boxed_float =
  let lltypes = [|lltype_of_block_header; lltype_of_unboxed_float|] in
  Llvm.struct_type context lltypes

let lltype_of_float = Llvm.pointer_type lltype_of_unboxed_float

let lltype_of_int = lltype_of_word
let lltype_of_char = lltype_of_int
let lltype_of_bool = lltype_of_int

let make_int n = Llvm.const_int lltype_of_int n
let make_byte b = Llvm.const_int lltype_of_byte b
let make_sidx i = Llvm.const_int (Llvm.i32_type context) i
let make_ufloat x = Llvm.const_float lltype_of_unboxed_float x
let make_null t = Llvm.const_null t

let i1_to_cbool i1 =
  Llvm.build_zext i1 lltype_of_int "i1int" builder

let cbool_to_i1 cbool =
  Llvm.build_trunc cbool (Llvm.i1_type context) "cond" builder

let i1_to_mlbool i1 =
  let cbool = i1_to_cbool i1 in
  let int_one = Llvm.const_int lltype_of_int 1 in
  let doubled = Llvm.build_shl cbool int_one "di1int" builder in
  Llvm.build_add doubled int_one "bool" builder

let bitcast value new_type =
  if Llvm.type_of value <> new_type
  then Llvm.build_bitcast value new_type "bcast" builder
  else value

let inttoptr value new_type =
  let curr_type = Llvm.type_of value in
  if curr_type <> new_type && curr_type = lltype_of_word
  then Llvm.build_inttoptr value new_type "ipcast" builder
  else value

let ptrcast value new_type =
  let curr_type = Llvm.type_of value in
  if curr_type <> new_type && curr_type <> lltype_of_word
  then Llvm.build_pointercast value new_type "pcast" builder
  else value

let recast value new_type =
  ptrcast (inttoptr value new_type) new_type

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
        raise (Compile_error ("Can not find global name " ^ llglobal_name
               ^ " for symbol/label " ^ for_link ^ " in the module"))

  let make_idxs index = [|make_int 0; make_sidx index|]

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

  let find_symbol symbol =
    match Llvm.lookup_function symbol the_module with
    | Some f -> f
    | None ->
        begin try
          let {llglobal_name; index} = Hashtbl.find symbols symbol in
          let ptr =
            Llvm.const_gep (find_global llglobal_name symbol) (make_idxs index)
          in
          normalized_symbol_cast ptr
        with Not_found ->
          if strict_symbols_mode then
            raise (Compile_error ("Can not find symbol " ^ symbol
                   ^ " in Global_memory.symbols"))
          else Llvm.const_null lltype_of_block
        end

  let find_symbol_type symbol =
    match Llvm.lookup_function symbol the_module with
    | Some f -> lltype_of_block
    | None ->
        begin try
          let {llglobal_name; index} = Hashtbl.find symbols symbol in
          let ptr =
            Llvm.const_gep (find_global llglobal_name symbol) (make_idxs index)
          in
          normalize_symbol_type (Llvm.type_of ptr)
        with Not_found -> lltype_of_block
        end

  let find_label label =
      begin try
        let {llglobal_name; index} = Hashtbl.find labels label in
        Llvm.const_gep
            (find_global llglobal_name (string_of_int label)) (make_idxs index)
      with Not_found ->
        raise (Compile_error ("Can not find label " ^ string_of_int label
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

module Closure = struct
  let closure_field_at closure offset field_name = Llvm.build_load (Llvm.build_gep
     closure [|make_int 2|] ("closure_offset_" ^ string_of_int offset) builder)
     field_name builder

  let gen_apply numof_args =
    (* i64* caml_applyM(i64 a1, i64 a2, ..., i64 aM, i64* fc) *)
    let apply_arg_types = Array.make (numof_args + 1) lltype_of_word in
    apply_arg_types.(numof_args) <- lltype_of_block;
    let apply_type = Llvm.function_type lltype_of_word apply_arg_types in
    let apply_name = "caml_apply" ^ string_of_int numof_args in
    let apply_def = Llvm.define_function apply_name apply_type the_module in
    let apply_args = Llvm.params apply_def in
    let closure = apply_args.(numof_args) in
    Array.iteri (fun i arg ->
      Llvm.set_value_name ("a" ^ string_of_int (i + 1)) arg) apply_args;
    Llvm.set_value_name "closure" closure;
    let entry_block = Llvm.entry_block apply_def in
    Llvm.position_at_end entry_block builder;

    let numof_free_vars = closure_field_at closure 1 "numof_free_vars" in
    let is_full_application =
      Llvm.build_icmp Llvm.Icmp.Eq numof_free_vars
          (make_int (2 * numof_args + 1)) "is_full_application" builder
    in
    let full_app_block = Llvm.append_block context "full_application" apply_def
    in
    let part_app_block =
      Llvm.append_block context "partial_application" apply_def
    in
    ignore (Llvm.build_cond_br
        is_full_application full_app_block part_app_block builder);

    Llvm.position_at_end full_app_block builder;
    let full_applicator_untyped =
      closure_field_at closure 2 "full_applicator_untyped"
    in
    let full_applicator = Llvm.build_inttoptr full_applicator_untyped
        (Llvm.pointer_type apply_type) "full_applicator" builder
    in
    let result = Llvm.build_call full_applicator apply_args "result" builder in
    ignore (Llvm.build_ret result builder);

    Llvm.position_at_end part_app_block builder;
    let part_applicator_type = Llvm.pointer_type (Llvm.function_type
        lltype_of_block [|lltype_of_word; lltype_of_block|])
    in
    let part_result = ref closure in
    for i = 0 to numof_args - 1 do
      begin
        let suffix = string_of_int (i + 1) in
        let part_applicator = Llvm.build_inttoptr (Llvm.build_load !part_result
            ("part_applicator_untyped" ^ suffix) builder) part_applicator_type
            ("part_applicator" ^ suffix) builder
        in
        part_result := Llvm.build_call part_applicator
            [|apply_args.(i); !part_result|] ("closure" ^ suffix) builder;
      end
    done;
    let casted_part_result = Llvm.build_ptrtoint !part_result lltype_of_word
        "casted_part_result" builder
    in
    ignore (Llvm.build_ret casted_part_result builder);
    apply_def
end

let add_param_idents fun_name args_list =
  Hashtbl.add param_idents_table fun_name (Array.of_list args_list)

let get_param_ident fun_name idx =
  try
    (Hashtbl.find param_idents_table fun_name).(idx)
  with Not_found ->
    raise (Compile_error ("Identifier for " ^ string_of_int idx
           ^ "th parameter of function " ^ fun_name ^ " not found"))

let insertion_block () =
  try
    Llvm.insertion_block builder
  with Not_found ->
    raise (Compile_error "Couldn't get the insertion block")

let get_curr_function () =
  Llvm.block_parent (insertion_block ())

let make_basicblocks curr_function n bb_name_gen =
  Array.init n (fun i -> Llvm.append_block context (bb_name_gen i) curr_function)

let make_basicblock name =
  Llvm.append_block context name (get_curr_function ())

let add_exit_target exit_label basic_block =
  if Hashtbl.mem exit_targets exit_label
  then raise (Compile_error "Unexpected duplicate of exit label!")
  else Hashtbl.add exit_targets exit_label basic_block

let get_exit_target exit_label =
  try
    Hashtbl.find exit_targets exit_label
  with Not_found ->
    raise (Compile_error "Can not find exit target")

let unreachable () =
   Llvm.build_unreachable builder

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

let caml_out_of_bounds_handler_f =
  make_external_noreturn_decl "caml_out_of_bounds_handler" [||]

let caml_exception_handler_f =
  make_external_noreturn_decl "caml_exception_handler" [||]

let llvm_gcroot_f =
  make_external_void_decl "llvm.gcroot"
      [|Llvm.pointer_type lltype_of_root; lltype_of_generic_ptr|]

let goto_entry_block curr_block =
  let curr_function = Llvm.block_parent curr_block in
  let entry_block = Llvm.entry_block curr_function in
  Llvm.position_at_end entry_block builder

let build_call fun_value arg_values ?ret_type call_name =
  let ret_value = Llvm.build_call fun_value arg_values call_name builder in
  match ret_type with
  | Some t -> recast ret_value t
  | None -> ret_value

let handle_gcroot root_value =
  let curr_block = insertion_block () in
  goto_entry_block curr_block;
  let root = Llvm.build_alloca lltype_of_root "root" builder in
  let (_ : Llvm.llvalue) = Llvm.build_call llvm_gcroot_f
      [|root; Llvm.const_null lltype_of_generic_ptr|] "" builder
  in
  Llvm.position_at_end curr_block builder;
  let casted = recast root_value lltype_of_root in
  let (_ : Llvm.llvalue) = Llvm.build_store casted root builder in
  root_value

let build_gccall fun_value arg_values ?ret_type call_name =
  handle_gcroot (build_call fun_value arg_values ?ret_type call_name)

let build_gcload addr_value load_name =
  handle_gcroot (Llvm.build_load addr_value load_name builder)

let fun_checkpoint message =
  dump_value (get_curr_function ());
  raise (Debug_assumption message)

let join type1 type2 =
  match (type1, type2) with
  | (default, _) when default = lltype_of_word -> type2
  | (_, default) when default = lltype_of_word -> type1
  | (block, _)
    when type2 <> lltype_of_unboxed_float && block = lltype_of_block -> type2
  | (_, block)
    when type1 <> lltype_of_unboxed_float && block = lltype_of_block -> type1
  | (t1, t2) when t1 = t2 -> type1
  | _ -> raise (Cmm_type_inference_error "Can not join types")

let find_type ident =
  try Hashtbl.find type_table ident
  with Not_found -> lltype_of_word

let add_type ident t =
  if t <> lltype_of_word
  then Hashtbl.replace type_table ident (join t (find_type ident))
  else ()

let add_symbol ident v =
  let curr_block = insertion_block () in
  goto_entry_block curr_block;
  let name = "local_" ^ Ident.unique_name ident in
  let local = Llvm.build_alloca (Llvm.type_of v) name builder in
  Hashtbl.add symbol_table ident local;
  Llvm.position_at_end curr_block builder;
  ignore (Llvm.build_store v local builder)

let get_symbol ident =
  begin try
    Hashtbl.find symbol_table ident
  with Not_found ->
     raise (Compile_error "Local identifier not found in symbol table")
  end

let get_symbol_value ident =
  Llvm.build_load (get_symbol ident) (Ident.unique_name ident) builder

let make_generic_fun_type numof_args =
  let fun_arg_types = Array.make numof_args lltype_of_word in
  Llvm.function_type lltype_of_word fun_arg_types

let get_fun_type fun_name numof_args =
  try
    let finded = Hashtbl.find fun_type_table fun_name in
    if Array.length (Llvm.param_types finded) <> numof_args
    then raise (Cmm_type_inference_error ("Number of params of function in call"
                ^ " site is not equal to number of params of it in other calls"))
    else finded
  with Not_found ->
    make_generic_fun_type numof_args

let put_fun_type fun_name fun_type =
  let arg_types = Llvm.param_types fun_type in
  let numof_args = Array.length arg_types in
  let known_type = get_fun_type fun_name numof_args in
  let new_ret_type =
    join (Llvm.return_type fun_type) (Llvm.return_type known_type)
  in
  let known_arg_types = Llvm.param_types known_type in
  let new_arg_types =
    Array.init numof_args (fun i -> join known_arg_types.(i) arg_types.(i))
  in
  let new_type = Llvm.function_type new_ret_type new_arg_types in
  Hashtbl.replace fun_type_table fun_name new_type

let rec lltype_of_expr ?(demand=lltype_of_word) expr =
  let join_with_demand requested =
    try join demand requested
    with _ -> raise (Cmm_type_inference_error "demand type isn't satisfied")
  in
  match expr with
  | Cmm.Cconst_int _ -> join_with_demand lltype_of_int
  | Cmm.Cconst_natint _ -> join_with_demand lltype_of_word
  | Cmm.Cconst_float _ -> join_with_demand lltype_of_unboxed_float
  | Cmm.Cconst_symbol s -> join_with_demand (Global_memory.find_symbol_type s)
  | Cmm.Cconst_pointer _ -> join_with_demand lltype_of_word
  | Cmm.Cconst_natpointer _ -> raise (Not_implemented_yet "Cconst_natpointer")
  | Cmm.Cvar ident ->
      begin try
        let joined_type = join_with_demand (find_type ident) in
        add_type ident joined_type;
        joined_type
      with _ -> raise (Cmm_type_inference_error ("Demand type and finded one in"
                       ^ " the type table can not be joined while typify Cvar"))
      end

  | Cmm.Cop (Cmm.Capply (_machtype, _debuginfo),
            (Cmm.Cconst_symbol fun_name) :: args_list) ->
      begin match Llvm.lookup_function fun_name the_module with
      | None ->
          let known_fun_type = get_fun_type fun_name (List.length args_list) in
          let known_arg_types = Llvm.param_types known_fun_type in
          let inferred_arg_types = Array.of_list (List.mapi (fun i arg ->
            lltype_of_expr ~demand:known_arg_types.(i) arg) args_list)
          in
          Array.iteri (fun i arg_type ->
            add_type (get_param_ident fun_name i) inferred_arg_types.(i))
            inferred_arg_types;
          let inferred_ret_type =
            join_with_demand (Llvm.return_type known_fun_type)
          in
          put_fun_type fun_name
              (Llvm.function_type inferred_ret_type inferred_arg_types);
          inferred_ret_type

      | Some fun_value ->
          let fun_type = Llvm.element_type (Llvm.type_of fun_value) in
          let param_types = Llvm.param_types fun_type in
          if List.length args_list > Array.length param_types
          then raise (Cmm_type_inference_error ("Function call with incorrect"
                      ^ " (too big) number of arguments")) else ();
          List.iteri (fun i p ->
            ignore (lltype_of_expr ~demand:param_types.(i) p))
            args_list;
          join_with_demand (Llvm.return_type fun_type)
      end

  | Cmm.Cop (Cmm.Capply (_machtype, _debuginfo), func :: args_list) ->
      let _ = lltype_of_expr ~demand:lltype_of_block func in
      List.iter (fun arg -> ignore (lltype_of_expr arg)) args_list;
      lltype_of_word

  | Cmm.Cop (Cmm.Cextcall (fun_name, _machtype, _some_flag, _debuginfo),
             args_list) ->
      List.iter (fun arg -> ignore (lltype_of_expr arg)) args_list;
      demand

  | Cmm.Cop (Cmm.Craise _debuginfo, _args) -> demand
  | Cmm.Cop (Cmm.Ccheckbound _debuginfo, [size; index]) ->
      let _ = lltype_of_expr ~demand:lltype_of_word size in
      let _ = lltype_of_expr ~demand:lltype_of_word index in
      demand

  | Cmm.Cop (Cmm.Ccheckbound _debuginfo, _args) ->
      raise (Not_implemented_yet "Ccheckbound with no two args")

  | Cmm.Cop (Cmm.Calloc, Cmm.Cconst_natint header :: fields) ->
      let tag = 0xFF land (Nativeint.to_int header) in
      begin match (tag, fields) with
      | (253, value :: []) ->
          let _ = lltype_of_expr ~demand:lltype_of_unboxed_float value in
          join_with_demand lltype_of_float
      | (0, _) | (247, _) -> join_with_demand lltype_of_block
      | _ -> raise (Not_implemented_yet ("I don't know what to do with"
                    ^ " allocation of a block with tag = "
                    ^ string_of_int tag ^ " and number of fields = "
                    ^ string_of_int (List.length fields)
                    ^ " (in lltype_of_expr)"))
      end

  | Cmm.Cop (op, [lhs; rhs]) ->
      let binary_op args_type res_type =
        let _ = lltype_of_expr ~demand:args_type lhs in
        let _ = lltype_of_expr ~demand:args_type rhs in
        join_with_demand res_type
      in
      begin match op with
      | Cmm.Caddi | Cmm.Csubi | Cmm.Cmuli | Cmm.Cdivi | Cmm.Cmodi
        | Cmm.Cand | Cmm.Cor | Cmm.Cxor | Cmm.Clsl | Cmm.Clsr | Cmm.Casr
        | Cmm.Ccmpi _ -> binary_op lltype_of_int lltype_of_int
      | Cmm.Cadda ->
          let joined_ptr_type = join_with_demand lltype_of_block in
          let _ = lltype_of_expr ~demand:joined_ptr_type lhs in
          let _ = lltype_of_expr ~demand:lltype_of_int rhs in
          joined_ptr_type

      | Cmm.Csuba -> binary_op lltype_of_block lltype_of_int
      | Cmm.Ccmpa _ -> binary_op lltype_of_block lltype_of_int
      | Cmm.Ccmpf _ -> binary_op lltype_of_unboxed_float lltype_of_int
      | Cmm.Cnegf -> raise (Not_implemented_yet "Cnegf")
      | Cmm.Caddf | Cmm.Csubf | Cmm.Cmulf | Cmm.Cdivf ->
          binary_op lltype_of_unboxed_float lltype_of_unboxed_float
      | Cmm.Cstore chunk ->
          begin match chunk with
          | Cmm.Word ->
              let _ = lltype_of_expr ~demand:lltype_of_block lhs in
              let _ = lltype_of_expr ~demand:lltype_of_word rhs in
              lltype_of_word
          | Cmm.Byte_unsigned ->
              let _ = lltype_of_expr ~demand:lltype_of_string lhs in
              let _ = lltype_of_expr ~demand:lltype_of_word rhs in
              lltype_of_word
          | Cmm.Double_u ->
              let _ = lltype_of_expr ~demand:lltype_of_float lhs in
              let _ = lltype_of_expr ~demand:lltype_of_unboxed_float rhs in
              lltype_of_word
          | _ -> raise (Not_implemented_yet ("I don't know what to do with"
                        ^ " store of chunk of such type (in lltype_of_expr)"))
          end
      | _ -> raise (Not_implemented_yet ("Binary operation matching"
                    ^ " in lltype_of_expr"))
      end
  | Cmm.Cop (op, [arg]) ->
      let unary_op arg_type res_type =
        let _ = lltype_of_expr ~demand:arg_type arg in
        join_with_demand res_type
      in
      begin match op with
      | Cmm.Cload chunk ->
          begin match chunk with
          | Cmm.Double_u -> unary_op lltype_of_float lltype_of_unboxed_float
          | Cmm.Word -> unary_op lltype_of_block lltype_of_word
          | Cmm.Byte_unsigned -> unary_op lltype_of_string lltype_of_char
          | _ -> raise (Not_implemented_yet ("I don't know what to do with"
                        ^ " load of chunk of such type (in lltype_of_expr)"))
          end
      | Cmm.Cnegf -> unary_op lltype_of_unboxed_float lltype_of_unboxed_float
      | Cmm.Cabsf -> unary_op lltype_of_unboxed_float lltype_of_unboxed_float
      | Cmm.Cfloatofint -> unary_op lltype_of_int lltype_of_unboxed_float
      | Cmm.Cintoffloat -> unary_op lltype_of_unboxed_float lltype_of_int
      | _ -> raise (Not_implemented_yet ("Unary operation matching in"
                    ^ " lltype_of_expr"))
      end

  | Cmm.Cifthenelse (predicate, if_true, if_false) ->
      let _ = lltype_of_expr ~demand:lltype_of_int predicate in
      lltype_of_branch ~demand if_true if_false

  | Cmm.Ccatch (_int, _ident_list, body, handler) ->
      lltype_of_branch ~demand body handler

  | Cmm.Cloop expr ->
      let _ =  lltype_of_expr expr in
      lltype_of_word

  | Cmm.Clet (ident, value, expr) -> let ident_type = lltype_of_expr value in
      add_type ident ident_type;
      let expr_type = lltype_of_expr ~demand expr in
      let new_ident_type = find_type ident in
      if ident_type = lltype_of_word && new_ident_type <> lltype_of_word
      then ignore (lltype_of_expr ~demand:new_ident_type value)
      else ();
      join_with_demand expr_type

  | Cmm.Csequence (fst, snd) ->
      let _ = lltype_of_expr fst in
      lltype_of_expr ~demand snd

  | Cmm.Cswitch (control_expr, _cases, exprs) ->
      let _ = lltype_of_expr ~demand:lltype_of_int control_expr in
      let joined_type = Array.fold_left (fun acc expr ->
        join acc (lltype_of_expr ~demand:acc expr)) demand exprs
      in
      let rejoined_type = Array.fold_left (fun acc expr ->
        join acc (lltype_of_expr ~demand:acc expr)) joined_type exprs
      in
      rejoined_type

  | Cmm.Cassign (ident, expr) ->
      let expr_type = lltype_of_expr expr in
      add_type ident expr_type;
      let _ = lltype_of_expr ~demand:(find_type ident) expr in
      demand

  | Cmm.Ctuple _ -> demand
  | Cmm.Cexit (_int, _expr_list) -> demand

  | Cmm.Ctrywith (_expr1, _ident, _expr2) ->
      raise (Not_implemented_yet "Ctrywith")
  | Cmm.Cop (_op, []) ->
      raise (Not_implemented_yet "Cmm.Cop with empty list of args - strange")
  | Cmm.Cop (_op, _ :: _ :: _ :: _) ->
      raise (Not_implemented_yet "Cmm.Cop with more then 2 args - strange")

and lltype_of_branch ~demand br1 br2 =
    let br1_type = lltype_of_expr ~demand br1 in
    let br2_type = lltype_of_expr ~demand:br1_type br2 in
    lltype_of_expr ~demand:br2_type br1


let lltype_of_machtype : Cmm.machtype -> Llvm.lltype = function
  | [||] -> Llvm.void_type context
  | [|Cmm.Addr|] -> lltype_of_block
  | [|Cmm.Int|] -> lltype_of_int
  | [|Cmm.Float|] -> lltype_of_unboxed_float
  | too_long_array ->
      raise (Not_implemented_yet ("I don't know yet what to do with machtype array with "
        ^ string_of_int (Array.length too_long_array) ^ " machtype_component's"))

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
  | Cmm.Cconst_int int_value -> Llvm.const_int lltype_of_int int_value
  | Cmm.Cconst_float float_string ->
      Llvm.const_float lltype_of_unboxed_float (float_of_string float_string)
  | Cmm.Cvar ident -> get_symbol_value ident

  | Cmm.Cconst_symbol symbol -> Global_memory.find_symbol symbol
  | Cmm.Cconst_pointer value -> make_int value
  | Cmm.Ctuple [] -> make_int 1
  | Cmm.Ctuple _ ->
      raise (Not_implemented_yet "Ctuple with non-empty list of fields")

  | Cmm.Cop (Cmm.Calloc, Cmm.Cconst_natint header :: fields) ->
      let header_int = Nativeint.to_int header in
      let wosize = header_int lsr 10 in
      let wosize_value = Llvm.const_int lltype_of_mlsize_t wosize in
      let tag = 0xFF land header_int in
      let tag_value = Llvm.const_int lltype_of_tag_t tag in
      begin match (tag, fields) with
      | (253 (* Double_tag *), value :: []) ->
          let unboxed_float_value = gen_expression value in
          let alloc_args = [|wosize_value; tag_value|] in
          let float_block =
            build_gccall caml_alloc_small_f alloc_args ~ret_type:lltype_of_float "salloc"
          in
          let (_ : Llvm.llvalue) =
            Llvm.build_store unboxed_float_value float_block builder
          in
          float_block
      | (254 (* Double_array_tag *), double_values) ->
          let alloc_args = [|wosize_value|] in
          let double_array =
            build_gccall caml_alloc_tuple_f alloc_args ~ret_type:lltype_of_float "talloc"
          in
          List.iteri (fun i double_expr ->
            let current_double =
              recast (gen_expression double_expr) lltype_of_unboxed_float
            in
            let store_addr = Llvm.build_gep double_array [|make_int i|]
              ("double_elem_addr" ^ string_of_int i ^ "_") builder
            in
            ignore (Llvm.build_store current_double store_addr builder)
          ) fields;
          double_array

      | (0, fields_of_tuple) | (247, fields_of_tuple) ->
          let alloc_args = [|wosize_value|] in
          let block =
            build_gccall caml_alloc_tuple_f alloc_args ~ret_type:lltype_of_block "talloc"
          in
          List.iteri (fun i field ->
            let field_value = recast (gen_expression field) lltype_of_word in
            let store_addr = Llvm.build_gep block [|make_int i|]
                  ("fieldi" ^ string_of_int i) builder
            in
            ignore (Llvm.build_store field_value store_addr builder)
          ) fields;
          block

      | _ -> raise (Not_implemented_yet ("I don't know what to do with"
                    ^ " allocation of a block with tag = "
                    ^ string_of_int tag ^ " and number of fields = "
                    ^ string_of_int (List.length fields)
                    ^ " (in gen_expression)"))
      end

  | Cmm.Cop (Cmm.Capply (_machtype, _debuginfo),
            (Cmm.Cconst_symbol fun_name) :: args_list) ->
      if !Clflags.dump_llvm then Printf.fprintf stderr "Capply of %s\n%!" fun_name;
      let fun_value = Global_memory.find_symbol fun_name in
      let arg_types =
        Llvm.param_types (Llvm.element_type (Llvm.type_of(fun_value)))
      in
      let args = Array.of_list (List.mapi (fun i arg ->
        let arg_value = gen_expression arg in
        recast arg_value arg_types.(i)) args_list) in
      build_gccall fun_value args "capply"

  | Cmm.Cop (Cmm.Craise _debuginfo, _args) ->
      let (_ : Llvm.llvalue) =
        Llvm.build_call caml_exception_handler_f [||] "noreturn" builder
      in
      unreachable ()

  | Cmm.Cop (Cmm.Ccheckbound _debuginfo, [size; index]) ->
      let size_value = recast (gen_expression size) lltype_of_word in
      let index_value = recast (gen_expression index) lltype_of_word in
      let is_too_big = Llvm.build_icmp Llvm.Icmp.Sge index_value size_value
           "is_out_of_bounds_too_big" builder
      in
      let is_negative = Llvm.build_icmp Llvm.Icmp.Slt index_value
          (Llvm.const_null lltype_of_int) "is_out_of_bounds_negative" builder
      in
      let is_out_of_bounds =
        Llvm.build_or is_too_big is_negative "is_out_of_bounds" builder
      in
      let handler_block = make_basicblock "out_of_bounds_handler" in
      let in_bounds_block = make_basicblock "in_bounds" in
      ignore (Llvm.build_cond_br is_out_of_bounds
          handler_block in_bounds_block builder);
      Llvm.position_at_end handler_block builder;
      ignore (Llvm.build_call caml_out_of_bounds_handler_f [||] "noreturn" builder);
      ignore (unreachable ());
      Llvm.position_at_end in_bounds_block builder;
      make_int 1

  | Cmm.Cop (Cmm.Cextcall (fun_name, _machtype, _some_flag, _debuginfo),
             args_list) ->
      let fun_value =
        begin match Llvm.lookup_function fun_name the_module with
        | None ->
            let arg_types = Array.make (List.length args_list) lltype_of_word in
            let fun_type = Llvm.function_type lltype_of_word arg_types in
            Llvm.declare_function fun_name fun_type the_module
        | Some some_value -> some_value
        end
      in
      let arg_types =
        Llvm.param_types (Llvm.element_type (Llvm.type_of(fun_value)))
      in
      let args = Array.of_list (List.mapi (fun i arg ->
        recast (gen_expression arg) arg_types.(i)) args_list) in
      build_gccall fun_value args "capply"

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
            (gen_expression base_addr_expr, Llvm.const_null lltype_of_word)
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
        Llvm.build_gep typed_base_addr [|typed_offset|] "elem_addr" builder
      in
      begin match (op_kind, value_exprs) with
      | (Cmm.Cload Cmm.Word, []) -> build_gcload effective_addr "indexed_word"
      | (Cmm.Cload Cmm.Byte_unsigned, []) ->
          let byte = Llvm.build_load effective_addr "indexed_byte" builder in
          Llvm.build_zext byte lltype_of_char "indexed_char" builder
      | (Cmm.Cload Cmm.Double_u, []) ->
          Llvm.build_load effective_addr "indexed_double" builder
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
            | _ -> recast value value_type
            end
          in
          Llvm.build_store value_to_store effective_addr builder
      | _ ->
          raise (Not_implemented_yet ("Unsupported load / store operation"
                 ^ " (non-empty argument list for load or non-equal to 1 "
                 ^ " number of arguments for store operation?"))
      end

  | Cmm.Cop (op, [lhs; rhs]) ->
      let lhs_value = gen_expression lhs in
      let rhs_value = gen_expression rhs in
      let gen_op generator name = generator lhs_value rhs_value name builder in
      let gen_op_int generator name = generator (recast lhs_value lltype_of_int)
          (recast rhs_value lltype_of_int) name builder
      in
      begin match op with
      | Cmm.Caddi -> gen_op_int Llvm.build_add  "caddi"
      | Cmm.Csubi -> gen_op_int Llvm.build_sub  "csubi"
      | Cmm.Cmuli -> gen_op_int Llvm.build_mul  "cmuli"
      | Cmm.Cdivi -> gen_op_int Llvm.build_udiv "cdivi"
      | Cmm.Cmodi -> gen_op_int Llvm.build_urem "cmodi"
      | Cmm.Cand  -> gen_op_int Llvm.build_and  "cand"
      | Cmm.Cor   -> gen_op_int Llvm.build_or   "cor"
      | Cmm.Cxor  -> gen_op_int Llvm.build_xor  "cxor"
      | Cmm.Clsl  -> gen_op_int Llvm.build_shl  "clsl"
      | Cmm.Clsr  -> gen_op_int Llvm.build_lshr "clsr"
      | Cmm.Casr  -> gen_op_int Llvm.build_ashr "casr"
      | Cmm.Caddf -> gen_op Llvm.build_fadd "caddf"
      | Cmm.Csubf -> gen_op Llvm.build_fsub "csubf"
      | Cmm.Cmulf -> gen_op Llvm.build_fmul "cmulf"
      | Cmm.Cdivf -> gen_op Llvm.build_fdiv "cdivf"
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
          let i1 = gen_op_int (Llvm.build_icmp llvm_cmp_flavor) "ccmpi" in
          i1_to_cbool i1

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
          let i1 = gen_op_int (Llvm.build_icmp llvm_cmp_flavor) "ccmpa" in
          i1_to_cbool i1

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
          let i1 = gen_op (Llvm.build_fcmp llvm_cmp_flavor) "ccmpf" in
          i1_to_cbool i1

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
          let casted = bitcast arg_value lltype_of_unboxed_float in
          Llvm.build_fptosi casted lltype_of_int "ioff" builder
      | _ -> raise (Not_implemented_yet ("Unary operation matching in"
                    ^ " gen_expression"))
      end

  | Cmm.Cexit (exit_label, []) -> Llvm.build_br (get_exit_target exit_label) builder

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
      let curr_function = get_curr_function () in
      let expr_blocks =
        make_basicblocks curr_function numof_exprs (fun i ->
          "caseexpr" ^ string_of_int cases.(i) ^ "_")
      in
      let switch =
        Llvm.build_switch control expr_blocks.(cases.(numof_cases - 1)) (numof_cases - 1) builder
      in
      for i = 0 to numof_cases - 2 do
        Llvm.add_case switch (make_int i) expr_blocks.(cases.(i))
      done;
      build_branches curr_function exprs expr_blocks "switch"

  | Cmm.Cifthenelse (predicate, if_true, if_false) ->
      let cond = cbool_to_i1 (gen_expression predicate) in
      let curr_function = get_curr_function () in
      let name_prefix = "if" ^ get_next_ifthenelse_name_suffix () in
      let branch_bbs =
        make_basicblocks curr_function 2 (fun i ->
          [|name_prefix ^ "true"; name_prefix ^ "false"|].(i))
      in
      let (_ : Llvm.llvalue) =
        Llvm.build_cond_br cond branch_bbs.(0) branch_bbs.(1) builder
      in
      build_branches curr_function [|if_true; if_false|] branch_bbs name_prefix

  | Cmm.Ccatch (exit_label, _ident_list, body, handler) ->
      let curr_function = get_curr_function () in
      let branch_bbs =
        make_basicblocks curr_function 2 (fun i -> [|"body"; "handler"|].(i))
      in
      let (_ : Llvm.llvalue) = Llvm.build_br branch_bbs.(0) builder in
      add_exit_target exit_label branch_bbs.(1);
      build_branches curr_function [|body; handler|] branch_bbs "catch"

  | Cmm.Cloop expr ->
      let loop_entry_block = insertion_block () in
      let (_ : Llvm.llvalue) = gen_expression expr in
      Llvm.build_br loop_entry_block builder

  | Cmm.Clet (ident, value, expr) ->
      add_symbol ident (gen_expression value);
      gen_expression expr

  | Cmm.Cassign (ident, expr) ->
      let value = gen_expression expr in
      let local = get_symbol ident in
      let _ = Llvm.build_store value local builder in
      make_int 1

  | Cmm.Csequence (fst, snd) ->
      let _ = gen_expression fst in
      gen_expression snd

  | _ -> raise (Not_implemented_yet "Outer matching in gen_expression")


and build_branches curr_function branch_exprs branch_bbs result_bb_name_prefix =
  let (_, branch_phi_pairs) =
    Array.fold_left (fun (i, acc) expr ->
      Llvm.position_at_end branch_bbs.(i) builder;
      let value = gen_expression expr in
      let last_bb = insertion_block () in
      (i + 1, (value, last_bb) :: acc))
    (0, [])
    branch_exprs
  in
  let nonterminated_branches = List.filter (fun (_v, last_bb) ->
    not (is_terminated last_bb)) branch_phi_pairs
  in
  match nonterminated_branches with
  | [] -> make_int 1
  | (value, last_bb) :: [] ->
      Llvm.position_at_end last_bb builder;
      value
  | phi_pairs ->
      let accurate_type = List.fold_left (fun acc_type (v, last_bb) ->
        join (Llvm.type_of v) acc_type) lltype_of_word phi_pairs
      in
      let result_block =
        Llvm.append_block context (result_bb_name_prefix ^ "result") curr_function
      in
      let casted_phi_pairs = List.map (fun (v, last_bb) ->
        let casted_value =
          Llvm.position_at_end last_bb builder;
          recast v accurate_type
        in
        dump_value casted_value;
        ignore (Llvm.build_br result_block builder);
        (casted_value, last_bb)) phi_pairs
      in
      (*fun_checkpoint "cp";*)
      Llvm.position_at_end result_block builder;
      Llvm.build_phi casted_phi_pairs (result_bb_name_prefix ^ "resval") builder


let gen_fundecl { Cmm.fun_name; fun_args; fun_body; _ } =
  let ret_type = lltype_of_expr fun_body in
  let arg_types =
    List.map (fun (ident, _machtype) -> find_type ident) fun_args
    |> Array.of_list
  in
  let fun_type = Llvm.function_type ret_type arg_types in
  put_fun_type fun_name fun_type;
  let accurate_fun_type = get_fun_type fun_name (Array.length arg_types) in
  let accurate_ret_type = Llvm.return_type accurate_fun_type in
  let fun_def = Llvm.declare_function fun_name accurate_fun_type the_module in
  Llvm.set_gc (Some gc_name) fun_def;
  let arg_llvalues = Llvm.params fun_def in
  (* Iterating over function parameters and assigning them verbose names and
   * adding them in symbol table *)
  let entry_bb = Llvm.append_block context "entry" fun_def in
  let after_gcroots_and_vars =
    Llvm.append_block context "after_gcroots_and_vars" fun_def
  in
  Llvm.position_at_end after_gcroots_and_vars builder;
  List.iteri (fun arg_num (arg_ident, _machtype) ->
    Llvm.set_value_name (Ident.unique_name arg_ident) arg_llvalues.(arg_num);
    add_symbol arg_ident arg_llvalues.(arg_num)
  ) fun_args;
  if !Clflags.dump_llvm then dump_value fun_def;
  let ret_val = gen_expression fun_body in
  let (_ : Llvm.llvalue) =
    Llvm.build_ret (recast ret_val accurate_ret_type) builder
  in
  Llvm.position_at_end entry_bb builder;
  let (_ : Llvm.llvalue) = Llvm.build_br after_gcroots_and_vars builder in
  fun_def

let gen_data items =
  dump_value (Closure.gen_apply 20);
  raise (Debug_assumption "caml_applyM was generated");
  let open Global_memory in
  let llglobal_name = new_global_name () in
  let make_pos index = {llglobal_name; index} in
  let (fields, fields_counter) =
    List.fold_left (fun (fls, i) item ->
      match item with
      | Cmm.Cint word ->
          (make_int (Nativeint.to_int word) :: fls, i + 1)
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
          (make_int 0 :: fls, i + 1)
      | Cmm.Clabel_address label ->
          add_label_request (make_pos i) label;
          (make_int 0 :: fls, i + 1)

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
