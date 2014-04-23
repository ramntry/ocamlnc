#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <unwind.h>
#include <string.h>
#include <stdarg.h>
#include <caml/mlvalues.h>

#define Make_header(wosize, tag, color) ((wosize) << 10 | (color) | (tag))
#define SHORT_STRING_LENGTH 1024

header_t caml_atom_table[256] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
  14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
  71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
  90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
  107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
  122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136,
  137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151,
  152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166,
  167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181,
  182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196,
  197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211,
  212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226,
  227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241,
  242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255 };

static void print_log(char const *fun_name, int line_number, char const *message_format, ...)
{
  fprintf(stderr, "--> (runtime log): in %s:%d: ", fun_name, line_number);
  va_list args;
  va_start(args, message_format);
  vfprintf(stderr, message_format, args);
  va_end(args);
  fputc('\n', stderr);
}

#ifndef NDEBUG
  #define log(...) \
    print_log(__func__, __LINE__, __VA_ARGS__)
#else
  #define log(...) \
  (void)0
#endif

static value alloc_via_malloc(mlsize_t wosize, tag_t tag)
{
  mlsize_t allocation_size = (wosize + 1) * sizeof(value);
  value *block = (value *)MALLOC(allocation_size);
  *block = Make_header(wosize, tag, 0);
  return (value)(block + 1);
}

value caml_alloc_small(mlsize_t wosize, tag_t tag)
{
  log("allocation of %lu word(s) (+1 for header) with tag %d", wosize, tag);
  return alloc_via_malloc(wosize, tag);
}

value caml_alloc_tuple(mlsize_t wosize)
{
  log("allocation of %lu word(s) (+1 for header) with tag 0", wosize);
  return alloc_via_malloc(wosize, 0);
}

value caml_alloc_closure(mlsize_t wosize)
{
  log("allocation of %lu word(s) (+1 for header) with Closure_tag", wosize);
  return alloc_via_malloc(wosize, Closure_tag);
}

value caml_modify(value addr, value new_value)
{
  *(value *)addr = new_value;
  return Val_unit;
}

value caml_exception_handler(void)
{
  printf("\nFatal error: Unknown exception\n");
  exit(0);
  return Val_unit;
}

value caml_out_of_bounds_handler(void)
{
  printf("\nFatal error: Out of bounds exception\n");
  exit(0);
  return Val_unit;
}

static value alloc_float(double d)
{
  value block = caml_alloc_small(1, Double_tag);
  *(double *)block = d;
  return block;
}

value caml_make_vect(value len, value init) /* int -> 'a -> 'a array */
{
  mlsize_t size = Long_val(len);
  if (size == 0)
    return Atom(0);
  if (Is_block(init) && Tag_val(init) == Double_tag) {
    double const init_double = Double_val(init);
    value res = alloc_via_malloc(size * Double_wosize, Double_array_tag);
    for (mlsize_t i = 0; i < size; ++i)
      Store_double_field(res, i, init_double);
    return res;
  }
  value res = alloc_via_malloc(size, 0);
  for (mlsize_t i = 0; i < size; ++i)
    Field(res, i) = init;
  return res;
}

value caml_array_sub(value array, value start_idx, value len) /* 'a array -> int -> int -> 'a array */
{
  tag_t tag = Tag_val(array);
  mlsize_t size = Long_val(len);
  mlsize_t idx = Long_val(start_idx);
  if (size == 0)
    return Atom(tag);
  if (tag == Double_array_tag) {
    size *= Double_wosize;
    idx *= Double_wosize;
  }
  value new_array = alloc_via_malloc(size, tag);
  memcpy((void *)new_array, (void *)((value *)array + idx), size * sizeof(value));
  return new_array;
}

value caml_array_append(value lhs, value rhs) /* 'a array -> 'a array -> 'a array */
{
  tag_t tag = Tag_val(lhs);
  assert(tag == Tag_val(rhs));
  mlsize_t lhs_size = Wosize_val(lhs);
  mlsize_t rhs_size = Wosize_val(rhs);
  value appended = alloc_via_malloc(lhs_size + rhs_size, tag);
  memcpy((void *)appended, (void *)lhs, lhs_size * sizeof(value));
  memcpy((void *)((value *)appended + lhs_size), (void *)rhs, rhs_size * sizeof(value));
  return appended;
}

value caml_array_concat(value array_list) /* 'a array list -> 'a array */
{
  if (!Is_block(array_list))
    return Atom(0);
  value first_array = Field(array_list, 0);
  tag_t tag = Tag_val(first_array);
  mlsize_t total_size = Wosize_val(first_array);
  int list_length = 1;
  value cursor = Field(array_list, 1);
  while (Is_block(cursor)) {
    ++list_length;
    value array = Field(cursor, 0);
    assert(tag == Tag_val(array));
    total_size += Wosize_val(array);
    cursor = Field(cursor, 1);
  }
  value concated = alloc_via_malloc(total_size, tag);
  mlsize_t offset = 0;
  for (cursor = array_list; list_length; --list_length) {
    value array = Field(cursor, 0);
    mlsize_t size = Wosize_val(array);
    memcpy((void *)((value *)concated + offset), (void *)array, size * sizeof(value));
    offset += size;
    cursor = Field(cursor, 1);
  }
  return concated;
}

value caml_array_blit(value src, value src_start, value dst, value dst_start, value len) /* 'a array -> int -> 'a array -> int -> int -> unit */
{
  mlsize_t src_start_idx = Long_val(src_start);
  mlsize_t dst_start_idx = Long_val(dst_start);
  mlsize_t size = Long_val(len);
  tag_t tag = Tag_val(dst);
  assert(tag == Tag_val(src));
  if (tag == Double_array_tag) {
    src_start_idx *= Double_wosize;
    dst_start_idx *= Double_wosize;
    size *= Double_wosize;
  }
  memcpy((void *)((value *)dst + dst_start_idx), (void *)((value *)src + src_start_idx), size * sizeof(value));
  return Val_unit;
}

value caml_obj_dup(value arg)
{
  mlsize_t size = Wosize_val(arg);
  if (size == 0)
    return arg;
  tag_t tag = Tag_val(arg);
  value res = alloc_via_malloc(size, tag);
  memcpy(Bp_val(res), Bp_val(arg), size * sizeof(value));
  return res;
}

value caml_lessthan(value lhs, value rhs)
{
  tag_t lhs_tag = Tag_hd(Hd_val(lhs));
  tag_t rhs_tag = Tag_hd(Hd_val(rhs));
  assert(lhs_tag == rhs_tag
      && "(runtime) caml_lessthan: Tags of arguments must be the same!");
  if (lhs_tag == Double_tag)
    return (*(double *)lhs < *(double *)rhs) * 2 + 1;
  assert(0 && "(runtime) caml_lessthan: Implemented yet only for float values");
  return (value)0;
}

value caml_print_endline(value string) /* string -> unit */
{
  printf("%s\n", (char const *)string);
  fflush(stdout);
  return Val_unit;
}

value caml_fill_string(value s, value offset, value len, value init)
{
  memset(&Byte(s, Long_val(offset)), Int_val(init), Long_val(len));
  return Val_unit;
}

value caml_print_string(value string) /* string -> unit */
{
  printf("%s", (char const *)string);
  return Val_unit;
}

value caml_read_float(value unit_value) /* unit -> float */
{
  double d = 0.0;
  scanf("%lf", &d);
  return alloc_float(d);
}

value caml_print_float(value block) /* float -> unit */
{
  printf("%g", *(double *)block);
  return Val_unit;
}

value caml_read_int(value unit_value) /* unit -> int */
{
  long n = 0;
  scanf("%ld", &n);
  return (value)(2 * n + 1);
}

value caml_int_of_string(value s) /* string -> int */
{
  long n = 0;
  sscanf((char const *)s, "%ld", &n);
  return (value)(2 * n + 1);
}

value caml_print_int(value word) /* int -> unit */
{
  long const n = (long)word >> 1;
  printf("%ld", n);
  return Val_unit;
}

value caml_print_char(value word) /* char -> unit */
{
  int const c = (long)word >> 1;
  putchar(c);
  return Val_unit;
}

value caml_create_string(mlsize_t len) /* int -> string */
{
  len >>= 1;
  mlsize_t wosize = (len + sizeof(value)) / sizeof(value);
  value result = alloc_via_malloc(wosize, String_tag);
  Field(result, wosize - 1) = 0;
  mlsize_t offset_index = Bsize_wsize(wosize) - 1;
  Byte(result, offset_index) = offset_index - len;
  return result;
}

value caml_read_file(value filename) /* string -> string */
{
  FILE *in = fopen((char const *)filename, "r");
  if (!in) {
    fprintf(stderr, "Can not open file %s\n", (char const *)filename);
    exit(1);
  }
  fseek(in, 0, SEEK_END);
  size_t const size = ftell(in);
  fseek(in, 0, SEEK_SET);
  value result = caml_create_string(size * 2 + 1);
  fread((char *)result, 1, size, in);
  fclose(in);
  return result;
}

value caml_read_short_string() /* unit -> string */
{
  static char buf[SHORT_STRING_LENGTH];
  scanf("%s", buf);
  value result = caml_create_string(strlen(buf));
  strcpy((char *)result, buf);
  return result;
}

value caml_blit_string(value s1, value ofs1, value s2, value ofs2, value n) /* string -> int -> string -> int -> int -> unit */
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}

static void eh_fatal_error(_Unwind_Reason_Code reason)
{
  int const exit_code = 1;
  fprintf(stderr, "Exception handling fatal error with reason code = %d => exit(%d)\n"
      , reason, exit_code);
  exit(exit_code);
}

void uncaught_exception(value exception_value)
{
  int const exit_code = 2;
  fprintf(stderr, "%s was called => exit(%d)\n", __func__, exit_code);
  exit(exit_code);
}

struct Exception
{
  struct _Unwind_Exception uw_header;
  value exception_value;
  uint64_t landing_pad;
};

static void exception_cleanup(_Unwind_Reason_Code reason, struct _Unwind_Exception *exc)
{
  // do nothing for static allocated exception
}

struct Exception active_exception = {
  { 0x4a424c424f434d4c  // exception class: vendor = 'JBLB', language = 'OCML'
  , exception_cleanup
  , 0x0  // private_1
  , 0x0  // private_2
  }
  , Val_unit
  , 0x0  // landing pad cache
};

// DWARF Constants
enum {
  DW_EH_PE_absptr   = 0x00,
  DW_EH_PE_uleb128  = 0x01,
  DW_EH_PE_udata2   = 0x02,
  DW_EH_PE_udata4   = 0x03,
  DW_EH_PE_udata8   = 0x04,
  DW_EH_PE_sleb128  = 0x09,
  DW_EH_PE_sdata2   = 0x0A,
  DW_EH_PE_sdata4   = 0x0B,
  DW_EH_PE_sdata8   = 0x0C,
  DW_EH_PE_pcrel    = 0x10,
  DW_EH_PE_textrel  = 0x20,
  DW_EH_PE_datarel  = 0x30,
  DW_EH_PE_funcrel  = 0x40,
  DW_EH_PE_aligned  = 0x50,
  DW_EH_PE_indirect = 0x80,
  DW_EH_PE_omit     = 0xFF
};

/// Read a uleb128 encoded value and advance pointer
/// See Variable Length Data Appendix C in:
/// @link http://dwarfstd.org/Dwarf4.pdf @unlink
/// @param data reference variable holding memory pointer to decode from
/// @returns decoded value
static uint64_t read_uleb128(uint8_t const **data)
{
  uint64_t result = 0;
  uint64_t shift = 0;
  unsigned char byte;
  uint8_t const *p = *data;
  do {
    byte = *p++;
    result |= (uintptr_t)(byte & 0x7F) << shift;
    shift += 7;
  } while (byte & 0x80);
  *data = p;
  return result;
}

/// Read a sleb128 encoded value and advance pointer
/// See Variable Length Data Appendix C in:
/// @link http://dwarfstd.org/Dwarf4.pdf @unlink
/// @param data reference variable holding memory pointer to decode from
/// @returns decoded value
static int64_t read_sleb128(uint8_t const **data)
{
  uint64_t result = 0;
  uint64_t shift = 0;
  unsigned char byte;
  uint8_t const *p = *data;
  do {
    byte = *p++;
    result |= (uint64_t)(byte & 0x7F) << shift;
    shift += 7;
  } while (byte & 0x80);
  *data = p;
  if ((byte & 0x40) && (shift < (sizeof(result) << 3)))
    result |= (uint64_t)(~0) << shift;
  return (int64_t)(result);
}

static void skip_leb128(uint8_t const **data)
{
  unsigned char byte;
  uint8_t const *p = *data;
  do {
    byte = *p++;
  } while (byte & 0x80);
  *data = p;
}

/// Read a pointer encoded value and advance pointer
/// See Variable Length Data in:
/// @link http://dwarfstd.org/Dwarf3.pdf @unlink
/// @param data reference variable holding memory pointer to decode from
/// @param encoding dwarf encoding type
/// @returns decoded value
static uint64_t read_encoded_pointer(uint8_t const **data, uint8_t encoding)
{
  uint64_t result = 0;
  if (encoding == DW_EH_PE_omit)
      return result;
  uint8_t const *p = *data;
  // first get value
  switch (encoding & 0x0F)
  {
  case DW_EH_PE_absptr:
    result = *((uint64_t *)p);
    p += sizeof(uint64_t);
    break;
  case DW_EH_PE_uleb128:
    result = read_uleb128(&p);
    break;
  case DW_EH_PE_sleb128:
    result = (uint64_t)(read_sleb128(&p));
    break;
  case DW_EH_PE_udata2:
    result = *((uint16_t *)p);
    p += sizeof(uint16_t);
    break;
  case DW_EH_PE_udata4:
    result = *((uint32_t *)p);
    p += sizeof(uint32_t);
    break;
  case DW_EH_PE_udata8:
    result = *((uint64_t *)p);
    p += sizeof(uint64_t);
    break;
  case DW_EH_PE_sdata2:
    result = (uint64_t)(*((int16_t *)p));
    p += sizeof(int16_t);
    break;
  case DW_EH_PE_sdata4:
    result = (uint64_t)(*((int32_t *)p));
    p += sizeof(int32_t);
    break;
  case DW_EH_PE_sdata8:
    result = (uint64_t)(*((int64_t *)p));
    p += sizeof(int64_t);
    break;
  default:
    // not supported
    exit(4);
    break;
  }
  // then add relative offset
  switch (encoding & 0x70)
  {
  case DW_EH_PE_absptr:
    // do nothing
    break;
  case DW_EH_PE_pcrel:
    if (result)
      result += (int64_t)(*data);
    break;
  case DW_EH_PE_textrel:
  case DW_EH_PE_datarel:
  case DW_EH_PE_funcrel:
  case DW_EH_PE_aligned:
  default:
    // not supported
    exit(8);
    break;
  }
  // then apply indirection
  if (result && (encoding & DW_EH_PE_indirect))
    result = *((uint64_t *)result);
  *data = p;
  return result;
}

static uint64_t extract_landing_pad_from_lsda(uint8_t const *lsda, struct _Unwind_Context *context)
{
  // Get the current instruction pointer and offset it before next
  // instruction in the current frame which threw the exception.
  uint64_t ip = _Unwind_GetIP(context) - 1;
  // Get beginning current frame's code (as defined by the
  // emitted dwarf code)
  uint64_t func_start = _Unwind_GetRegionStart(context);
  uint64_t ip_offset = ip - func_start;
  log("    actual call site ip = %p, ip offset = %llu\n", (void *)ip, (unsigned long long)ip_offset);

  // Parse LSDA header.
  uint8_t lpad_start_encoding = *lsda++;
  uint8_t const *lpad_start = (uint8_t const *)read_encoded_pointer(&lsda, lpad_start_encoding);
  if (lpad_start == 0)
    lpad_start = (uint8_t const *)func_start;
  if (*lsda++ != DW_EH_PE_omit)
    skip_leb128(&lsda);
  uint8_t call_site_encoding = *lsda++;
  uint32_t call_site_table_length = (uint32_t)(read_uleb128(&lsda));
  uint8_t const *call_site_table_start = lsda;
  uint8_t const *call_site_table_end = call_site_table_start + call_site_table_length;

  // Parse call site table
  log("    appropriate call site searching...\n");
  uint8_t const *call_site_ptr = call_site_table_start;
  while (call_site_ptr < call_site_table_end) {
    // There is one entry per call site.
    // The call sites are non-overlapping in [start, start+length)
    // The call sites are ordered in increasing value of start
    uint64_t start = read_encoded_pointer(&call_site_ptr, call_site_encoding);
    uint64_t length = read_encoded_pointer(&call_site_ptr, call_site_encoding);
    uint64_t landing_pad = read_encoded_pointer(&call_site_ptr, call_site_encoding);
    skip_leb128(&call_site_ptr);
    log(" .  call site: ip <- [%llu, %llu), landing pad = %llu\n"
        , (unsigned long long)start, (unsigned long long)(start + length), (unsigned long long)landing_pad);
    if ((start <= ip_offset) && (ip_offset < (start + length))) {
      // Found the call site containing ip.
      if (landing_pad == 0)
        return 0;
      return (uint64_t)lpad_start + landing_pad;
    }
  }
  return 0;
}

_Unwind_Reason_Code caml_personality(int version, _Unwind_Action actions, uint64_t exception_class,
    struct _Unwind_Exception *exception_object, struct _Unwind_Context *context)
{
  struct Exception *exception = (struct Exception *)exception_object;
  value exception_value = exception->exception_value;
  log("%s was called during %s phase with exception_value = %lu, actions = %x\n"
      , __func__, actions & _UA_CLEANUP_PHASE ? "cleanup" : "search", exception_value, actions);

  if (actions & _UA_HANDLER_FRAME) {
    log("--- it is the handler frame! Try to pass control to the landing pad\n");
    _Unwind_SetGR(context, 0 /* rax */, (uint64_t)exception_value);
    _Unwind_SetIP(context, exception->landing_pad);
    return _URC_INSTALL_CONTEXT;
  }
  if (actions & _UA_CLEANUP_PHASE)
    return _URC_CONTINUE_UNWIND;

  uint8_t const *lsda = (uint8_t const *)_Unwind_GetLanguageSpecificData(context);
  log("... and lsda address = %p\n", lsda);
  if (lsda == NULL)
    return _URC_CONTINUE_UNWIND;

  uint64_t landing_pad = extract_landing_pad_from_lsda(lsda, context);
  log("    landing pad address = %p\n", (void *)landing_pad);
  if (landing_pad == 0)
    return _URC_CONTINUE_UNWIND;

  log("--- landing pad was found! Go to cleanup phase\n");
  exception->landing_pad = landing_pad;
  return _URC_HANDLER_FOUND;
}

void caml_raise(value exception_value)
{
  log("%s was called with exception_value = %lu\n", __func__, exception_value);
  active_exception.exception_value = exception_value;
  _Unwind_Reason_Code unexpected_return_code =
      _Unwind_RaiseException((struct _Unwind_Exception *)&active_exception);
  if (unexpected_return_code == _URC_END_OF_STACK)
    uncaught_exception(exception_value);
  else
    eh_fatal_error(unexpected_return_code);
}
