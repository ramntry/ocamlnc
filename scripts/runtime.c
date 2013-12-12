#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <caml/mlvalues.h>

#define Make_header(wosize, tag, color) ((wosize) << 10 | (color) | (tag))

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
  value *block = (value *)malloc(allocation_size);
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

value caml_blit_string(value s1, value ofs1, value s2, value ofs2, value n) /* string -> int -> string -> int -> int -> unit */
{
  memmove(&Byte(s2, Long_val(ofs2)), &Byte(s1, Long_val(ofs1)), Int_val(n));
  return Val_unit;
}

