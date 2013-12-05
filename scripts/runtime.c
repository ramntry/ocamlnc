#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <caml/mlvalues.h>

#define Make_header(wosize, tag, color) ((wosize) << 10 | (color) | (tag))

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

