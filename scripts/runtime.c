#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <caml/mlvalues.h>

#define Make_header(wosize, tag, color) ((wosize) << 10 | (color) | (tag))

value const const_unit = 1;

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
  log("Unknown exception");
  exit(1);
  return const_unit;
}

static value alloc_float(double d)
{
  value block = caml_alloc_small(1, Double_tag);
  *(double *)block = d;
  return block;
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
  return const_unit;
}

value caml_print_string(value string) /* string -> unit */
{
  printf("%s", (char const *)string);
  return const_unit;
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
  return const_unit;
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
  return const_unit;
}
