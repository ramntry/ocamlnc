#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include <stdio.h>

value stub1() {
  printf("This is stub1!\n");
  return Val_unit;
}
