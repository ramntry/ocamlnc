#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"

value factorial(value n){
  CAMLparam1(n);
  CAMLlocal2(a,b);

  int m = Int_val(n);
  int x = 1;
  int i;
  for (i = 1; i <= m; i++) x *= i;
  a = copy_string("abc");
  CAMLreturn (Val_int(x));
}
