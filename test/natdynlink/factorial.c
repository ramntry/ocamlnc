#include "caml/mlvalues.h"

value factorial(value n){
  int m = Int_val(n);
  int x = 1;
  int i;
  for (i = 1; i <= m; i++) x *= i;
  return (Val_int(x));
}
