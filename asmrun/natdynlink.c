#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"
#include "alloc.h"

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

/* Data segments are used by the Is_atom predicate (mlvalues.h)
   to detect static Caml blocks.

   TODO: use dichotomic search 
*/

typedef struct segment {
  void *begin;
  void *end;
  struct segment *next;
} segment;

segment *caml_dyn_data_segments = NULL;

static segment *cons(void *begin, void *end, segment *tl) {
  segment *lnk = caml_stat_alloc(sizeof(segment));
  lnk->begin = begin;
  lnk->end = end;
  lnk->next = tl;
  return lnk;
}

int caml_is_in_data(void *p) {
  segment *lnk;
  for (lnk = caml_dyn_data_segments; NULL != lnk; lnk = lnk->next)
    if (p >= lnk->begin && p <= lnk->end) return 1;
  return 0;
}

static void *getsym(void *handle, char *module, char *name){
  char *fullname = malloc(strlen(module) + strlen(name) + 5);
  sprintf(fullname, "caml%s%s", module, name);
  void *sym = dlsym (handle, fullname);
  if (NULL == sym) {
    printf("natdynlink: cannot find symbol %s\n", fullname);
    exit(2);
  }
  free(fullname);
  return sym;
}

extern char caml_globals_map[];

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return (value)caml_globals_map;
}

CAMLprim value caml_natdynlink_globals_inited(value unit)
{
  return Val_int(caml_globals_inited);
}

CAMLprim value caml_natdynlink_open
(value private, value filename, value symbols)
{
  CAMLparam3 (private, filename, symbols);

  void *handle =
    dlopen(String_val(filename),
	   (private == Val_true
	    ? RTLD_NOW
	    : RTLD_NOW | RTLD_GLOBAL
	    ));
    
  if (NULL == handle)
    CAMLreturn(caml_copy_string(dlerror()));
  
#define sym(n) getsym(handle,unit,n)
  while (symbols != Val_unit) {
    char *unit = String_val(Field(symbols,0));
    symbols = Field(symbols,1);
    caml_register_frametable(sym("__frametable"));
    caml_register_dyn_global((value)sym(""));
    caml_dyn_data_segments = 
      cons(sym("__data_begin"),sym("__data_end"),caml_dyn_data_segments); 
    void (*entrypoint)(void) = sym("__entry");
    caml_callback((value)(&entrypoint), 0);
  }
#undef sym

  CAMLreturn (Val_unit);
}
