#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"
#include "alloc.h"

#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>

#define RTLD_NOW 0
#define RTLD_GLOBAL 0

static void * dlopen(char * libname, int flags)
{
  HMODULE m;
  /* flags currently ignored */
  m = LoadLibraryEx(libname, NULL, 0);
  if (m == NULL) m = LoadLibrary(libname);
  return (void *) m;
}

static void dlclose(void * handle)
{
  FreeLibrary((HMODULE) handle);
}

static void * dlsym(void * handle, char * name)
{
  return (void *) GetProcAddress((HMODULE) handle, name);
}

static char * dlerror(void)
{
  static char dlerror_buffer[256];
  DWORD msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,         /* message source */
                  GetLastError(), /* error number */
                  0,            /* default language */
                  dlerror_buffer, /* destination */
                  sizeof(dlerror_buffer), /* size of destination */
                  NULL);         /* no inserts */
  if (msglen == 0)
    return "unknown error";
  else
    return dlerror_buffer;
}
#else
#include <dlfcn.h>
#endif




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

static segment *segment_cons(void *begin, void *end, segment *tl) {
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


/* Symbols used for dynamic resolution */


static void *getsym(void *handle, char *module, char *name, int opt){
  char *fullname = malloc(strlen(module) + strlen(name) + 5);
  void *sym;
  sprintf(fullname, "caml%s%s", module, name);
  sym = dlsym (handle, fullname);
  if (NULL == sym && !opt) {
    printf("natdynlink: cannot find symbol %s\n", fullname);
    exit(2);
  }
  free(fullname);
  return sym;
}

extern intnat caml_dynsym(const char *name);

static void caml_relocate(char *reloctable) {
  intnat reloc;
  char *name;
  intnat s;
  uintnat absolute;

  if (*(char*)(reloctable)) 
    return;  /* Already relocated */

  *(char*)(reloctable) = 1;
  reloctable++;

  while (0 != *(name = reloctable)) {
    /*    printf("Symbol %s\n", name);  */
    reloctable += strlen(name) + 1;

    s = caml_dynsym(name);
    if (NULL == (void*) s) {
      printf("Cannot resolve %s\n", name);
      exit(2);
      /* TODO: exception */
    }
    /* printf("-> %lx\n", (intnat)s);  */

    while (NULL != (void*) (reloc = (*((intnat*)reloctable)))) {
      /*      uintnat c = (*((unsigned char*)(reloc) - 1)); */
      reloctable += sizeof(intnat);
      absolute = *((unsigned char*)(reloctable));
      reloctable++;
      /*
      printf("Reloc %lx (%lx) [%lx: %lx -> %lx]\n", reloc, 
	     absolute,
	     c,
	     *(intnat*)reloc,
	     (intnat) (s - reloc ));
      */
      if (absolute) {
	*((intnat*)reloc) += s;
      } else {
	*((intnat*)reloc) = s - reloc - 4;
      }
    }
    reloctable += sizeof(intnat);
  }
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
  void *sym,*sym2;
  void *handle =
    dlopen(String_val(filename),
	   (private == Val_true
	    ? RTLD_NOW
	    : RTLD_NOW | RTLD_GLOBAL
	    ));
  
  if (NULL == handle)
    CAMLreturn(caml_copy_string(dlerror()));

#define optsym(n) getsym(handle,unit,n,1)
  while (symbols != Val_unit) {
    char *unit = String_val(Field(symbols,0));
    void (*entrypoint)(void);

    symbols = Field(symbols,1);

    sym = optsym("__frametable");
    if (NULL != sym) caml_register_frametable(sym);

    sym = optsym("");
    if (NULL != sym) caml_register_dyn_global(sym);

    sym = optsym("__data_begin");
    sym2 = optsym("__data_end");
    if (NULL != sym && NULL != sym2)
      caml_dyn_data_segments = segment_cons(sym,sym2,caml_dyn_data_segments); 

    sym = optsym("__reloctable");
    if (NULL != sym) caml_relocate(sym);

    sym = optsym("__symtable");
    if (NULL != sym) caml_register_symtable(sym);

    entrypoint = optsym("__entry");
    if (NULL != entrypoint) caml_callback((value)(&entrypoint), 0);
  }
#undef sym
#undef optsym

  CAMLreturn (Val_unit);
}
