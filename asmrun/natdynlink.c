#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"
#include "alloc.h"
#include "natdynlink.h"
#include "osdeps.h"

#include <stdio.h>
#include <string.h>

#if defined(_WIN32) || defined(__CYGWIN__)

#include <windows.h>

static char * winerror(void)
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

static void allow_write(char *begin, char *end) {
  static long int pagesize = 0;
  long int old;
  int res;
  SYSTEM_INFO si;

  if (0 == pagesize) {
    GetSystemInfo (&si);
    pagesize = si.dwPageSize;
  }

  begin -= (int) begin % pagesize;
  res = VirtualProtect(begin, end - begin, PAGE_EXECUTE_WRITECOPY, &old);
  if (0 == res) {
    fprintf(stderr, "natdynlink: VirtualProtect failed  %s\n", winerror());
    exit(2);
  }
}

#include "exports.h"
#include "prims.h"

static void *staticsym(char *name) { 
  int i;

  /* +5 :  jump over the _imp_ prefix */
  for (i = 0; caml_names_of_export_symbols[i] != NULL; i++)
    if (strcmp(name, caml_names_of_export_symbols[i]+5) == 0)
      return caml_export_symbols[i];

  /* +1 : jump over the _ prefix */
  for (i = 0; caml_names_of_builtin_cprim[i] != NULL; i++)
    if (strcmp(name+1, caml_names_of_builtin_cprim[i]) == 0)
      return caml_builtin_cprim[i];

  return NULL; 
}

#else

#include <dlfcn.h>

static void *staticsym(char *name) {
  static void *glb = NULL;
  if (NULL == glb) {
    glb = dlopen(NULL, RTLD_NOW);
    if (NULL == glb) {
      printf("Cannot get global dl handle\n");
      exit(2);
    }
  }

  return dlsym(glb, name);
}

#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>

static void allow_write(void *begin, void *end) {
  static long int pagesize = 0;

  if (0 == pagesize) {
    errno = 0;
    pagesize = sysconf(_SC_PAGESIZE);
    if (errno){
      fprintf(stderr, "natdynlink: cannot get pagesize\n"); 
      exit(2); 
    }
  }

  begin -= (int) begin % pagesize;
  int res = mprotect(begin, end - begin, PROT_WRITE | PROT_EXEC | PROT_READ);
  if (0 != res) { 
    fprintf(stderr, "natdynlink: mprotect failed %s\n", strerror(errno)); 
    exit(2); 
  }
}
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
  sym = caml_dlsym (handle, fullname);
  if (NULL == sym && !opt) {
    printf("natdynlink: cannot find symbol %s\n", fullname);
    exit(2);
  }
  /* printf("%s => %lx\n", fullname, (uintnat) sym); */
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
    reloctable += strlen(name) + 1;

    s = caml_dynsym(name);
    if (NULL == (void*) s) s = (intnat) staticsym(name);

    if (NULL == (void*) s) {
      printf("Cannot resolve %s\n", name);
      exit(2);
      /* TODO: exception */
    }
    /* printf("%s -> %lx\n", name, (intnat)s); */

    while (NULL != (void*) (reloc = (*((intnat*)reloctable)))) {
      /* uintnat c = (*((unsigned char*)(reloc) - 1)); */
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

void caml_init_dynunits() {
  int i;
  for (i = 0; NULL != caml_dynunits[i].reloctable; i++) {
    allow_write(caml_dynunits[i].code_begin,caml_dynunits[i].code_end);
    caml_relocate(caml_dynunits[i].reloctable);
  }
}

CAMLprim value caml_natdynlink_open
(value private, value filename, value symbols)
{
  CAMLparam3 (private, filename, symbols);
  void *sym,*sym2;
  void *handle;

  handle =
    caml_dlopen(String_val(filename), 1);
  /*
	   (private == Val_true
	    ? RTLD_NOW
	    : RTLD_NOW | RTLD_GLOBAL
	    ));
  */
  
  if (NULL == handle)
    CAMLreturn(caml_copy_string(caml_dlerror()));

#define optsym(n) getsym(handle,unit,n,1)
  while (symbols != Val_unit) {
    char *unit;
    void (*entrypoint)(void);

    unit = String_val(Field(symbols,0));

    symbols = Field(symbols,1);

    sym = optsym("__frametable");
    if (NULL != sym) caml_register_frametable(sym);

    sym = optsym("");
    if (NULL != sym) caml_register_dyn_global(sym);

    sym = optsym("__data_begin");
    sym2 = optsym("__data_end");
    if (NULL != sym && NULL != sym2)
      caml_dyn_data_segments = segment_cons(sym,sym2,caml_dyn_data_segments); 

    sym = optsym("__code_begin");
    sym2 = optsym("__code_end");
    if (NULL != sym && NULL != sym2) allow_write(sym,sym2);

    sym = optsym("__symtable");
    if (NULL != sym) caml_register_symtable(sym);

    sym = optsym("__reloctable");
    if (NULL != sym) caml_relocate(sym);

    entrypoint = optsym("__entry");
    if (NULL != entrypoint) caml_callback((value)(&entrypoint), 0);
  }
#undef sym
#undef optsym

  CAMLreturn (Val_unit);
}
