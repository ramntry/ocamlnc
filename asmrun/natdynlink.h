typedef struct dynunit {
  char *reloctable;
  void *code_begin;
  void *code_end;
} dynunit;

extern dynunit caml_dynunits[];

void caml_init_dynunits();
