include ../../config/Makefile

CAMLOPT=../../ocamlopt
COMPFLAGS=-verbose
SHARED=-shared -closed -S -dstartup
#SHARED=-shared

PLUGINS=plugin.so plugin2.so sub/plugin.so sub/plugin3.so plugin4.so mypack.so packed1.so packed1_client.so pack_client.so plugin_ref.so plugin_high_arity.so plugin_ext.so plugin_ext2.so plugin_simple.so bug.so

all: $(PLUGINS) $(PLUGINS:.so=.cmx) main main_ext mylib.so

clean:
	rm -f *.so *.o *.cm* main main_ext *.exe *.s *.asm *.obj
	rm -f sub/*.so sub/*.o sub/*.cm* sub/*.s sub/*.asm sub/*.obj
	rm -f plugin_ext2.ml

main: api.cmx main.cmx
	$(CAMLOPT) -o main -linkall dynlink.cmxa api.cmx main.cmx

main_ext: api.cmx main.cmx factorial.s.$(O)
	$(CAMLOPT) -o main_ext dynlink.cmxa api.cmx main.cmx factorial.s.$(O)

sub/plugin3.so: sub/api.cmi sub/api.cmx sub/plugin3.ml
	(cd sub; mv api.cmx api.cmx.bak; ../$(CAMLOPT) -c $(SHARED) $(COMPFLAGS) plugin3.ml; mv api.cmx.bak api.cmx)

plugin2.so: api.cmx plugin.cmi plugin.cmx
	(mv plugin.cmx plugin.cmx.bak; $(CAMLOPT) -c $(COMPFLAGS) $(SHARED) plugin2.ml; mv plugin.cmx.bak plugin.cmx)

sub/api.so: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) api.ml)

sub/api.cmi: sub/api.mli
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.mli)

sub/api.cmx: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.ml)

plugin.so: api.cmx plugin.cmi
sub/plugin.so: api.cmx
plugin4.so: api.cmx
main.so: api.cmx

plugin_ext.so: api.cmx plugin_ext.ml factorial.d.$(O)
	$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) factorial.d.$(O) plugin_ext.ml

plugin_ext2.so: api.cmx plugin_ext.ml
	cp plugin_ext.ml plugin_ext2.ml
	$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) plugin_ext2.ml

packed1_client.cmx: packed1.so

pack_client.cmx: mypack.so

packed1.so: api.cmx packed1.ml
	$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) -for-pack Mypack packed1.ml

mypack.so: packed1.so
	$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) -S -pack -o mypack.cmx packed1.cmx

mylib.so: plugin.so plugin2.so
	$(CAMLOPT) $(COMPFLAGS) $(SHARED) -a -o mylib.cmxa -shared plugin.cmx plugin2.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .so .cmxa

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.so:
	$(CAMLOPT) -c $(SHARED) $(COMPFLAGS) $<

factorial.d.$(O): factorial.c
	$(CAMLOPT) -c -ccopt "$(DLLCCCOMPOPTS)" factorial.c
	cp factorial.$(O) factorial.d.$(O)

factorial.s.$(O): factorial.c
	$(CAMLOPT) -c factorial.c
	cp factorial.$(O) factorial.s.$(O)
