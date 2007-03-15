include ../../config/Makefile

CAMLOPT=../../ocamlopt
COMPFLAGS= -closed
#SHARED=-shared -closed -S -dstartup
#SHARED=-shared

PLUGINS=plugin.so plugin2.so sub/plugin.so sub/plugin3.so plugin4.so mypack.so packed1.so packed1_client.so pack_client.so plugin_ref.so plugin_high_arity.so plugin_ext.so plugin_simple.so bug.so

all: $(PLUGINS) $(PLUGINS:.so=.cmx) main mylib.so

clean:
	rm -f *.so *.o *.cm* main main_ext *.exe *.s *.asm *.obj
	rm -f sub/*.so sub/*.o sub/*.cm* sub/*.s sub/*.asm sub/*.obj

main: api.cmx main.cmx
	$(CAMLOPT) -o main -linkall dynlink.cmxa api.cmx main.cmx

main_ext: api.cmx main.cmx factorial.$(O)
	$(CAMLOPT) -o main_ext dynlink.cmxa api.cmx main.cmx factorial.$(O)

sub/plugin3.cmx: sub/api.cmi sub/api.cmx sub/plugin3.ml
	(cd sub; mv api.cmx api.cmx.bak; ../$(CAMLOPT) -c $(COMPFLAGS) plugin3.ml; mv api.cmx.bak api.cmx)

plugin2.cmx: api.cmx plugin.cmi plugin.cmx
	(mv plugin.cmx plugin.cmx.bak; $(CAMLOPT) -c $(COMPFLAGS) plugin2.ml; mv plugin.cmx.bak plugin.cmx)

sub/api.so: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) $(SHARED) api.ml)

sub/api.cmi: sub/api.mli
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.mli)

sub/api.cmx: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.ml)

plugin.cmx: api.cmx plugin.cmi
sub/plugin.cmx: api.cmx
plugin4.cmx: api.cmx
main.cmx: api.cmx
plugin_ext.cmx: api.cmx plugin_ext.ml
	$(CAMLOPT) -c $(COMPFLAGS) plugin_ext.ml

plugin_ext.so: factorial.$(O) plugin_ext.cmx
	$(CAMLOPT) -shared -o plugin_ext.so factorial.$(O) plugin_ext.cmx

packed1_client.cmx: packed1.cmx

pack_client.cmx: mypack.cmx

packed1.cmx: api.cmx packed1.ml
	$(CAMLOPT) -c $(COMPFLAGS) -for-pack Mypack packed1.ml

mypack.cmx: packed1.cmx
	$(CAMLOPT) -c $(COMPFLAGS) -S -pack -o mypack.cmx packed1.cmx

mylib.cmxa: plugin.cmx plugin2.cmx
	$(CAMLOPT) $(COMPFLAGS) -a -o mylib.cmxa plugin.cmx plugin2.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .so .cmxa

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.so:
	$(CAMLOPT) -o $@ -shared $(COMPFLAGS) $<

.cmxa.so:
	$(CAMLOPT) -o $@ -shared -linkall $(COMPFLAGS) $<

factorial.$(O): factorial.c
	$(CAMLOPT) -c factorial.c

