CAMLOPT=../../ocamlopt
COMPFLAGS=-verbose

PLUGINS=plugin.so plugin2.so sub/plugin.so sub/plugin3.so plugin4.so mypack.so packed1.so packed1_client.so pack_client.so plugin_ref.so plugin_high_arity.so

all: $(PLUGINS) $(PLUGINS:.so=.cmx) main_rev main mylib.so

clean:
	rm -f *.so *.o *.cm* main
	rm -f sub/*.so sub/*.o sub/*.cm*

main: api.cmx main.cmx
	$(CAMLOPT) -o main dynlink.cmxa api.cmx main.cmx

main_rev: api.cmx main.cmx
	$(CAMLOPT) -o main_rev dynlink.cmxa main.cmx api.cmx

sub/plugin3.so: sub/api.cmi sub/api.cmx sub/plugin3.ml
	(cd sub; mv api.cmx api.cmx.bak; ../$(CAMLOPT) -c $(COMPFLAGS) plugin3.ml; mv api.cmx.bak api.cmx)

plugin2.so: api.cmx plugin.cmi plugin.cmx
	(mv plugin.cmx plugin.cmx.bak; $(CAMLOPT) -c $(COMPFLAGS) plugin2.ml; mv plugin.cmx.bak plugin.cmx)

sub/api.so: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.ml)

sub/api.cmi: sub/api.mli
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.mli)

sub/api.cmx: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.ml)

plugin.so: api.cmx plugin.cmi
sub/plugin.so: api.cmx
plugin4.so: api.cmx
main.so: api.cmx

packed1_client.cmx: packed1.cmx

pack_client.cmx: mypack.cmx

packed1.cmx: api.cmx packed1.ml
	$(CAMLOPT) -c $(COMPFLAGS) -for-pack Mypack packed1.ml

mypack.so: packed1.cmx
	$(CAMLOPT) -c $(COMPFLAGS) -shared -pack -o mypack.cmx packed1.cmx

mylib.so: plugin.cmx plugin2.cmx
	$(CAMLOPT) $(COMPFLAGS) -a -o mylib.cmxa -shared plugin.cmx plugin2.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .so .cmxa

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.so:
	$(CAMLOPT) -c -shared $(COMPFLAGS) $<

#.cmx.so:
#	gcc -shared -o $@ $(<:.cmx=.o)

#.cmxa.so:
#	gcc -shared -o $@ -Wl,-whole-archive $(<:.cmxa=.a) -Wl,-no-whole-archive
