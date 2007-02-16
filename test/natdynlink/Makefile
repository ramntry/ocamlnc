CAMLOPT=../../ocamlopt
COMPFLAGS=
FLAGSMAIN=-ccopt -Wl,-E

all: plugin.so plugin2.so sub/plugin.so sub/plugin3.so plugin4.so mypack.so packed1.so packed1_client.so pack_client.so main_rev main plugin_ref.so

clean:
	rm -f *.so *.o *.cm* main
	rm -f sub/*.so sub/*.o sub/*.cm*

main: api.cmx main.cmx
	$(CAMLOPT) -o main $(FLAGSMAIN) natdynlink.cmxa api.cmx main.cmx

main_rev: api.cmx main.cmx
	$(CAMLOPT) -o main_rev $(FLAGSMAIN) natdynlink.cmxa main.cmx api.cmx

sub/plugin3.cmx: sub/api.cmi sub/api.cmx sub/plugin3.ml
	(cd sub; mv api.cmx api.cmx.bak; ../$(CAMLOPT) -c $(COMPFLAGS) plugin3.ml; mv api.cmx.bak api.cmx)

plugin2.cmx: api.cmx plugin.cmi plugin.cmx
	(mv plugin.cmx plugin.cmx.bak; $(CAMLOPT) -c $(COMPFLAGS) plugin2.ml; mv plugin.cmx.bak plugin.cmx)

sub/api.cmx: sub/api.cmi sub/api.ml
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.ml)

sub/api.cmi: sub/api.mli
	(cd sub; ../$(CAMLOPT) -c $(COMPFLAGS) api.mli)

plugin.cmx: api.cmx plugin.cmi
sub/plugin.cmx: api.cmx
plugin4.cmx: api.cmx
main.cmx: api.cmx

packed1_client.cmx: packed1.cmx

pack_client.cmx: mypack.cmx

packed1.cmx: api.cmx packed1.ml
	$(CAMLOPT) -c $(COMPFLAGS) -for-pack Mypack packed1.ml

mypack.cmx: packed1.cmx
	$(CAMLOPT) -c $(COMPFLAGS) -pack -o mypack.cmx packed1.cmx


.SUFFIXES: .ml .mli .cmo .cmi .cmx .so

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.so:
	gcc -shared -o $@ $(<:.cmx=.o)