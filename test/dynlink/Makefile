#INSPATH=C:/cygwin/home/frisch/ocaml3093
INSPATH=C:/cygwin/home/frisch/natdynlink_ins_mingw
OCAMLRUN=$(INSPATH)/bin/ocamlrun.exe
OCAMLC=$(OCAMLRUN) $(INSPATH)/bin/ocamlc
OCAMLMKLIB=$(OCAMLRUN) $(INSPATH)/bin/ocamlmklib

#O=obj
#A=lib
O=o
A=a

stub1.$(O): stub1.c
	$(OCAMLC) -c stub1.c

libplug1.$(A): stub1.$(O)
	$(OCAMLMKLIB) -o plug1 stub1.$(O)

plug1.cmo: plug1.ml
	$(OCAMLC) -c plug1.ml

plug1.cma: plug1.cmo libplug1.$(A)
	$(OCAMLMKLIB) -o plug1 plug1.cmo

stub2.$(O): stub2.c
	$(OCAMLC) -c stub2.c

libplug2.$(A): stub2.$(O)
	$(OCAMLMKLIB) -o plug2 stub2.$(O)

plug2.cmo: plug2.ml
	$(OCAMLC) -c plug2.ml

plug2.cma: plug2.cmo libplug2.$(A)
	$(OCAMLMKLIB) -a -o plug2 plug2.cmo

main.cmo: main.ml
	$(OCAMLC) -c main.ml

main.exe: main.cmo
	$(OCAMLC) -o main.exe dynlink.cma main.cmo

static.exe: plug1.cma plug2.cma
	$(OCAMLC) -o static.exe -linkall plug1.cma plug2.cma

custom.exe: plug2.cma plug1.cma
	$(OCAMLC) -o custom.exe -custom -linkall plug2.cma plug1.cma

demo: main.exe plug1.cma plug2.cma static.exe custom.exe
	@echo "********** Dynamic"
	$(OCAMLRUN) ./main.exe plug1.cma plug2.cma
	@echo "********** Static"
	$(OCAMLRUN) ./static.exe
	@echo "********** Custom"
	./custom.exe

clean:
	rm -f *.lib *.obj *.o *.a *.dll *.manifest *.exe *.cmo *.cmi *~ *.exp *.cma