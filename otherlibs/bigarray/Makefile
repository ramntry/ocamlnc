#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################

# $Id$

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -g -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib -I ../unix
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib -I ../unix

C_OBJS=bigarray_stubs.o mmap_unix.o

CAML_OBJS=bigarray.cmo

all: libbigarray.a libbigarray.$(SO) bigarray.cma

allopt: libbigarray.a libbigarray.$(SO) bigarray.cmxa

libbigarray.a: $(C_OBJS)
	rm -f libbigarray.a
	ar rc libbigarray.a $(C_OBJS)
	$(RANLIB) libbigarray.a

libbigarray.so: $(C_OBJS)
	$(MKSHAREDLIB) libbigarray.so $(C_OBJS)

bigarray.cma: $(CAML_OBJS)
	$(CAMLC) -a -linkall $(CUSTOM_IF_NOT_SHARED) -o bigarray.cma \
           $(CAML_OBJS) -cclib -lbigarray

bigarray.cmxa: $(CAML_OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o bigarray.cmxa \
           $(CAML_OBJS:.cmo=.cmx) -cclib -lbigarray

install:
	cp bigarray.cmi bigarray.mli libbigarray.a bigarray.cma $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) libbigarray.a
	if test -f libbigarray.so; then cp libbigarray.so $(LIBDIR); fi
	cp bigarray.h $(LIBDIR)/caml/bigarray.h

installopt:
	cp bigarray.a $(CAML_OBJS:.cmo=.cmx) bigarray.cmxa $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) bigarray.a

partialclean:
	rm -f *.cm* 

clean: partialclean
	rm -f libbigarray.* *.o bigarray.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM -I../../byterun -I../unix *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
