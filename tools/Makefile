
all:
	$(MAKE) -f Makefile.cmt2annot
	$(MAKE) -f Makefile.cmt2ml
	$(MAKE) -f Makefile.cmt2info

depend:
	rm -f .depend
	$(MAKE) -f Makefile.cmt2annot depend
	$(MAKE) -f Makefile.cmt2ml depend
	$(MAKE) -f Makefile.cmt2info depend

clean:
	rm -f *.o *.cm? *.cm?? *~ *.byte *.opt *.cmt *.cmti *.annot
