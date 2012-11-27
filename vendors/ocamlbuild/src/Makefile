byte-only: ocamlbuild_pack.cmo ocamlbuild.byte ocamlbuildlib.cma
native-only: ocamlbuild_pack.cmx ocamlbuild.opt ocamlbuildlib.cmxa
install: installbyte
	if test -f ocamlopt.opt; then $(MAKE) -f Makefile.SRCDIR installopt
mixed-boot:
lib-native-only:

installbyte:
	cp -f $(BINDIR)/ocamlc.opt$(EXE)

SRCDIR=ocamlbuild

PACK_MLIS=  signatures.mli
PACK_MLS= \
 discard_printf.ml \
my_std.ml my_unix.ml tags.ml \
display.ml log.ml shell.ml \
bool.ml glob_ast.ml glob_lexer.ml \
glob.ml lexers.ml param_tags.ml \
command.ml ocamlbuild_Myocamlbuild_config.ml \
ocamlbuild_where.ml slurp.ml \
options.ml pathname.ml \
configuration.ml flags.ml \
hygiene.ml digest_cache.ml \
resource.ml rule.ml solver.ml \
report.ml tools.ml fda.ml \
findlib.ml ocaml_arch.ml \
ocaml_utils.ml ocaml_dependencies.ml \
ocaml_compiler.ml ocaml_tools.ml \
ocaml_specific.ml plugin.ml \
exit_codes.ml hooks.ml main.ml

PACK_CMIS= $(PACK_MLIS:.mli=.cmi)
PACK_CMOS= $(PACK_MLS:.ml=.cmo)
PACK_CMXS=$(PACK_MLS:.ml=.cmx)

TMP_SRCS=glob_lexer.ml lexers.ml ocamlbuild_Myocamlbuild_config.ml

ocamlbuild_Myocamlbuild_config.ml: myocamlbuild_config.ml
	cp -f myocamlbuild_config.ml ocamlbuild_Myocamlbuild_config.ml

ocamlbuild_pack.cmo: $(PACK_CMOS) $(PACK_CMIS)
	$(CAMLC) -pack -g $(PACK_CMIS) $(PACK_CMOS) -o ocamlbuild_pack.cmo

ocamlbuild_pack.cmx: $(PACK_CMOS) $(PACK_CMIS)
	$(CAMLOPT) $(COMPFLAGS) -g -for-pack Ocamlbuild_pack $(PACK_MLS)
	$(CAMLOPT) $(COMPFLAGS) -pack -g $(PACK_CMIS) $(PACK_CMXS) -o ocamlbuild_pack.cmx

ocamlbuild.byte: ocamlbuild_pack.cmo ocamlbuild_executor.cmo ocamlbuild_unix_plugin.cmo ocamlbuild.cmo
	$(CAMLC) unix.cma -g -I stdlib -I otherlibs/unix ocamlbuild_pack.cmo ocamlbuild_executor.cmo ocamlbuild_unix_plugin.cmo ocamlbuild.cmo -o ocamlbuild.byte

ocamlbuild.opt: ocamlbuild_pack.cmx ocamlbuild_executor.cmx ocamlbuild_unix_plugin.cmx ocamlbuild.cmx
	$(CAMLOPT) unix.cmxa -g -I stdlib -I otherlibs/unix ocamlbuild_pack.cmx ocamlbuild_executor.cmx ocamlbuild_unix_plugin.cmx ocamlbuild.cmx -o ocamlbuild.opt

ocamlbuild_plugin.cmo: ocamlbuild_plugin.ml
	$(CAMLC) $(COMPFLAGS) -c -rectypes  ocamlbuild_plugin.ml
ocamlbuild_plugin.cmi: ocamlbuild_plugin.mli
	$(CAMLC) $(COMPFLAGS) -c -rectypes  ocamlbuild_plugin.mli
ocamlbuild_plugin.cmx: ocamlbuild_plugin.ml
	$(CAMLOPT) $(COMPFLAGS) -c -rectypes ocamlbuild_plugin.ml

ocamlbuild_unix_plugin.cmx: ocamlbuild_unix_plugin.ml
	$(CAMLOPT) $(COMPFLAGS) -c -rectypes ocamlbuild_unix_plugin.ml

LIB_CMOS=ocamlbuild_pack.cmo ocamlbuild_plugin.cmo ocamlbuild_executor.cmo ocamlbuild_unix_plugin.cmo
LIB_CMXS=$(LIB_CMOS:.cmo=.cmx)

ocamlbuildlib.cma: $(LIB_CMOS)
	$(CAMLC) -a -linkall -I stdlib $(LIB_CMOS) -o ocamlbuildlib.cma
ocamlbuildlib.cmxa: $(LIB_CMXS)
	$(CAMLOPT) -a -linkall -I stdlib $(LIB_CMXS) -o ocamlbuildlib.cmxa

include Makefile.rules
include ../Makefile.config
