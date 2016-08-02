# build fs_spec/build and fs_test/lib first

-include ../../config.sh

OCAMLFIND:=ocamlfind
CAMLC:=$(OCAMLFIND) ocamlc
CAMLOPT:=$(OCAMLFIND) ocamlopt

EXTRACTCMA:=$(EXTRACTDIR)/extract.cma
EXTRACTCMXA:=$(EXTRACTDIR)/extract.cmxa

# allow overriding in case fs_spec is not sibling directory; override
# with absolute path (this var also appears in ../Makefile)
BUILD?=../../fs_spec/build
FS_SPEC_LIB_CMA=$(BUILD)/fs_spec_lib.cma
FS_SPEC_LIB_CMXA=$(BUILD)/fs_spec_lib.cmxa

LIB?=../lib

CAMLLIBS:=unix,bigarray,str,num,bytes

LIBS:= \
  -I $(EXTRACTDIR) extract.cmxa \
	-I $(BUILD) fs_spec_lib.cmxa \
	-I $(LIB) fs_check_lib.cmxa \
	../lib/syscall_stubs.o

PKGS:=$(CAMLLIBS),bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax

NATIVES:=check2.native debug.native

all: before_all $(NATIVES) 

before_all: $(EXTRACTCMA) $(EXTRACTCMXA) $(FS_SPEC_LIB_CMA) $(FS_SPEC_LIB_CMXA) $(LIB)/fs_check_lib.cmxa

depend:
	$(MAKE) -f Makefile.depend

check2.native: checklib_2.cmx check2.cmx
	$(OCAMLFIND) ocamlopt -linkpkg -o $@ -syntax camlp4o -package $(PKGS) $(LIBS) $(COMPFLAGS) $^

debug.native: checklib_2.cmx debug.cmx
	$(OCAMLFIND) ocamlopt -linkpkg -o $@ -syntax camlp4o -package $(PKGS) $(LIBS) $(COMPFLAGS) $^

-include .depend


########################################################################
CAMLCINCLUDES:= -package $(PKGS) $(LIBS:.cmxa=.cma)
CAMLCARGS:=$(COMPFLAGS) $(CAMLCINCLUDES) -syntax camlp4o
CAMLOPTARGS:=$(CAMLCARGS:.cma=.cmxa)


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCARGS) -c $<

.mli.cmi:
	$(CAMLOPT) $(CAMLOPTARGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(CAMLOPTARGS) -c $<


clean: 
	rm -f *.cmi *.cmx *.cmo *.o
	rm -f *.native 
