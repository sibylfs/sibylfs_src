OCAMLFIND:=ocamlfind
OCAMLOPT:=$(OCAMLFIND) ocamlopt

#BISECTDIR=$(shell $(OCAMLFIND) -query bisect)
#WARNINGS=-w @f@p@u@s@40YZ+20+4
WARNINGS=-w @f@p@u@s@40
COMPFLAGS=$(WARNINGS) -g


LEM:=$(shell command -v ../src_ext/lem/lem || command -v lem)
EXTRACTDIR:=$(shell dirname $(LEM))/ocaml-lib/_build
EXTRACTCMA:=$(EXTRACTDIR)/extract.cma
EXTRACTCMXA:=$(EXTRACTDIR)/extract.cmxa


# allow overriding in case fs_spec is not sibling directory
BUILD?=../fs_spec/build
FS_SPEC_LIB_CMA=$(BUILD)/fs_spec_lib.cma
FS_SPEC_LIB_CMXA=$(BUILD)/fs_spec_lib.cmxa


CAMLLIBS=unix,bigarray,str,num,bytes
CMIS=$(EXTRACTDIR)/*.cmi \
     $(BUILD)/lem_support.cmi \
     $(BUILD)/abstract_string.cmi \
     $(BUILD)/fs_interface.cmi \
     $(BUILD)/fs_dict_wrappers.cmi \
     $(BUILD)/fs_prelude.cmi 


LIBS:= \
  -I $(EXTRACTDIR) extract.cmxa \
	-I $(BUILD) fs_spec_lib.cmxa \
	-I lib fs_check_lib.cmxa \
	lib/syscall_stubs.o

BISECT?= # set this to "bisect" to build with bisect code coverage
PKGS:=$(CAMLLIBS),bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,$(BISECT)


SUBDIRS:= ld_preload


NATIVES=fs_test_check \
  fs_test_posix \
  debug/check2.native \
	debug/debug.native \
	paths/testpath.native
BYTES= #testgen.byte #testpath.byte
EXECS=fs_test run_trace tgen 


all:
	$(MAKE) beforeall
	$(MAKE) $(NATIVES) $(BYTES) $(EXECS) subdirs

beforeall: $(EXTRACTCMA) $(EXTRACTCMXA) $(FS_SPEC_LIB_CMA) $(FS_SPEC_LIB_CMXA) lib/fs_check_lib.cmxa

subdirs: $(SUBDIRS)
.PHONY: $(SUBDIRS)

$(SUBDIRS): 
	$(MAKE) -C $@

include: $(CMIS)
	mkdir -p include
	cp $(CMIS) include

lib/fs_check_lib.cmxa: FORCE
	$(MAKE) -C lib

debug/check2.native debug/debug.native: lib/fs_check_lib.cmxa
	$(MAKE) -C debug

OPTARGS:=-linkpkg -syntax camlp4o -package $(PKGS) $(LIBS) $(COMPFLAGS)

fs_test_check: check.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

fs_test_posix: posix.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

run_trace: run_trace.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

fs_test: fs_test.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

tgen: test_generation/tgen.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

paths/testpath.native: paths/testpath.ml 
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

clean_not_lib:
	rm -rf include
	rm -f *.cmi *.cmx *.cmo *.o
	rm -f $(NATIVES) $(BYTES) $(EXECS)
	$(MAKE) -C debug clean
	rm -f test_generation/*.cmi test_generation/*.cmx test_generation/*.cmo test_generation/*.o 
	rm -f paths/testpath.cmi paths/testpath.cmx paths/testpath.o paths/testpath.native
	for f in $(SUBDIRS); do ($(MAKE) -C $$f clean); done

clean: clean_not_lib
	$(MAKE) -C lib clean

FORCE:


-include Makefile.local
