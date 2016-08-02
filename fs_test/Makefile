######################################## check env vars

-include ../config.sh # we may be using env vars instead of a config.sh
ifndef SIBYLFS_CONFIG
$(error SIBYLFS_CONFIG is not defined; check ../../config.sh)
else
endif

######################################## define variables

BISECT?= # set this to "bisect" to build with bisect code coverage

# allow overriding in case fs_spec is not sibling directory (which it
# isn't when nix-build in fs_test, since fs_spec is built in nix)
BUILD?=../fs_spec/build

OCAMLFIND:=ocamlfind
OCAMLOPT:=$(OCAMLFIND) ocamlopt

#BISECTDIR=$(shell $(OCAMLFIND) -query bisect)
#WARNINGS=-w @f@p@u@s@40YZ+20+4
WARNINGS=-w @f@p@u@s@40
COMPFLAGS=$(WARNINGS) -g

CAMLLIBS=unix,bigarray,str,num,bytes
PKGS:=$(CAMLLIBS),bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,$(BISECT)

LIBS:= \
  -I $(EXTRACTDIR) extract.cmxa \
	-I $(BUILD) fs_spec_lib.cmxa \
	-I lib fs_check_lib.cmxa \
	lib/syscall_stubs.o


######################################## define variables corresponding to file(s)

EXTRACTCMA:=$(EXTRACTDIR)/extract.cma
EXTRACTCMXA:=$(EXTRACTDIR)/extract.cmxa

# FS_SPEC_LIB_CMA:=$(BUILD)/fs_spec_lib.cma
# FS_SPEC_LIB_CMXA:=$(BUILD)/fs_spec_lib.cmxa

# cmis we need from elsewhere
CMIS:=$(EXTRACTDIR)/*.cmi \
     $(BUILD)/lem_support.cmi \
     $(BUILD)/abstract_string.cmi \
     $(BUILD)/fs_interface.cmi \
     $(BUILD)/fs_dict_wrappers.cmi \
     $(BUILD)/fs_prelude.cmi 

NATIVES:=\
debug/check2.native debug/debug.native paths/testpath.native \
fs_test_check fs_test_posix \
fs_test run_trace tgen
# BYTES= #testgen.byte #testpath.byte
# EXECS=fs_test run_trace tgen # FIXME why not in natives?


######################################## target: all
all:
	$(MAKE) beforeall
	$(MAKE) lib/fs_check_lib.cmxa 
	$(MAKE) $(NATIVES) 
	$(MAKE) -C ld_preload

## beforeall
# make sure these are all here
beforeall: $(EXTRACTCMA) $(EXTRACTCMXA) $(BUILD)/fs_spec_lib.cma $(BUILD)/fs_spec_lib.cmxa

## include - copy the .cm... files from fs_spec, and cmis from lem

# note that we don't pick up changes in these fs_spec (some of these
# dependencies), so you have to make clean; make if anything changes
# in fs_spec
include: $(CMIS) $(BUILD)/fs_spec_lib.cma $(BUILD)/fs_spec_lib.cmxa
	mkdir -p include
	cp $(CMIS) include
	cp $(BUILD)/fs_spec_lib.cma $(BUILD)/fs_spec_lib.cmxa include
	chmod u+w include/* # in case we need to overwrite, and they are readonly from nix
	touch include

## make -C lib/ (for lib/fs_check_lib.cmxa)
lib/fs_check_lib.cmxa: include FORCE
	$(MAKE) -C lib/

## targets depending on fs_check_lib.cmxa
## make -C debug/
debug/check2.native debug/debug.native: lib/fs_check_lib.cmxa
	$(MAKE) -C debug/

OPTARGS:=-linkpkg -syntax camlp4o -package $(PKGS) $(LIBS) $(COMPFLAGS)

## paths/testpath.native tgen
paths/testpath.native: paths/testpath.ml 
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

tgen: test_generation/tgen.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<


## other executables: fs_test_check fs_test_posix run_trace fs_test tgen
fs_test_check: check.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

fs_test_posix: posix.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

run_trace: run_trace.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<

fs_test: fs_test.ml
	$(OCAMLOPT) -o $@ $(OPTARGS) $<


######################################## clean
clean_not_lib:
	rm -rf include
	$(MAKE) -C debug clean
	$(MAKE) -C ld_preload clean
	rm -f paths/testpath.cmi paths/testpath.cmx paths/testpath.o paths/testpath.native
	rm -f *.cmi *.cmx *.cmo *.o
	rm -f $(NATIVES) 
	rm -f test_generation/*.cmi test_generation/*.cmx test_generation/*.cmo test_generation/*.o 

clean: clean_not_lib
	$(MAKE) -C lib clean

FORCE:


-include Makefile.local

# Local variables:
# mode: outline-minor
# outline-regexp: "[#][#]+ .*"
# End:
