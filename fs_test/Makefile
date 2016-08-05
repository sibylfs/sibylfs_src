SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

define natives
debug/check2.native debug/debug.native 
paths/testpath.native 
fs_test_check fs_test_posix
fs_test run_trace tgen
endef
NATIVES:=$(shell echo $(natives))

######################################## target: all
all:
	$(MAKE) include
	$(MAKE) -C lib 
	$(MAKE) $(NATIVES) 
	$(MAKE) -C ld_preload

## include - copy the .cm... files from fs_spec, and cmis from lem

# no dep tracking: if anything changes in fs_spec you have to make clean
include: 
	mkdir -p include
	cp $$from_lem $$from_spec include
	chmod u+w include/* # in case we need to overwrite, and they are readonly from nix
	touch include


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
