SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

NATIVES:=debug/check2.native debug/debug.native paths/testpath.native fs_test_check fs_test_posix fs_test run_trace test_generation/tgen 

######################################## target: all
all: include # do this first
	$(MAKE) lib/fs_check_lib.cmxa $(NATIVES)
	$(MAKE) -C ld_preload

######################################## include - copy the .cm... files from fs_spec, and cmis from lem

# no dep tracking: if anything changes in fs_spec you have to make clean
include: 
	mkdir -p include
	cp $$from_lem $$from_spec include
	chmod u+w include/* # in case we need to overwrite, and they are readonly from nix
	touch include

######################################## lib/fs_check_lib.cmxa

lib/fs_check_lib.cmxa:
	$(MAKE) -C lib 

######################################## targets depending on fs_check_lib.cmxa 

# FIXME could be tidier if we used consistent naming

debug/check2.native debug/debug.native: lib/fs_check_lib.cmxa
	$(MAKE) -C debug/

paths/testpath.native: %.native: %.ml lib/fs_check_lib.cmxa
	$$ocamlopt -linkpkg -o $@ $<

%: %.ml lib/fs_check_lib.cmxa
	$$ocamlopt -linkpkg -o $@ $<

fs_test_check fs_test_posix: fs_test_%: %.ml lib/fs_check_lib.cmxa
	$$ocamlopt -linkpkg -o $@ $<


######################################## clean
clean_natives:
	rm -f $(NATIVES)

clean_not_lib:
	rm -f $(NATIVES) 
	rm -rf include
	-$(MAKE) -C debug clean
	-$(MAKE) -C ld_preload clean
	rm -f paths/testpath.cmi paths/testpath.cmx paths/testpath.o paths/testpath.native
	rm -f *.cmi *.cmx *.cmo *.o
	rm -f test_generation/*.cmi test_generation/*.cmx test_generation/*.cmo test_generation/*.o 
	rm -f ./result

clean: clean_not_lib
	-$(MAKE) -C lib clean

FORCE:


-include Makefile.local

# Local variables:
# mode: outline-minor
# outline-regexp: "[#][#]+ .*"
# End:
