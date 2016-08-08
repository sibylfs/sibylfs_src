# build fs_spec/build and fs_test/lib first
SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

NATIVES:=check2.native debug.native

all: before_all $(NATIVES) 

before_all: ../lib/fs_check_lib.cmxa

# FIXME do we want to do this in .. ? then we can include all the libs
check2.native: checklib_2.x check2.x
	$$mk_native -linkpkg -o $@ checklib_2.cmx check2.cmx

debug.native: checklib_2.x debug.x
	$$mk_native -linkpkg -o $@ checklib_2.cmx debug.cmx

include .depend

%.x: %.mli %.ml 
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml 
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@


clean: 
	rm -f *.cmi *.cmx *.cmo *.o
	rm -f *.x
	rm -f *.native 
