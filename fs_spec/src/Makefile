SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

all: stage1 # do this first, so $(xs) expands correctly
	$(MAKE) fs_spec_lib.cma fs_spec_lib.cmxa

FORCE:

# we expect to be in TMP, not in src
run_link: 
	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src
	ln -s ../src/{*cppo,*lem,*mli,*ml,*.sed} .
	touch $@

# expect link to have completed in an earlier invocation of make, so wildcard ok
run_cppo: $(wildcard *.lem *.lem_cppo) 
	(for f in *.lem_cppo; do	$$cppo $$CPPO_ARGS $$f -o $${f/.lem_cppo/.lem}; done)
	touch $@

# we have to invoke lem with the files in the right order

dot_lem:=\
t_list_array.lem \
t_fs_prelude.lem \
  t_fs_spec_properties.lem \
t_fs_spec.lem \
  t_fs_spec_fs_command_properties.lem \
  t_fs_spec_properties_perms.lem \
t_dir_heap.lem

run_lem: run_cppo $(dot_lem)
	$$lem -lib $$LEMLIB $$LEMFLAGS -ocaml $(dot_lem)
	sed -i.bak -f patch_lem.sed t_fs_spec.ml
	touch $@

# now we are sure lem accepts the files, copy them to plain ml
run_cp: run_lem $(wildcard t_*.ml) 
	for f in t_*.ml; do \
	  cp $$f $${f/t_/}; \
	  sed -i.bak -f patch_gen_ml.sed $${f/t_/}; \
	done ;
	touch $@

# now we have various stages of ocaml compilation

stage1: run_cp
	touch $@

include .depend
%.x: %.mli %.ml stage1
	@echo "* ocamlc/opt $*.mli $*.ml"
	@./ocamlc.sh -c $*.mli $*.ml
	@touch $@

%.x: %.ml stage1
	@echo "* ocamlc/opt $*.ml"
	@./ocamlc.sh -c $*.ml
	@touch $@

fs_spec_lib.cma: $(xs)
	(if [ "$$BUILD_BYTE" != "false" ]; then ocamlfind ocamlc -g -a -o $@ $(xs:.x=.cmo); fi)

fs_spec_lib.cmxa: $(xs)
	(if [ "$$BUILD_NATIVE" != "false" ]; then ocamlfind ocamlopt -g -a -o $@ $(xs:.x=.cmx); fi)

clean_ocaml: FORCE
	rm -f *.cmi *.cmo *.cmx *.o *.x *.cma *.cmxa *.a a.out

clean: FORCE
	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src!
	-rm -rf $$(comm -13 <(cd ../src && ls -1 | sort) <(ls -1 |sort) ) # comm may not exist on all systems
