SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

# expect files to be linked in from ../Makefile
all: stage1 
	$(MAKE) fs_spec_lib.cma fs_spec_lib.cmxa # so deps are there

FORCE:

# # we expect to be in TMP, not in src
# run_link: 
# 	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src
# 	ln -s ../src/{*cppo,*lem,*mli,*ml,*.sed} .
# 	touch $@

run_cppo: $(wildcard *.lem_cppo) 
	(for f in *.lem_cppo; do $$cppo $$CPPO_ARGS $$f -o $${f/.lem_cppo/.lem}; done)
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

run_lem: run_cppo $(wildcard *.lem)
	$$lem -lib $$LEMLIB $$LEMFLAGS -ocaml $(dot_lem)
	sed -i.bak -f patch_lem.sed t_fs_spec.ml
	touch $@

# now we are sure lem accepts the files, copy them to plain ml
run_cp: run_lem 
	for f in t_*.ml; do cp $$f $${f/t_/}; sed -i.bak -f patch_gen_ml.sed $${f/t_/}; done
	touch $@

# now we have various stages of ocaml compilation

stage1: run_cp
	touch $@

include .depend
%.x: %.mli %.ml stage1
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml stage1
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

fs_spec_lib.cma: $(xs)
	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)

fs_spec_lib.cmxa: $(xs)
	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)

clean_ocaml: FORCE
	rm -f *.cmi *.cmo *.cmx *.o *.x *.cma *.cmxa *.a a.out

clean: FORCE
	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src!
	-rm -rf $$(comm -13 <(cd ../src && ls -1 | sort) <(ls -1 |sort) ) # comm may not exist on all systems

.SUFFIXES:
