SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=config.sh
export BASH_ENV

all: stage1 # do this first, so $(xs) expands correctly
	$(MAKE) fs_spec_lib.cma fs_spec_lib.cmxa

FORCE:

# we expect to be in TMP, not in src
run_link: 
	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src
	ln -s ../src/{*cppo,*lem,*mli,*ml,*.sed} .
	touch $@

# expect link to have completed
run_cppo: $(wildcard *.lem *.lem_cppo) 
	for f in *.lem_cppo; do	$$cppo $$CPPO_ARGS $$f -o $${f/.lem_cppo/.lem}; done
	touch $@

# we have to invoke lem with the files in the right order

lem:=\
t_list_array.lem \
t_fs_prelude.lem \
t_fs_spec_properties.lem \
t_fs_spec.lem \
t_fs_spec_fs_command_properties.lem \
t_fs_spec_properties_perms.lem \
t_dir_heap.lem

run_lem: run_cppo $(lem)
	$$lem -lib $$LEMLIB $$LEMFLAGS -ocaml $(lem)
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
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml stage1
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

# need to get .cmo in right order
xs:=abstract_string.x lem_support.x list_array.x fs_prelude.x fs_spec.x dir_heap.x fs_dict_wrappers.x fs_dump.x fs_printer.x fs_interface.x

fs_spec_lib.cma: $(xs)
	ocamlfind ocamlc -g -a -o $@ $(xs:.x=.cmo)

fs_spec_lib.cmxa: $(xs)
	ocamlfind ocamlopt -g -a -o $@ $(xs:.x=.cmx)


# those things we can clean if we are in src
# clean_src:
# 	rm -f *.cmi *.cmo *.cmx *.o *.x *.cma *.cmxa *.a

clean: FORCE
	if [ `basename $(CURDIR)` = "src" ]; then false; fi # fail if in src!
	rm -f *.lem_cppo *.lem *.mli *.ml *.cmi *.cmo *.cmx *.o *.x *.cma *.cmxa *.a
	mv *.bak /tmp
	rm run* stage1