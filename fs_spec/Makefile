SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=config.sh
export BASH_ENV

TMP:=_build

all: 
	mkdir -p $(TMP)
	cp Makefile config.sh $(TMP)
	$(MAKE) -C $(TMP) link
	$(MAKE) -C $(TMP) fs_spec_lib.cma fs_spec_lib.cmxa

FORCE:

link: 
	ln -sf ../src/* .
	touch $@

run_cppo: link $(wildcard *.lem *.lem_cppo) 
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

fs1:=lem_support.mli lem_support.ml \
	abstract_string.mli abstract_string.ml \
	fs_prelude.mli fs_prelude.ml \
	list_array.mli list_array.ml \
	fs_dict_wrappers.mli fs_dict_wrappers.ml
s1: run_cp $(fs1)
	$$ocamlc -c $(fs1)
	$$ocamlopt -c $(fs1)
	touch $@

s2: s1 fs_spec.ml
	$$ocamlc -c fs_spec.ml
	$$ocamlopt -c fs_spec.ml
	touch $@

fs3:=fs_printer.ml \
	dir_heap.ml \
	fs_dump.ml \
	fs_interface.mli fs_interface.ml
s3: s2 $(fs3)
	$$ocamlc -c $(fs3)
	$$ocamlopt -c $(fs3)
	touch $@

# need to get .cmo in right order
mls:=$(filter %.ml,$(fs1) $(fs2) $(fs3))
fs_spec_lib.cma: s3
	ocamlfind ocamlc -g -a -o $@ $(mls:.ml=.cmo)

fs_spec_lib.cmxa: s3
	ocamlfind ocamlopt -g -a -o $@ $(mls:.ml=.cmx)


clean: FORCE
	rm -f *.lem_cppo *.lem *.mli *.ml *.cmi *.cmo *.cmx 
	rm -rf $(TMP)
