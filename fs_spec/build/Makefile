# WARNING! WARNING! WARNING!
# This file is generated from Makefile.org using emacs, org, babel, tangle
# WARNING! WARNING! WARNING!

# http://stackoverflow.com/questions/20389064/using-gnu-make-functions-to-check-if-variables-are-defined
check-var-defined = $(if $(1),,$(error $(1) is not defined))

-include ../../config.sh # we may be using env vars instead of a config.sh

$(call check-var-defined,SIBYLFS_CONFIG)
# ifndef SIBYLFS_CONFIG
# $(error SIBYLFS_CONFIG is not defined; check ../../config.sh)
# else
# endif

# do we want to be very cautious? let's just check LEMPATH as well
# ifndef LEM
# $(error)
# else
# endif

$(call check-var-defined,LEM)
$(call check-var-defined,LEMLIB)
$(call check-var-defined,EXTRACTDIR)
$(call check-var-defined,CPPO)

BISECT?= # or "-package bisect" to build with bisect
OCAMLC:=ocamlfind ocamlc $(BISECT)
OCAMLOPT:=ocamlfind ocamlopt $(BISECT)
WARNINGS:=-w -26-8
# COMPFLAGS:=$(WARNINGS) -package sexplib,sexplib.syntax # might as well

# LEM:= from env
LEMFLAGS:=-lib $(LEMLIB) -only_changed_output -wl_unused_vars ign -wl_rename err
EXTRACTCMA:=$(EXTRACTDIR)/extract.cma
EXTRACTCMXA:=$(EXTRACTDIR)/extract.cmxa

OCAMLC_ARGS:=-I $(EXTRACTDIR) extract.cma -package sexplib,sexplib.syntax,sha $(WARNINGS) -c 
OCAMLOPT_ARGS:=$(OCAMLC_ARGS:.cma=.cmxa)

DOTCPPO:=\
t_fs_spec.lem_cppo \
t_fs_spec_fs_command_properties.lem_cppo \
t_fs_spec_properties_perms.lem_cppo \
t_dir_heap.lem_cppo


GENERATED_LEM:=$(patsubst %.lem_cppo,%.lem,$(DOTCPPO))

DOTLEM:=\
t_list_array.lem \
t_fs_prelude.lem \
t_fs_spec_properties.lem \
$(GENERATED_LEM)

DOTMLI:=abstract_string.mli \
fs_dict_wrappers.mli        \
fs_interface.mli            \
fs_prelude.mli              \
lem_support.mli             \
list_array.mli

GENERATED_TML:=$(patsubst %.lem,%.ml,$(DOTLEM)) 

GENERATED_ML:=$(patsubst t_%.ml,%.ml,$(GENERATED_TML))

DOTML:=\
abstract_string.ml      \
lem_support.ml          \
fs_prelude.ml           \
list_array.ml           \
fs_spec.ml              \
dir_heap.ml             \
fs_dict_wrappers.ml     \
fs_printer.ml           \
fs_dump.ml              \
fs_interface.ml         


CMO:=$(DOTML:.ml=.cmo)

# this makefile is phased: the following phases must be run in order;
# it should be possible to type: `make before_all`, and then to make
# any of the subsequent targets e.g. `make compile`
all: 
	$(MAKE) before_all 
	$(MAKE) generated_lem
	$(MAKE) generated_tml
	$(MAKE) generated_ml
	$(MAKE) compile
	$(MAKE) generated.tar # in case I forget to update it

# need these present before we do anything
before_all: $(EXTRACTCMA) $(EXTRACTCMXA) link
.PHONY: before_all

link: 
	ln -sf ../src/* .
	touch $@

# currently switch manually between the versions 
# TODO: add some more comfort
# -D xxx include something
# -U xxx exclude something
# -n don't output source annotations
# by default all aspects are present, if you want to turn them off,
# use command line argument 'CPPO_ARGS="xxxx"'

ifndef CPPO_ARGS
CPPO_ARGS:=-D aspect_perms
endif

$(GENERATED_LEM): %.lem: %.lem_cppo
	if [ -f $*.lem ]; then chmod ugo+w $*.lem; fi
	$(CPPO) $(CPPO_ARGS) $*.lem_cppo -o $*.lem
	chmod ugo-w $*.lem

# don't use this in rules - just for triggering build of generated lem files
generated_lem: $(GENERATED_LEM)
	touch $@

run_lem: $(DOTLEM)
	echo building for $@
	$(LEM) $(LEMFLAGS) -ocaml $(DOTLEM)
	cp t_fs_spec.ml t_fs_spec.ml.tmp
	sed -f patch_lem.sed t_fs_spec.ml.tmp > t_fs_spec.ml
	rm t_fs_spec.ml.tmp
	touch $@

# make the t_ml files depend on actually running lem; lem processes all files at once
$(GENERATED_TML): run_lem

generated_tml: $(GENERATED_TML)
	touch $@

# the following sets up a dependency known to make, between t_*.ml and *.ml
$(GENERATED_ML): %.ml: t_%.ml
	if [ -f $@ ]; then chmod ugo+w $@; fi
	cp $< $@.tmp
	sed -f patch_gen_ml.sed $@.tmp >$@
	rm -f $@.tmp
	chmod ugo-w $@

# don't use this in rules - just for triggering build of generated ml files
generated_ml: $(GENERATED_ML)
	touch $@

DEPS_abstract_string=
DEPS_lem_support=
DEPS_list_array=abstract_string.cmi
DEPS_fs_prelude=lem_support.cmi
DEPS_fs_dict_wrappers=fs_prelude.cmi lem_support.cmi
DEPS_fs_spec=list_array.cmi abstract_string.cmi fs_prelude.cmi lem_support.cmi
DEPS_dir_heap=fs_spec.cmi list_array.cmi fs_prelude.cmi lem_support.cmi
DEPS_fs_dump=dir_heap.cmi list_array.cmi fs_spec.cmi fs_dict_wrappers.cmi
DEPS_fs_printer=list_array.cmi fs_spec.cmi fs_dict_wrappers.cmi lem_support.cmi
DEPS_fs_interface=fs_dump.cmi dir_heap.cmi fs_printer.cmi fs_spec.cmi abstract_string.cmi lem_support.cmi

# add camlp4o only for those .ml files (and .mli interfaces) that need it
camlp4o=-syntax camlp4o
EXTRA_abstract_string.ml=$(camlp4o)
EXTRA_fs_spec.ml=$(camlp4o)
EXTRA_dir_heap.ml=$(camlp4o)
EXTRA_fs_dump.ml=$(camlp4o)
EXTRA_fs_printer.ml=$(camlp4o)
EXTRA_fs_interface.ml=$(camlp4o)

# cm[ox] depend on cmi; had to move these to associated rules
# %.cmo: %.cmi
# %.cmx: %.cmi

# make invocations of commands slightly more succinct by exporting to
# bash env

export OCAMLC_ARGS export OCAMLOPT_ARGS

# cmo depends on ml 
%.cmo: %.ml %.cmi
	$(OCAMLC) $$OCAMLC_ARGS $(EXTRA_$*.ml) $*.ml

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $$OCAMLOPT_ARGS $(EXTRA_$*.ml) $*.ml

# cmi depends on mli and dependent cmi; this makefile syntax not working
%.cmi: %.mli # FIXME won't work $(DEPS_$*)
	$(OCAMLOPT) $$OCAMLOPT_ARGS $(EXTRA_$*.ml) $*.mli

# an alternative to $(DEPS_$*); ugly; FIXME
list_array.cmi: $(DEPS_list_array)
fs_prelude.cmi: $(DEPS_fs_prelude)
fs_dict_wrappers.cmi: $(DEPS_fs_dict_wrappers)
fs_spec.cmi: $(DEPS_fs_spec)
dir_heap.cmi: $(DEPS_dir_heap)
fs_dump.cmi: $(DEPS_fs_dump)
fs_printer.cmi: $(DEPS_fs_printer)
fs_interface.cmi: $(DEPS_fs_interface)

# mli files we need to create (others exist already)
dir_heap.mli fs_dump.mli fs_printer.mli fs_spec.mli: %.mli: %.ml # won't work $(DEPS_$*)
	$(OCAMLC) $$OCAMLC_ARGS $(EXTRA_$*.ml) -i $*.ml > $*.mli

# ugly FIXME
fs_spec.mli: $(DEPS_fs_spec)
dir_heap.mli: $(DEPS_dir_heap)
fs_dump.mli: $(DEPS_fs_dump)
fs_printer.mli: $(DEPS_fs_printer)

compile: $(CMO:.cmo=.cmi) $(CMO) $(CMO:.cmo=.cmx) fs_spec_lib.cma fs_spec_lib.cmxa

# -g flag turns on debugging info (e.g. for stacktrace)
fs_spec_lib.cma: $(CMO)
	$(OCAMLC) -g -a -o $@ $(CMO)

fs_spec_lib.cmxa: $(CMO:.cmo=.cmx)
	$(OCAMLOPT) -g -a -o $@ $(CMO:.cmo=.cmx)

interface_only: generated_ml
	$(OCAMLC) $$OCAMLC_ARGS -syntax camlp4o -c abstract_string.mli -c lem_support.mli fs_interface.mli
	touch $@

define generated_files

t_fs_spec_fs_command_properties.lem
t_dir_heap.lem
t_fs_spec.lem
t_fs_spec_properties_perms.lem

t_dir_heap.ml
t_fs_prelude.ml
t_fs_spec_fs_command_properties.ml
t_fs_spec.ml
t_fs_spec_properties.ml
t_fs_spec_properties_perms.ml
t_list_arrayAuxiliary.ml
t_list_array.ml

dir_heap.ml
fs_prelude.ml
fs_spec_fs_command_properties.ml
fs_spec.ml
fs_spec_properties.ml
fs_spec_properties_perms.ml
list_array.ml

endef

generated.tar: generated_ml
	$(file >files_in_tar,$(generated_files))
	tar cvf $@  --files-from files_in_tar

generated: FORCE
	tar --touch -xvf generated.tar

# FIXME tidy this
# NB don't delete generated.tar - this is part of the "source" and should be upto date
clean: FORCE
	rm -f $(GENERATED_LEM) $(GENERATED_TML) $(GENERATED_ML)
	rm -f *.cm[iox] *.a *.o *.cma *.cmxa dir_heap.mli fs_dump.mli fs_printer.mli fs_spec.mli
	rm -f link generated_lem run_lem generated_tml generated_ml cmo cmx interface_only files_in_tar
	find . -maxdepth 1 -type l -exec rm \{\} \;	
	rm -f depend.{dot,pdf} .depend.min
	rm -f $(patsubst %.lem, %Auxiliary.lem, $(DOTLEM))
	rm -f $(patsubst %.lem, %Auxiliary.ml, $(DOTLEM))
	rm -f $(patsubst %.lem, %Auxiliary.byte, $(DOTLEM))
	rm -f $(patsubst %.lem, %.html, $(DOTLEM))
	rm -f $(patsubst %.lem, %ml.md, $(DOTLEM))
	rm -f $(patsubst %.lem, %.md, $(DOTLEM))
	rm -f *Script.sml
	rm -f a.out
	rm -rf ocamldoc ocamldoc* depend.svg fs_spec.html fs_spec.ml.md # t_fs_spec.lem_cppo.{html,md}
	rm -f *.tmp

FORCE:
