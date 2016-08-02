include ../../config.sh
ifndef SIBYLFS_CONFIG
$(error SIBYLFS_CONFIG is not defined; check ../../config.sh)
else
endif

# do we want to be very cautious? let's just check LEMPATH as well
ifndef LEMPATH
$(error)
else
endif

BISECT?= # or "-package bisect" to build with bisect
CAMLC:=ocamlfind ocamlc $(BISECT)
CAMLOPT:=ocamlfind ocamlopt $(BISECT)
WARNINGS:=-w -26-8
COMPFLAGS:=$(WARNINGS) 

LEM:=$(LEMPATH)
LEMFLAGS:=-only_changed_output -wl_unused_vars ign -wl_rename err
EXTRACTCMA:=$(EXTRACTDIR)/extract.cma
EXTRACTCMXA:=$(EXTRACTDIR)/extract.cmxa

CAMLCINCLUDES:=-I $(EXTRACTDIR) extract.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

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

# need these present before we do anything
before_all: $(EXTRACTCMA) $(EXTRACTCMXA) link
.PHONY: before_all

link: 
	ln -sf ../src/* .
	touch link

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
	touch run_lem

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

compile: cmo cmx fs_spec_lib.cma fs_spec_lib.cmxa

cmo: generated_ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c abstract_string.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c lem_support.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_interface.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c list_array.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_prelude.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_spec.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c dir_heap.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_dict_wrappers.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -package sha -c fs_dump.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_printer.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_interface.mli
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c abstract_string.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c list_array.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c lem_support.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_prelude.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_dict_wrappers.ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -c fs_interface.ml
	touch cmo

cmx: generated_ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c abstract_string.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c lem_support.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_interface.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c list_array.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_prelude.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_spec.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c dir_heap.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_dict_wrappers.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -package sha -c fs_dump.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_printer.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c fs_interface.mli
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c abstract_string.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c list_array.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c lem_support.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_prelude.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_dict_wrappers.ml
	$(CAMLOPT) $(CAMLOPTINCLUDES) -package sexplib,sexplib.syntax -c fs_interface.ml
	touch cmx

# -g flag turns on debugging info (e.g. for stacktrace)
fs_spec_lib.cma: $(CMO)
	$(CAMLC) -g -a -o $@ $(CMO)

fs_spec_lib.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -g -a -o $@ $(CMO:.cmo=.cmx)

interface_only: generated_ml
	$(CAMLC) $(CAMLCINCLUDES) -package sexplib,sexplib.syntax -syntax camlp4o -c abstract_string.mli -c lem_support.mli fs_interface.mli
	touch $@

# FIXME tidy this
clean: FORCE
	rm -f $(GENERATED_LEM) $(GENERATED_TML) $(GENERATED_ML)
	rm -f *.cm[iox] *.a *.o
	rm -f fs_spec_lib.cma fs_spec_lib.cmxa
	rm -f generated_lem generated_tml generated_ml
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
	find . -maxdepth 1 -type l -exec rm \{\} \;
	rm -f run_lem link
	rm -f *.tmp
	rm interface_only cmo cmx

FORCE:
