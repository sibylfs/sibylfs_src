#S HELL:=bash

include ../../config.sh

BISECT?= # or "bisect" to build with bisect
# OCAMLDEP:=ocamlfind ocamldep -package sexplib,sexplib.syntax -syntax camlp4o 
CAMLC:=ocamlfind ocamlc -package sha -package sexplib,sexplib.syntax,$(BISECT) -syntax camlp4o
CAMLOPT:=ocamlfind ocamlopt -package sha -package sexplib,sexplib.syntax,$(BISECT) -syntax camlp4o
WARNINGS:=-w -26-8
COMPFLAGS:=$(WARNINGS) 

# use a local lem if it exists, otherwise default to path
LEM:=$(LEMPATH)/lem
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

DOTMLI:=$(wildcard *.mli)

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


# Lem build process
#
# t_xxx.lem_cppo
#  |  generated_lem
#  \/
# t_xxx.lem ---------------+-----------------------+
#  |  generated_tml       |  lem_hol              |  lem_isa
#  \/                      \/                      \/
# t_xxx.ml                 t_xxxScript.sml         T_xxx.thy
#  |  generated_ml
#  \/
# xxx.ml
#  |
#  \/
# 
# Other build commands: lem_hol_copy, lem_isa_copy


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

compile: $(CMO) fs_spec_lib.cma fs_spec_lib.cmxa #lem_latex
.PHONY: compile

cmo: $(CMO)
.PHONY: cmo

# -g flag turns on debugging info (e.g. for stacktrace)
fs_spec_lib.cma: $(CMO)
	$(CAMLC) -g -a -o $@ $(CMO)

fs_spec_lib.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -g -a -o $@ $(CMO:.cmo=.cmx)


########################################################################
# includes 

include Makefile.doc
# include Makefile.depend
include Makefile.cppo_to_lem
include Makefile.lem_to_tml
include Makefile.tml_to_ml
include Makefile.lem_non_ml
-include Makefile.local # for any extra local targets

########################################################################
# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


########################################################################
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

FORCE:
