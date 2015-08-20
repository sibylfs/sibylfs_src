include Makefile.common

FS:=$(shell cat .depend.linear)

MLS:=$(filter-out %.mli,$(FS))

#$(info MLS=$(MLS)!  FS=$(FS)!)

WARNINGS=-w @f@p@u@s@40
COMPFLAGS=$(WARNINGS) -g

LIB:=fs_check_lib

all: 
	$(MAKE) before_all
	$(MAKE) srcs
	$(MAKE) cmo cmx
	$(MAKE) $(LIB).cma $(LIB).cmxa syscall_stubs.o

srcs: FORCE
	$(MAKE) -f Makefile.srcs srcs

before_all:
	$(MAKE) -C .. include

depend: srcs
	$(MAKE) -f Makefile.depend


########################################################################
cmo: $(MLS:.ml=.cmo)
	touch cmo

cmx: $(MLS:.ml=.cmx)
	touch cmx

$(LIB).cma: cmo
	$(CAMLC) -g -a -o $@ $(MLS:.ml=.cmo)

$(LIB).cmxa: cmx
	$(CAMLOPT) -g -a -o $@ $(MLS:.ml=.cmx)

# those that don't depend on syscall_stubs.c
TOP_LEVEL_MLS:=diff.ml fs_path.ml posix_agent.ml stat.ml dump.ml \
  checklib_shared_types.ml fs_ast.ml fs_lr_parser.ml fs_lexer.ml trace.ml checkLib.ml

top_level_lib.cma: $(TOP_LEVEL_MLS:.ml=.cmo)
	$(CAMLC) -g -a -o $@ $(TOP_LEVEL_MLS:.ml=.cmo)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .o .c

CFLAGS:=-I $(shell ocamlc -where) -fPIC -Wall -Wextra -Werror -std=c99

%.o: %.c
	cc $(CFLAGS) -o $*.o -c $*.c

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<

clean:
	rm -f ./*.cmi ./*.cmx ./*.cmo ./*.o ./*.a ./*.cmxa ./*.cma
	rm -f ./fs_lexer.ml ./fs_lr_parser.mli ./fs_lr_parser.ml
	rm -f cmo cmx srcs
	rm -f depend.dot depend.pdf
	rm -f fs_test_version.ml
	rm -rf ocamldoc

include Makefile.doc
-include .depend
-include Makefile.local # for any local additions to Makefile

FORCE:
