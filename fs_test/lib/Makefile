SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

######################################## all, srcs, before_all
all: before_all srcs
	$(MAKE) fs_check_lib.cma fs_check_lib.cmxa syscall_stubs.o


srcs: # FIXME depends?
	$(MAKE) -f Makefile.srcs srcs

before_all:
	$(MAKE) -C .. include


######################################## compile

compile: $(xs)

include .depend

%.x: %.mli %.ml srcs
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml srcs
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

fs_check_lib.cma: $(xs)
	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)

fs_check_lib.cmxa: $(xs)
	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)


.SUFFIXES: .o .c

CFLAGS:=-I $(shell ocamlc -where) -fPIC -Wall -Wextra -Werror -std=c99

%.o: %.c
	cc $(CFLAGS) -o $*.o -c $*.c

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.x *.cma *.cmxa *.a a.out
	rm -f *.x
	rm -f fs_lexer.ml fs_lr_parser.mli fs_lr_parser.ml
	rm -f fs_test_version.ml
	rm -f srcs
	rm -rf ocamldoc

-include Makefile.local

FORCE:

## local vars
# Local variables:
# mode: outline-minor
# outline-regexp: "[#][#]+ .*"
# End:
