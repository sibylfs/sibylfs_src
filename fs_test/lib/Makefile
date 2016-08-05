SHELL:=/bin/bash

# bash will read this config file first
BASH_ENV=bash_env.sh
export BASH_ENV

xs:=fs_test_version.x posix_agent.x syscall.x fs_test_config.x fs_test_system.x fs_test_mount.x fs_path.x fs_test_cli.x diff.x fs_test_index.x dump.x checklib_shared_types.x posix_ops.x fs_ast.x fs_lr_parser.x fs_lexer.x trace.x fs_test_util.x stat.x checkLib.x checkLib_with_posix.x fs_test_html.x

LIB:=fs_check_lib

######################################## all, srcs, before_all
all: before_all srcs
	$(MAKE) $(LIB).cma $(LIB).cmxa syscall_stubs.o


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

$(LIB).cma: $(xs)
	ocamlfind ocamlc -g -a -o $@ $(xs:.x=.cmo)

$(LIB).cmxa: $(xs)
	ocamlfind ocamlopt -g -a -o $@ $(xs:.x=.cmx)


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

# include Makefile.doc
-include Makefile.local # for any local additions to Makefile

FORCE:



## local vars
# Local variables:
# mode: outline-minor
# outline-regexp: "[#][#]+ .*"
# End:
