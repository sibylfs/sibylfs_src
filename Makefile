# FIXME we should probably make fs_test depend on fs_spec_lib.{cma,cmxa} rather than rebuilding everything

SUBDIRS:=src_ext fs_spec/build fs_test 

all: src_ext fs_spec/build fs_test 

fs_test: fs_spec/build

.PHONY: $(SUBDIRS) dep

dep:
	./install_opam_deps.sh

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for f in $(SUBDIRS); do ($(MAKE) -C $$f clean); done


-include Makefile.local
