# FIXME we should probably make fs_test depend on fs_spec_lib.{cma,cmxa} rather than rebuilding everything

SUBDIRS:=fs_spec fs_test 

all: fs_spec fs_test 

FORCE:

.PHONY: $(SUBDIRS) dep

dep:
	./install_opam_deps.sh

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for f in $(SUBDIRS); do ($(MAKE) -C $$f clean); done


-include Makefile.local
