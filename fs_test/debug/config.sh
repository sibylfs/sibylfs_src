set -a # export all vars

# FIXME we should include fs_check_lib when we know it has been built
  ocamlc="ocamlfind ocamlc   $WARN $CCFLAGS -I ../include extract.cma  fs_spec_lib.cma  -I ../lib fs_check_lib.cma $PKGS"
ocamlopt="ocamlfind ocamlopt $WARN $CCFLAGS -I ../include extract.cmxa fs_spec_lib.cmxa -I ../lib fs_check_lib.cmxa $PKGS"
