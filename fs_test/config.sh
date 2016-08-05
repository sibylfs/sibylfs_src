# export all vars
set -a 

# assume top-level config already sourced

# FIXME dangerous relative path
SPEC_BUILD="${SPEC_BUILD:-../fs_spec/_build}" # default value

# cmis from elsewhere
CMIS="$EXTRACTDIR/*.cmi 
$SPEC_BUILD/lem_support.cmi 
$SPEC_BUILD/abstract_string.cmi 
$SPEC_BUILD/fs_interface.cmi
$SPEC_BUILD/fs_dict_wrappers.cmi
$SPEC_BUILD/fs_prelude.cmi"


COREPKGS="unix,bigarray,str,num,bytes"
XTRAPKGS="bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,$BISECT"
PKGS="-package $COREPKGS,$XTRAPKGS -syntax camlp4o"
WARN="-w @f@p@u@s@40"
CCFLAGS="-g"
ocamlc="ocamlfind ocamlc $WARN $CCFLAGS -I $EXTRACTDIR extract.cma -I $PKGS"
ocamlopt="ocamlfind ocamlopt $WARN $CCFLAGS -I $EXTRACTDIR extract.cmxa -I $SPEC_BUILD fs_spec_lib.cmxa $PKGS"
# 	-I lib fs_check_lib.cmxa \
# 	lib/syscall_stubs.o

ocamldep="ocamlfind ocamldep $PKGS"

# 
# INCLUDE:=../include
# PKGS:=bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,unix-errno,ctypes
# CAMLC:=ocamlfind ocamlc -package $(PKGS) -syntax camlp4o $(COMPFLAGS)
# CAMLOPT:=ocamlfind ocamlopt -package $(PKGS) -syntax camlp4o $(COMPFLAGS)
# CAMLCINCLUDES:=-I ../include
# CAMLOPTINCLUDES:=$(CAMLCINCLUDES)
# OCAMLDEP:=ocamlfind ocamldep -package $(PKGS) -syntax camlp4o
# OCAMLDOC:=ocamlfind ocamldoc -package $(PKGS) -syntax camlp4o
