set -a # export all vars
# set -x # debug

BASH_DIR=$(realpath $(dirname $BASH_SOURCE))
ROOT=$BASH_DIR/..

test -f $ROOT/config.sh && source $ROOT/config.sh

SPEC_BUILD="${SPEC_BUILD:-$ROOT/fs_spec/_build}" # default if not set

# cmis from elsewhere
from_lem="
$EXTRACTDIR/*.cmi 
$EXTRACTDIR/extract.*
"
from_spec="
$SPEC_BUILD/lem_support.cmi 
$SPEC_BUILD/abstract_string.cmi 
$SPEC_BUILD/fs_interface.cmi
$SPEC_BUILD/fs_dict_wrappers.cmi
$SPEC_BUILD/fs_prelude.cmi
$SPEC_BUILD/fs_spec_lib.*
"

COREPKGS="unix,bigarray,str,num,bytes"
XTRAPKGS="bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,$BISECT"
PKGS="-package $COREPKGS,$XTRAPKGS"
SYNTAX="-syntax camlp4o"
WARN="-w @f@p@u@s@40"
CCFLAGS="-g"

WITH_FS_CHECK_LIB="-I $BASH_DIR/lib fs_check_lib.cmxa"

# FIXME we should include fs_check_lib when we know it has been built
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN $CCFLAGS $PKGS -I $BASH_DIR/include extract.cma  fs_spec_lib.cma $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN $CCFLAGS $PKGS -I $BASH_DIR/include extract.cmxa fs_spec_lib.cmxa $SYNTAX"
ocamldep="ocamlfind ocamldep $WARN $CCFLAGS $PKGS -I $BASH_DIR/include extract.cmxa fs_spec_lib.cmxa $SYNTAX $FCLXA"

mk_native="$ocamlopt $WITH_FS_CHECK_LIB $BASH_DIR/lib/syscall_stubs.o"
