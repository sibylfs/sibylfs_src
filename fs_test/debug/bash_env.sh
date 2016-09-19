set -a # export all vars
# set -x # debug

BASH_DIR=$(realpath $(dirname $BASH_SOURCE))
ROOT=$BASH_DIR/../..

test -f $ROOT/config.sh && source $ROOT/config.sh
source ../bash_env.sh

  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN $CCFLAGS $PKGS -I $BASH_DIR/include extract.cma  fs_spec_lib.cma $SYNTAX $WITH_FS_CHECK_LIB"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN $CCFLAGS $PKGS -I $BASH_DIR/include extract.cmxa fs_spec_lib.cmxa $SYNTAX $WITH_FS_CHECK_LIB"
