set -a # export all vars
# set -x # debug

BASH_DIR=$(realpath $(dirname $BASH_SOURCE))
ROOT=$BASH_DIR/../..

test -f $ROOT/config.sh && source $ROOT/config.sh
source ../bash_env.sh

  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN $CCFLAGS $PKGS -I ../include fs_spec_lib.cma  $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN $CCFLAGS $PKGS -I ../include fs_spec_lib.cmxa $SYNTAX"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

function run_menhir {
    menhir --explain --infer --ocamlc "ocamlfind ocamlc $PKGS -I ../include $SYNTAX" $@
}
