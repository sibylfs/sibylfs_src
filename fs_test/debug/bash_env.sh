set -a # export all vars
# set -x # debug

BASH_DIR=$(realpath $(dirname $BASH_SOURCE))
ROOT=$BASH_DIR/../..

test -f $ROOT/config.sh && source $ROOT/config.sh
source ../bash_env.sh
