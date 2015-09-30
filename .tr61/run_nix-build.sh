#!/bin/sh

# generate fs_test/lib/fs_test_version.ml using git commit id;
# requires git checkout


# determine git revision
GIT_REV=`git rev-parse HEAD | tr -d '\n'`

# and whether dirty
DIRTY_FLAG=`git diff-index --quiet HEAD || echo "dirty"`

if [ "$DIRTY_FLAG"="dirty" ];
then
    DIRTY=true
else 
    DIRTY=false
fi


# generate version file
(cd fs_test/lib; make clean; make -f Makefile.srcs fs_test_version.ml)


# run nix-build
nix-build
