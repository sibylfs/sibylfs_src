#!/usr/bin/env bash

# allow override in case not git repo

# what if sibylfs_src is a subdir of a git repo?
IN_GIT_DIR=`git status || echo false` 
if [ "$IN_GIT_DIR" = "false" ]; then
    if [ -z "$DIRTY"]; then DIRTY=true; fi
    if [ -z "$GIT_REV" ]; then GIT_REV="unknown_git_rev"; fi
else   
    GIT_DIFF=`git diff-index --quiet HEAD || echo "dirty"`
    if [ "$GIT_DIFF" = "dirty" ]; then DIRTY=true; else DIRTY=false; fi
    if [ -z "$GIT_REV" ]; then GIT_REV=`git rev-parse HEAD | tr -d '\n'`; fi
fi

# note we don't force this to be rebuilt every time, but the sources
# could change from clean to dirty

cat >fs_test_version.ml <<EOF
let git_rev = "$GIT_REV"

let git_dirty = $DIRTY
EOF


# echo d:$DIRTY gr: $GIT_REV

