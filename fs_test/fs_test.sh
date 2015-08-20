#!/bin/bash

# invoke fs_test with absolute path

this_file=${BASH_SOURCE[0]}
dir=`dirname $this_file`
command="$dir/fs_test $@"
eval $command


