#!/bin/bash

cd ../../test-suite

echo -e "Running tests ...\n"

exec 3>&-

#sometimes handy while working on just one suite
#rm -r /results/*

#calls
../fs_test/fs_test run --fs=ext --suite=permissions_root

#./testall.sh

echo -e "\nTests finished, starting interactive session ...\n"

/bin/bash
