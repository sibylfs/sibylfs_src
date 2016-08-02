#!/usr/bin/env bash

# Edit this file to remove the exit, and ensure the paths are set
# appropriately

# path to the directory containing the lem executable; must be an
# absolute path
LEMPATH=/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/lem

# path to lem's library (lots of .lem files); must be an absolute path
LEMLIB=/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/library

# path to lem's ocaml library; must be an absolute path; we expect to
# find EXTRACTDIR, with extract.cm[x]a
EXTRACTDIR=/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/ocaml-lib/_build

# if on your path, this does not have to be an absolute path; it is
# the command to run cppo
CPPO=cppo

SIBYLFS_CONFIG=true

# exit 1