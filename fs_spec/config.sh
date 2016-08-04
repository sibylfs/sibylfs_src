# export all vars
set -a 

test -f ../../config.sh && . ../../config.sh

CPPO_ARGS="-D aspect_perms"
LEMFLAGS="-lib $LEMLIB -only_changed_output -wl_unused_vars ign -wl_rename err"
PKGS="-package sexplib,sexplib.syntax,sha -syntax camlp4o"
ocamlc="ocamlfind ocamlc -I $EXTRACTDIR extract.cma $PKGS"
ocamlopt="ocamlfind ocamlopt -I $EXTRACTDIR extract.cmxa $PKGS"
