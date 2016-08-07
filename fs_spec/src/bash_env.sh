# export all vars
set -a 

 # if using nix, this may not be present
test -f $SIBYLFS_SRC/config.sh && source $SIBYLFS_SRC/config.sh

CPPO_ARGS="-D aspect_perms"
LEMFLAGS="-lib $LEMLIB -only_changed_output -wl_unused_vars ign -wl_rename err"
PKGS="-package sexplib,sexplib.syntax,sha"

# these include syntax, so should work on all files; may be overridden in ocamlc.sh
ocamlc="ocamlfind ocamlc -I $EXTRACTDIR extract.cma $PKGS -syntax camlp4o"
ocamlopt="ocamlfind ocamlopt -I $EXTRACTDIR extract.cmxa $PKGS -syntax camlp4o"
ocamldep="ocamlfind ocamldep $PKGS"