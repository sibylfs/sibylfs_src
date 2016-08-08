set -a # export all vars
# set -x # debug

root=$(realpath $(dirname $BASH_SOURCE))/../..

 # if using nix, this may not be present
test -f $root/config.sh && source $root/config.sh

CPPO_ARGS="-D aspect_perms"

LEMFLAGS="-lib $LEMLIB -only_changed_output -wl_unused_vars ign -wl_rename err -wl_comp_message ign -wl_pat_exh ign"

PKGS="-package sexplib,sexplib.syntax,sha"
SYNTAX="-syntax camlp4o" # simplify: use for every file

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

# these include syntax, so should work on all files; may be overridden in ocamlc.sh
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN -I $EXTRACTDIR extract.cma  $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN -I $EXTRACTDIR extract.cmxa $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"
