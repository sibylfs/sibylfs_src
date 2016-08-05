# export all vars
set -a 

COREPKGS="unix,bigarray,str,num,bytes"
XTRAPKGS="bytes,sexplib,sexplib.syntax,cmdliner,fd-send-recv,sha,cow,cow.syntax,unix-fcntl,$BISECT"
PKGS="-package $COREPKGS,$XTRAPKGS -syntax camlp4o"
WARN="-w @f@p@u@s@40"
CCFLAGS="-g"
  ocamlc="ocamlfind ocamlc   $WARN $CCFLAGS -I ../include fs_spec_lib.cma  $PKGS"
ocamlopt="ocamlfind ocamlopt $WARN $CCFLAGS -I ../include fs_spec_lib.cmxa $PKGS"
