{ }:
let 
    pkgs = import <nixpkgs> {};
    inherit (pkgs) stdenv fetchgit ocaml;
    op = pkgs.ocamlPackages;
    inherit (op) findlib cppo sexplib cstruct;
    sha = import ../.nix/sha { };
    fd_send_recv = import ../.nix/fd-send-recv { };
    lem = import ../.nix/lem { };
    ocaml_cow = import ../.nix/ocaml_cow { };
    ocaml_dyntype = import ../.nix/dyntype { };
    ocaml_version = (stdenv.lib.getVersion ocaml);
in stdenv.mkDerivation {
    name = "fs_spec";
  
    src = ./.;  
    buildInputs = [ ocaml findlib cppo sexplib sha op.cmdliner fd_send_recv lem pkgs.coreutils pkgs.git op.menhir ocaml_cow ]; # git for version num
  
    buildPhase = ''
    export CPPO=${cppo}/bin/cppo
    export LEM=${lem}/lem/lem
    export LEMLIB=${lem}/lem/library
    export PATH=$LEMPATH:$PATH
    export LD_LIBRARY_PATH=${cstruct}/lib/ocaml/${ocaml_version}/site-lib/cstruct
    export EXTRACTDIR=${lem}/lem/ocaml-lib/_build
    export SIBYLFS_CONFIG=true
    make -C build
    mkdir -p $out
    cp -RL build $out
    '';
  
    installPhase = "true";  # skip

}
