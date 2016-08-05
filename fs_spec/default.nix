{ }:
let 
    pkgs = import <nixpkgs> {};
    inherit (pkgs) stdenv fetchgit ocaml;
    op = pkgs.ocamlPackages;
    inherit (op) findlib cppo sexplib cstruct;
    sha = import ../.nix/sha { };
    fd_send_recv = import ../.nix/fd-send-recv { };
    lem_in_nix = import ../.nix/lem { };
    ocaml_cow = import ../.nix/ocaml_cow { };
    ocaml_dyntype = import ../.nix/dyntype { };
    ocaml_version = (stdenv.lib.getVersion ocaml);
in stdenv.mkDerivation {
    name = "fs_spec";
  
    src = ./.;  
    buildInputs = [ ocaml findlib cppo sexplib sha op.cmdliner fd_send_recv lem_in_nix pkgs.coreutils pkgs.git op.menhir ocaml_cow ]; # git for version num
  
    buildPhase = ''
    export cppo=${cppo}/bin/cppo
    export lem=${lem_in_nix}/lem/lem
    export LEMLIB=${lem_in_nix}/lem/library
    export LD_LIBRARY_PATH=${cstruct}/lib/ocaml/${ocaml_version}/site-lib/cstruct
    export EXTRACTDIR=${lem_in_nix}/lem/ocaml-lib/_build
    make
    mkdir -p $out
    cp -RL _build $out
    '';
  
    installPhase = "true";  # skip

}
