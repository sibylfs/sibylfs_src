{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    perl = pkgs.perl;
    ocaml = pkgs.ocaml;
    op = pkgs.ocamlPackages;
    findlib=op.findlib;
    cppo=op.cppo;
    sha = import ./sha { };
    ocaml_sexplib = op.ocaml_sexplib;
    isabelle = import ./isabelle { };
    lem = import ./lem { };
in stdenv.mkDerivation {
    name = "lemenv";

    # make these available in nix-shell
    lem = lem;
    isabelle = isabelle;
    sha = sha;
    
  
    src = lem;  
    buildInputs = [ isabelle ocaml findlib sha cppo ocaml_sexplib lem ]; 
  
    buildPhase = ''
false
      '';

# eval "${!curPhase:-$curPhase}" from nix-shell

shellHook = ''
    export LEMPATH=${lem}/lem
    export PATH=$PATH:${lem}/lem
    export LEMLIB=${lem}/lem/library
  '';

}
