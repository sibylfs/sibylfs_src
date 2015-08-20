{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml = pkgs.ocaml;
    findlib = pkgs.ocamlPackages.findlib; # needed?
in stdenv.mkDerivation {
    name = "ocaml_sha";
  
    src = fetchurl {
      url = https://github.com/vincenthz/ocaml-sha/archive/ocaml-sha-v1.9.tar.gz;
      sha256 = "caa1dd9071c2c56ca180061bb8e1824ac3b5e83de8ec4ed197275006c2a088d0";
    };
  
    buildInputs = [ ocaml findlib ]; 
  
    createFindlibDestdir = true;

}
