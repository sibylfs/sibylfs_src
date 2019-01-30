{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml_old = import ./../ocaml_old {};
    ocaml = ocaml_old.ocaml;
    op = ocaml_old.ocamlPackages;
in stdenv.mkDerivation {
    name = "ocaml_dyntype";
  
    src = fetchurl {
      url = https://github.com/mirage/dyntype/archive/dyntype-0.9.0.tar.gz;
      sha256 = "60e9417f7613d121cea4e9cde4aaafde26b1914b6fd5f4096f6b384e442ab1d0";
    };
  
    buildInputs = [ ocaml op.findlib op.type_conv pkgs.which op.camlp4 ]; 
  
    createFindlibDestdir = true;

}
