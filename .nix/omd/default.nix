{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml_old = import ./../ocaml_old {};
    ocaml = ocaml_old.ocaml;
    ocamlPackages = ocaml_old.ocamlPackages;
    op = ocamlPackages;
in stdenv.mkDerivation {
    name = "ocaml_omd";
  
    src = fetchurl {
      url = http://pw374.github.io/distrib/omd/omd-1.2.6.tar.gz;
      sha256 = "4164fe538149e51e19c2bd786f5f817b7c2f6ba9d1376965d2e43d93d745aeb6";
    };
  
    buildInputs = [ ocaml op.findlib  ]; 
 
    configurePhase="
    mkdir -p $out/bin
    export bindir=$out/bin
    make configure
";

    createFindlibDestdir = true;

}
