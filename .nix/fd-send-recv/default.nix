{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml = pkgs.ocaml;
    findlib = pkgs.ocamlPackages.findlib; # needed?
in stdenv.mkDerivation {
    name = "ocaml_fd-send-recv";
  
    src = fetchurl {
      url = https://github.com/xen-org/ocaml-fd-send-recv/archive/ocaml-fd-send-recv-1.0.1.tar.gz;
      sha256 = "664f109b63412493d3689057010938fe8d0fe122e57e4eecc6a2adf0e94f3c92";
    };
  
    buildInputs = [ ocaml findlib ]; 
  
    createFindlibDestdir = true;

}
