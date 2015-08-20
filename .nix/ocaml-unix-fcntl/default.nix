{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml = pkgs.ocaml;
    findlib = pkgs.ocamlPackages.findlib; # needed?
    ocaml-unix-errno = import ../ocaml-unix-errno { } ;
in stdenv.mkDerivation {
    name = "ocaml-unix-fcntl";
  
    src = fetchgit {
      url = https://github.com/dsheets/ocaml-unix-fcntl.git;
      rev = "83ae867";
      sha256 = "947788eac9d42f9a813796c27edc1b4d7b887038e7f34285551b8c130c34fa39";
    };
  
    buildInputs = [ ocaml findlib pkgs.ocamlPackages.ctypes ocaml-unix-errno ocaml-unix-errno.rresult ]; 
  
    createFindlibDestdir = true;

}
