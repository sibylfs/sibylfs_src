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
      rev = "83ae867"; # july 27 2015
      sha256 = "06fznyvgkgfxkwk60v499ifb74y0ip6p5wwggckj2fspp0ljnlyg";
    };
  
    buildInputs = [ ocaml findlib pkgs.ocamlPackages.ctypes ocaml-unix-errno ocaml-unix-errno.rresult ]; 
  
    createFindlibDestdir = true;

}
