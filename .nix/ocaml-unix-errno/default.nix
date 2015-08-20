{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml = pkgs.ocaml;
    findlib = pkgs.ocamlPackages.findlib; # needed?
    ocaml_version = (stdenv.lib.getVersion ocaml);
in 
let 
    rresult = stdenv.mkDerivation {
    name = "ocaml-rresult";
  
    src = fetchgit {
      url = https://github.com/dbuenzli/rresult.git;
      rev = "4524762a5";
      sha256 = "249b97f25725f67d7358f046d0411071ee0b0dd200872b62f24e827e6be7d431";
    };
  
    buildInputs = [ ocaml findlib pkgs.opam pkgs.ocamlPackages.ctypes ]; 

    buildPhase = ''

ocaml pkg/git.ml
ocaml pkg/build.ml native=true native-dynlink=true # FIXME tr not sure about these; trying to get the same result as https://github.com/dbuenzli/rresult/blob/master/opam

    '';

    installPhase = ''
opam-installer --prefix=$out/lib/ocaml/${ocaml_version}/site-lib/ --libdir=. rresult.install
    '';
  
    createFindlibDestdir = true;

    };
in
stdenv.mkDerivation {
    name = "ocaml-unix-errno";
  
    src = fetchgit {
      url = https://github.com/dsheets/ocaml-unix-errno.git;
      rev = "5b705b7";
      sha256 = "ba478307f514c98431f2358dde4ef56e0c6a5401c809ebad3a50b852c58d10e5";
    };
  
    buildInputs = [ ocaml findlib pkgs.ocamlPackages.ctypes rresult ]; 
  
    createFindlibDestdir = true;

    rresult = rresult;

}
