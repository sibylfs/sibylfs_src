{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml_old = import ./../ocaml_old {};
    ocaml = ocaml_old.ocaml;
    findlib = pkgs.ocamlPackages.findlib; # needed?
    ocaml_version = (stdenv.lib.getVersion ocaml);
in 
let 
    rresult = stdenv.mkDerivation {
    name = "ocaml-rresult";
  
    src = fetchgit {
      url = https://github.com/dbuenzli/rresult.git;
      rev = "4524762a5";
      sha256 = "1zriaj4xaj7a5zw848m6sp0rh69xk15zdazpnkpw899y39yv5a0p";
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
      sha256 = "0pf54p7pcvbxhszh7scsw81m8p2yl7l3mqpy7mxz9nygaq6xibrp";
    };
  
    buildInputs = [ ocaml findlib pkgs.ocamlPackages.ctypes rresult ]; 
  
    createFindlibDestdir = true;

    rresult = rresult;

}
