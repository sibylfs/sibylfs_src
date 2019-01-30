{ }:

let
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml_old = import ./../ocaml_old {};
    ocaml = ocaml_old.ocaml;
    ocamlPackages = ocaml_old.ocamlPackages;
    op = ocamlPackages;
    findlib=op.findlib;
  ocaml_version = (builtins.parseDrvName ocaml.name).version;
  version = "1.1";
  pname = "ulex";

in

stdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "http://www.cduce.org/download/${pname}-${version}.tar.gz";
    sha256 = "0fjlkwps14adfgxdrbb4yg65fhyimplvjjs1xqj5np197cig67x0";
  };

  createFindlibDestdir = true;

  buildInputs = [ocaml findlib op.camlp4];

  buildFlags = "all all.opt";

  meta = {
    homepage = http://www.cduce.org/download.html;
    description = "A lexer generator for Unicode and OCaml";
    license = stdenv.lib.licenses.mit;
    maintainers = [ stdenv.lib.maintainers.roconnor ];
  };
}
