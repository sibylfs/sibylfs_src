{ }:
let 
    pkgs = import <nixpkgs> {};
in {
  ocamlPackages = pkgs.ocamlPackages_4_02;
  ocaml = pkgs.ocamlPackages_4_02.ocaml;
}
