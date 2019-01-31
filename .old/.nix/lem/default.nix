{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml = pkgs.ocaml;
    git = pkgs.git;
    findlib = pkgs.ocamlPackages.findlib; # needed?
#    isabelle = import ./../isabelle { }; # not needed?
in stdenv.mkDerivation {
    name = "lem";
  
    src = fetchgit {
      url = https://tomridge@bitbucket.org/tomridge/lem.git;
      rev = "5b4a168"; 
      sha256 = "19642yadi089p8si87n0rbn0mwwxyvzjrv33g848gaqy811i0zak";
    };
  
    buildInputs = [ ocaml git pkgs.perl  ]; # isabelle pkgs.pkgconfig findlib 
  
    buildPhase = ''
      echo 'let v="5b4a168"' >src/version.ml  # complete hack - the source code isn't a git repo after fetchgit
      echo "!!!"
      make

      echo "!!!"
      make ocaml-libs

      #mkdir -p $out/lem/.isabelle  # after build, need to copy these images to local .isabelle
      #export USER_HOME=$out/lem
      # make isa-libs
      #isabelle build -d isabelle-lib -b LEM # do this in the lem source dir

      # sudo chmod u+w ~/.isabelle/Isabelle2014/heaps/polyml-5.5.2_x86-linux/*
      # cp .isabelle/Isabelle2014/heaps/polyml-5.5.2_x86-linux/* ~/.isabelle/Isabelle2014/heaps/polyml-5.5.2_x86-linux/
      '';
  
    installPhase = ''
mkdir -p $out/lem && cp -R -L * $out/lem
''; # so we can inspect the result
    # note that we need to export LEM_LIBRARY_PATH=<absolute path to lem directory>/library before invoking lem

}
