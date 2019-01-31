{ }:

# adapted from nixpkgs by tr

let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    perl = pkgs.perl;
    nettools = pkgs.nettools;
#    java = pkgs.jre;
#    polyml = pkgs.polyml;
#    proofgeneral = pkgs.emacs24Packages.proofgeneral;

in

#{ stdenv, fetchurl, perl, nettools, java, polyml, proofgeneral }:
# nettools needed for hostname

let
  dirname = "Isabelle2014";
  theories = ["HOL" "FOL" "ZF"];
in

stdenv.mkDerivation {
  name = "isabelle-2014";
  inherit dirname theories;



  src = if stdenv.isDarwin
    then fetchurl {
      url = http://isabelle.in.tum.de/dist/Isabelle2014_macos.tar.gz; # FIXME update
      sha256 = "1aa3vz2nnkkyd4mlsqbs69jqfxlll5h0k5fj9m1j9wqiddqwvwcf";
    }
    else fetchurl {
      url = http://isabelle.in.tum.de/website-Isabelle2014/dist/Isabelle2014_linux.tar.gz;
      sha256 = "0z81pwwllavka4r57fx6yi9kbpbb9xbanp8dsjix49qpyj2a72jy";
    };

  buildInputs = [ perl ]
             ++ stdenv.lib.optionals (!stdenv.isDarwin) [ nettools ];

  sourceRoot = dirname;

  postPatch = ''
    ENV=$(type -p env)
    patchShebangs "."
    substituteInPlace lib/Tools/env \
      --replace /usr/bin/env $ENV
    substituteInPlace lib/Tools/install \
      --replace /usr/bin/env $ENV
  '';

#    substituteInPlace etc/settings \
#      --subst-var-by ML_HOME "${polyml}/bin" \
#      --subst-var-by PROOFGENERAL_HOME "${proofgeneral}/share/emacs/site-lisp/ProofGeneral"
#    substituteInPlace contrib/jdk/etc/settings \
#      --replace ISABELLE_JDK_HOME= '#ISABELLE_JDK_HOME='
#    substituteInPlace contrib/polyml-5.5.2-1/etc/settings \
#      --replace 'ML_HOME="$POLYML_HOME/$ML_PLATFORM"' \
#                "ML_HOME=\"${polyml}/bin\""


  buildPhase = ''
    # note the -b in the following
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv $TMP/$dirname $out
    cd $out/$dirname
    bin/isabelle install $out/bin
#    $out/bin/isabelle build -s -b HOL
  '';

  meta = {
    description = "A generic proof assistant";

    longDescription = ''
      Isabelle is a generic proof assistant.  It allows mathematical formulas
      to be expressed in a formal language and provides tools for proving those
      formulas in a logical calculus.
    '';
    homepage = http://isabelle.in.tum.de/;
    license = "LGPL";
    maintainers = [ stdenv.lib.maintainers.jwiegley ];
  };
}
