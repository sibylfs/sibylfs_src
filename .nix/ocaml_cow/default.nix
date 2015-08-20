{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchurl = pkgs.fetchurl;
    ocaml = pkgs.ocaml;
    op = pkgs.ocamlPackages;
    findlib = op.findlib;
    dyntype = import ../dyntype { };
    omd = import ../omd { };
    ulex = import ../ulex { };
    cstruct = op.cstruct;
  ocaml_version = (stdenv.lib.getVersion ocaml);
    strace = 
      if stdenv.isDarwin then null else pkgs.strace;
in stdenv.mkDerivation {
    name = "ocaml_cow";
  
    src = fetchurl {
      url = https://github.com/mirage/ocaml-cow/archive/v1.2.1.tar.gz;
      sha256 = "c9ae1f0be8ca2fb37673e89c5e5f1a4cfb70d7cfa7e99322ca49592431434ba0";
    };
  
#    patches = ./patch;


  postConfigure = ''
    substituteInPlace camlp4/META.in \
    --replace +camlp4 $out/lib/ocaml/${ocaml_version}/site-lib/camlp4
  '';

#     substituteInPlace camlp4/config/Camlp4_config.ml \
#     --replace \
#       "Filename.concat ocaml_standard_library" \
#       "Filename.concat \"$out/lib/ocaml/${ocaml_version}/site-lib\""


    camlp4=op.camlp4;

    buildInputs = [ ocaml findlib pkgs.which strace ]; 
  
    propagatedBuildInputs = [ dyntype omd op.type_conv op.camlp4 op.re ulex op.uri op.xmlm op.ezjsonm op.camlp4 cstruct ];

   buildPhase = "
export LD_LIBRARY_PATH=${cstruct}/lib/ocaml/${ocaml_version}/site-lib/cstruct
make
";

#    installPhase="true";

    createFindlibDestdir = true;

}
