{ }:
let 
    pkgs = import <nixpkgs> {};
    inherit (pkgs) stdenv fetchgit ocaml;
    op = pkgs.ocamlPackages;
    inherit (op) findlib cppo sexplib cstruct;
    sha = import ../.nix/sha { };
    fd_send_recv = import ../.nix/fd-send-recv { };
    lem_in_nix = import ../.nix/lem { };
    ocaml_cow = import ../.nix/ocaml_cow { };
    ocaml_dyntype = import ../.nix/dyntype { };
    ocaml-unix-fcntl = import ../.nix/ocaml-unix-fcntl { };
    ocaml-unix-errno = import ../.nix/ocaml-unix-errno { };
    ocaml_version = (stdenv.lib.getVersion ocaml);
    fs_spec = import ../fs_spec { };
in stdenv.mkDerivation {
    name = "fs_test";
  
    src = ./.;  
    buildInputs = [ ocaml findlib cppo sexplib sha op.ctypes op.cmdliner fd_send_recv lem_in_nix pkgs.coreutils pkgs.git op.menhir ocaml_cow ocaml-unix-fcntl ocaml-unix-errno ocaml-unix-errno.rresult fs_spec ]; # git for version num
  
    buildPhase = ''
    export lem=${lem_in_nix}/lem/lem
    export LEMLIB=${lem_in_nix}/lem/library
    export LD_LIBRARY_PATH=${cstruct}/lib/ocaml/${ocaml_version}/site-lib/cstruct
    export GIT_REV="$out"
    export DIRTY_FLAG=""
    export SPEC_BUILD=${fs_spec}/_build
    make
    mkdir -p $out

# commenting so that the closure is not too large
#    cp -RL ${fs_spec}/build $out
#    mkdir -p $out/fs_test
#    cp -RL . $out/fs_test
    '';
  
    installPhase = ''
mkdir -p $out/bin
cp fs_test fs_test_check fs_test_posix run_trace tgen lib/fs_test_version.ml $out/bin
# paths/testpath.native testall.sh 
''; 


#  dontPatchELF = true;
  dontPatchShebangs = true;

}
