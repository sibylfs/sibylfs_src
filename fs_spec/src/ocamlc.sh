# assume bash_env.sh sourced already

# what target? http://stackoverflow.com/questions/965053/extract-filename-and-extension-in-bash
target1="${@: -1}" # last arg
target1="${target1%.*}" # strip extension
#echo "target1 is $target1"
case "$target1" in
    abstract_string | fs_spec | dir_heap | fs_dump | fs_printer | fs_interface)
        SYNTAX="-syntax camlp4o";;
    *)
        SYNTAX="";;
esac

ocamlc="ocamlfind ocamlc -I $EXTRACTDIR extract.cma $PKGS $SYNTAX"
ocamlopt="ocamlfind ocamlopt -I $EXTRACTDIR extract.cmxa $PKGS $SYNTAX"

# assume at most 2 targets given
if [ "$BUILD_BYTE" != false ]; then
    CMD="$ocamlc $SYNTAX $@"
    # echo $CMD
    $CMD
fi

if [ "$BUILD_NATIVE" != false ]; then
    CMD="$ocamlopt $SYNTAX $@"
    # echo $CMD
    $CMD
fi
