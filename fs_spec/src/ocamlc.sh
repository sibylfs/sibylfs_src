# assume bash_env.sh sourced already

# take -syntax from (last) target
# what target? http://stackoverflow.com/questions/965053/extract-filename-and-extension-in-bash
target1="${@: -1}" # last arg is target
target1="${target1%.*}" # strip extension
#echo "target1 is $target1"
case "$target1" in
    abstract_string | fs_spec | dir_heap | fs_dump | fs_printer | fs_interface)
        SYNTAX="-syntax camlp4o";;
    *)
        SYNTAX="";;
esac

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

  ocamlc="ocamlfind ocamlc   $WARN -I $EXTRACTDIR extract.cma  $PKGS $SYNTAX"
ocamlopt="ocamlfind ocamlopt $WARN -I $EXTRACTDIR extract.cmxa $PKGS $SYNTAX"

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
