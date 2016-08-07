TMP:=_build

all: 
	mkdir -p $(TMP)
	cd $(TMP) && ln -sf ../src/Makefile ../src/bash_env.sh ../src/ocamlc.sh ../src/.depend .
	$(MAKE) -C $(TMP) run_link
	$(MAKE) -C $(TMP)

clean: FORCE
	rm -rf $(TMP)

FORCE:
