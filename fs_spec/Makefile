TMP:=_build

all: 
	mkdir -p $(TMP)
	cp src/Makefile src/config.sh src/.depend $(TMP)
	$(MAKE) -C $(TMP) run_link
	$(MAKE) -C $(TMP)

clean: FORCE
	rm -rf $(TMP)

FORCE:
