TMP:=_build

all: 
	mkdir -p $(TMP)
	cd $(TMP) && ln -sf ../src/* ../src/.depend .
	$(MAKE) -C $(TMP)

clean: FORCE
	rm -rf $(TMP) ./result

FORCE:
