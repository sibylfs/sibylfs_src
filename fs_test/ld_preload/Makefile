UNAME=$(shell uname)

ifeq ($(UNAME),Linux)
DL_FLAGS=-ldl
else
ifeq ($(UNAME),FreeBSD)
DL_FLAGS=
else
DL_FLAGS=
endif
endif

all:  mycommand.native libtestlib.so a.out

mycommand.native: mycommand.ml
	ocamlbuild $@ # make produce a /bin/sh: 1: tput: not found error; not sure why
	cp $@ /tmp

# http://stackoverflow.com/questions/14884126/build-so-file-from-c-file-using-gcc-command-line
libtestlib.so: testlib.c
	cc -shared -o libtestlib.so -fPIC $< $(DL_FLAGS)

a.out: testmain.c
	cc $<

testnolib: all
	./a.out

testlib: all
	LD_PRELOAD=$(PWD)/libtestlib.so ./a.out

clean:
	rm -f a.out libtestlib.so *.o
	ocamlbuild -clean
