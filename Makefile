.PHONY: all clean

all:
	ocamlbuild -cflag -bin-annot ocamlaryTestLibrary.cma

clean:
	ocamlbuild -clean
