.PHONY: all clean

all:
	ocamlbuild -cflag -bin-annot \
		ocamlaryTestLibrary.cmi \
		ocamlaryTestModule.cmi

clean:
	ocamlbuild -clean
