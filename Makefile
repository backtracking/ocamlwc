
all: ocamlwc ocamlwc.dvi

ocamlwc: ocamlwc.ml
	ocamlopt -o ocamlwc ocamlwc.ml

ocamlwc.dvi: ocamlwc.ml
	ocamlweb -s --no-index -o ocamlwc.tex --impl ocamlwc.mll
	latex ocamlwc

install:
	cp ocamlwc $$HOME/bin/$$OSTYPE

ocamlwc.ml: ocamlwc.mll
	ocamllex ocamlwc.mll

clean::
	rm -f ocamlwc ocamlwc.ml ocamlwc.tex
	rm -f *~ *.cm[io] *.o
