
# where to install

BINDIR = /usr/bin

all: ocamlwc ocamlwc.dvi

ocamlwc: ocamlwc.ml
	ocamlopt -o ocamlwc ocamlwc.ml

ocamlwc.dvi: ocamlwc.ml
	ocamlweb -s --no-index -o ocamlwc.tex --impl ocamlwc.mll
	latex ocamlwc

ocamlwc.ps: ocamlwc.dvi
	dvips ocamlwc.dvi -o ocamlwc.ps

local:: ocamlwc
	cp ocamlwc $$HOME/bin/$$OSTYPE

install:: ocamlwc
	cp ocamlwc $(BINDIR)

ocamlwc.ml: ocamlwc.mll
	ocamllex ocamlwc.mll

FTP=$$HOME/WWW/ftp/ocaml/misc
VERSION=0.1
NAME=ocamlwc-$(VERSION)

export:: ocamlwc.ps
	cp GPL README ocamlwc.mll $(FTP)
	gzip --best -c ocamlwc.ps > $(FTP)/ocamlwc.ps.gz
	mkdir -p export/$(NAME)
	cp GPL README Makefile ocamlwc.mll export/$(NAME)
	cd export; tar zcf $(NAME).tar.gz $(NAME)
	cd export; cp $(NAME).tar.gz $(FTP)

clean::
	rm -f ocamlwc ocamlwc.ml ocamlwc.tex
	rm -f *~ *.cm[iox] *.o
	rm -f *.log *.aux *.dvi

