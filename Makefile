

all: toplevel main

toplevel:
	ocamlbuild -use-menhir -use-ocamlfind tools.top

main:
	ocamlbuild -use-menhir -use-ocamlfind main.native

run: all
	cp -R Resources _build/FrontEnd/Resources && \
	cp -R FrontEnd/tests _build/FrontEnd/tests && \
	cd _build/FrontEnd && ./tools.top

clean:
	ocamlbuild -clean


.PHONY: all clean run toplevel main