

all: toplevel main

toplevel:
	ocamlbuild -use-menhir -use-ocamlfind tools.top

main:
	ocamlbuild -use-menhir -use-ocamlfind main.native

run: all
	cd _build && ./tools.top

clean:
	ocamlbuild -clean


.PHONY: all clean run toplevel main