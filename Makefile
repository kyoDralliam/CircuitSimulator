

all: toplevel main

toplevel:
	ocamlbuild -use-menhir tools.top

main:
	ocamlbuild -use-menhir -lib str main.native

run: all
	cd _build && ./tools.top

clean:
	ocamlbuild -clean


.PHONY: all clean run toplevel main