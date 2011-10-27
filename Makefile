

all: toplevel main

toplevel:
	ocamlbuild -use-menhir tools.top

main:
	ocamlbuild -use-menhir main.native

run: all
	cd _build && ./tools.top

clean:
	ocamlbuild -clean


.PHONY: all clean run toplevel main