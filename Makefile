

all:
	ocamlbuild -use-menhir tools.top

run: all
	cd _build && ./tools.top

clean:
	ocamlbuild -clean


.PHONY: all clean run