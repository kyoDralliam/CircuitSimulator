all: main

main:
	ocamlbuild -use-menhir -yaccflags --infer,--explain,--dump assembleur.native
	@cp _build/assembleur.native ../assembleur 

debug:
	ocamlbuild -use-menhir -yaccflags --infer,--explain,--dump assembleur.d.byte

clean:
	ocamlbuild -clean
	rm -f ../assembleur