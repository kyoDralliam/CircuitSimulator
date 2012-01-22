all: main

main:
	ocamlbuild -use-menhir -yaccflags --infer,--explain,--dump assembleur.native

debug:
	ocamlbuild -use-menhir -yaccflags --infer,--explain,--dump assembleur.d.byte

clean:
	ocamlbuild -clean