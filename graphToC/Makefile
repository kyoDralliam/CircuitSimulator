TMP = _build
LIBS = str
NATIVEORBYTE = native

CYCLES = 10

.PHONY : all test clean delete

vpath %.cmi $(TMP)
vpath %.cmo $(TMP)
vpath %.byte $(TMP)

all : main circuit

test : circuit
	echo -ne "01" | ./$< -s $(CYCLES)

clean :
	ocamlbuild -clean
	rm -f main.$(NATIVEORBYTE)

delete : clean
	rm -f main
	rm -f circuit

circuit : $(TMP)/circuit.c
	$(CC) $< -o $@

$(TMP)/circuit.c : main
	rm -f $@
	./$< > $@

main : main.$(NATIVEORBYTE)
	cp $< $@


debug : dbg/main
	ocamldebug dbg/main-debug
	rm -rf dbg

dbg/ :
	mkdir dbg

dbg/main : dbg/main.cmo dbg/main.cmi dbg/graphToC.cmo dbg/graphToC.cmi dbg/astToGraph.cmo dbg/astToGraph.cmi | dbg/
	ocamlc -g -I dbg -o dbg/graphToC str.cma dbg/astToGraph.cmo dbg/graphToC.cmo dbg/main.cmo

dbg/main.cmo dbg/main.cmi : main.ml dbg/graphToC.cmi dbg/astToGraph.cmi | dbg/
	ocamlc -g -c -I dbg -o dbg/main.cmo main.ml

dbg/graphToC.cmo dbg/graphToC.cmi : graphToC.ml dbg/astToGraph.cmi | dbg/
	ocamlc -g -c -I dbg -o dbg/graphToC.cmo graphToC.ml

dbg/astToGraph.cmo dbg/astToGraph.cmi : astToGraph.ml | dbg/
	ocamlc -g -c -I dbg -o dbg/astToGraph.cmo astToGraph.ml


FORCE :

%.byte : FORCE
	@ocamlbuild -quiet -libs $(LIBS) $@
%.native : FORCE
	@ocamlbuild -quiet -libs $(LIBS) $@
%.cmo : FORCE
	@ocamlbuild -quiet $@
%.cmi : FORCE
	@ocamlbuild -quiet $@