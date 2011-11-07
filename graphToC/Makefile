TMP = _build
LIBS = str

CYCLES = 10

.PHONY : all test clean delete

vpath %.cmi $(TMP)
vpath %.cmo $(TMP)
vpath %.byte $(TMP)

all : graphToC circuit

test : circuit
	echo -ne "01" | ./$< -s $(CYCLES)

clean :
	ocamlbuild -clean
	rm -f graphToC.byte

delete : clean
	rm -f graphToC
	rm -f circuit

circuit : $(TMP)/circuit.c
	$(CC) $< -o $@

$(TMP)/circuit.c : graphToC
	rm -f $@
	./$< > $@

graphToC : graphToC.byte
	cp $< $@

FORCE :

%.byte : FORCE
	@ocamlbuild -quiet -libs $(LIBS) $@
%.cmo : FORCE
	@ocamlbuild -quiet $@
%.cmi : FORCE
	@ocamlbuild -quiet $@
