TMP = _build
LIBS = str
NATIVEORBYTE = native

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
	rm -f graphToC.$(NATIVEORBYTE)

delete : clean
	rm -f graphToC
	rm -f circuit

circuit : $(TMP)/circuit.c
	$(CC) $< -o $@

$(TMP)/circuit.c : graphToC
	rm -f $@
	./$< > $@

graphToC : graphToC.$(NATIVEORBYTE)
	cp $< $@

FORCE :

%.byte : FORCE
	@ocamlbuild -quiet -libs $(LIBS) $@
%.native : FORCE
	@ocamlbuild -quiet -libs $(LIBS) $@
%.cmo : FORCE
	@ocamlbuild -quiet $@
%.cmi : FORCE
	@ocamlbuild -quiet $@
