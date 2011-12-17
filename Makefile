GPP=g++-mp-4.5

all: main

toplevel:
	ocamlbuild -use-menhir -lib str tools.top

main:
	ocamlbuild -use-menhir -lib str main.native
	@cp _build/FrontEnd/main.native obsidian

debug:
	ocamlbuild -use-menhir -lib str main.d.byte

run: all
	cp -R Resources _build/FrontEnd/Resources && \
	cp -R FrontEnd/tests _build/FrontEnd/tests && \
	cd _build/FrontEnd && ./tools.top

compiled-tests/:
	mkdir compiled-tests

tests: main | compiled-tests/
	./obsidian -o compiled-tests/count-mod-24 FrontEnd/tests/count-mod-24.rock
	./obsidian -o compiled-tests/count-mod-256 FrontEnd/tests/count-mod-256.rock
	./obsidian -o compiled-tests/parallel-adder FrontEnd/tests/parallel-adder.rock
	# Memory
	$(GPP) -o _build/memory.o -c FrontEnd/devices/memory.cpp
	./obsidian -cc $(GPP) -o compiled-tests/use-memory _build/memory.o \
FrontEnd/tests/use-memory.rock
	# Bus
	$(GPP) -o _build/bus.o -c FrontEnd/devices/bus.cpp
	./obsidian -cc $(GPP) -o compiled-tests/test-bus _build/bus.o \
FrontEnd/tests/test-bus.rock -ccflags "-lncurses,-lpthread"

clean:
	ocamlbuild -clean
	rm -rf compiled-tests


.PHONY: all clean run toplevel main debug tests