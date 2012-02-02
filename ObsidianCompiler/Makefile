GPP=g++

all: ../obsidian

../obsidian: _build/FrontEnd/main.native
	@cp _build/FrontEnd/main.native ../obsidian

toplevel:
	ocamlbuild -use-menhir -lib str tools.top

_build/FrontEnd/main.native:
	ocamlbuild -use-menhir -lib str main.native
	rm main.native

debug:
	ocamlbuild -use-menhir -lib str main.d.byte

run: toplevel
	cp -R ../Resources _build/FrontEnd/Resources && \
	cp -R ../Rock/Tests _build/FrontEnd/tests && \
	cd _build/FrontEnd && ./tools.top

compiled-tests/:
	mkdir compiled-tests

tests: main | compiled-tests/
	../obsidian -o compiled-tests/count-mod-24 ../Rock/Tests/count-mod-24.rock
	../obsidian -o compiled-tests/count-mod-256 ../Rock/Tests/count-mod-256.rock
	../obsidian -o compiled-tests/parallel-adder ../Rock/Tests/parallel-adder.rock
	# Memory
	$(GPP) -o _build/memory.o -c ../Rock/Devices/memory-bis.cpp
	../obsidian -cc $(GPP) -o compiled-tests/use-memory _build/memory.o \
../Rock/Tests/use-memory.rock
	# Bus
	$(GPP) -o _build/bus.o -c ../Rock/Devices/bus.cpp
	../obsidian -cc $(GPP) -o compiled-tests/test-bus _build/bus.o \
../Rock/Tests/test-bus.rock -ccflags "-lncurses,-lpthread"


clean:
	ocamlbuild -clean
	rm -f ../obsidian

.PHONY: all clean run toplevel main debug tests