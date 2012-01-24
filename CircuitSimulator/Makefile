GPP=clang++
FREQ=8000
NB_OP=1000000

all: micro horloge

run: all
	./Micro -c $(FREQ) $(NB_OP)

obsidian:
	$(MAKE) --directory=ObsidianCompiler 

compiled-tests/:
	mkdir compiled-tests

tests: obsidian | compiled-tests/
	mkdir _build
	./obsidian -o compiled-tests/count-mod-24 Rock/Tests/count-mod-24.rock
	./obsidian -o compiled-tests/count-mod-256 Rock/Tests/count-mod-256.rock
	./obsidian -o compiled-tests/parallel-adder Rock/Tests/parallel-adder.rock
	# Memory
	$(GPP) -o _build/memory.o -c Rock/Devices/memory-bis.cpp
	./obsidian -cc $(GPP) -o compiled-tests/use-memory _build/memory.o \
Rock/Tests/use-memory.rock
	# Bus
	$(GPP) -o _build/bus.o -c Rock/Devices/bus.cpp
	./obsidian -cc $(GPP) -o compiled-tests/test-bus _build/bus.o \
Rock/Tests/test-bus.rock -ccflags "-lncurses,-lpthread"
	rm -R _build

micro: obsidian
	mkdir _build
	$(GPP) -o _build/memory.o -c Rock/Devices/memory.cpp
	$(GPP) -o _build/bus-clock.o -c Rock/Devices/bus-clock.cpp
	./obsidian -cc $(GPP) -o Micro _build/bus-clock.o _build/memory.o \
	-ccflags "-lncurses,-lpthread" Rock/Processor/Micro.rock
	rm -R _build

micro-classique: obsidian
	mkdir _build
	$(GPP) -o _build/memory.o -c Rock/Devices/memory.cpp
	./obsidian -cc $(GPP) -o MicroClassique _build/memory.o Rock/Processor/micro-classique.rock
	rm -R _build

assembleur:
	$(MAKE) --directory=AssembleurDir

horloge: assembleur
	./assembleur -o ram Asm/horloge.s 

run-facto: micro-classique assembleur
	./assembleur -o ram Asm/facto_rec.s
	./MicroClassique -s 567

clean:
	$(MAKE) --directory=ObsidianCompiler clean
	$(MAKE) --directory=AssembleurDir clean	
	rm -rf compiled-tests
	rm -f Micro
	rm -f ram
	rm -f MicroClassique

.PHONY: all clean run toplevel main debug tests assembleur