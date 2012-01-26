GPP=g++
FREQ=8000
NB_OP=-

all: micro horloge

run: all
	./micro -c $(FREQ) $(NB_OP)

obsidian: .FORCE
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
	./obsidian -cc $(GPP) -o micro _build/bus-clock.o _build/memory.o \
	-ccflags "-lncurses,-lpthread,-O2" Rock/Processor/Micro.rock
	rm -Rf _build

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

micro-fast: obsidian
	mkdir _build
	$(GPP) -o _build/memory.o -c Rock/Devices/memory.cpp
	$(GPP) -o _build/bus-clock.o -c Rock/Devices/bus-clock.cpp
	./obsidian -cc $(GPP) -o micro-fast _build/bus-clock.o \
	    _build/memory.o -ccflags "-lncurses,-lpthread,-O2" \
	    Rock/Processor/Micro-freq-1.rock
	rm -Rf _build

micro-flash: obsidian
	mkdir _build
	$(GPP) -o _build/memory.o -c Rock/Devices/memory.cpp
	$(GPP) -o _build/bus-clock.o -c Rock/Devices/bus-clock-skip.cpp
	./obsidian -cc $(GPP) -o micro-flash _build/bus-clock.o \
	    _build/memory.o -ccflags "-lncurses,-lpthread,-O2" \
	    Rock/Processor/Micro-freq-1.rock
	rm -Rf _build

run-fast: micro-fast assembleur
	./assembleur -o ram Asm/horloge-recalcule.s
	./micro-fast $(NB_OP)

run-flash: micro-flash assembleur
	./assembleur -o ram Asm/horloge-recalcule.s
	./micro-flash $(NB_OP)

run-auto: micro-fast assembleur
	./assembleur -o ram Asm/horloge-autonome.s
	./micro-fast $(NB_OP)

run-revol: micro-fast assembleur
	./assembleur -o ram Asm/horlogerevolutionnaire.s
	./micro-fast $(NB_OP)


clean:
	$(MAKE) --directory=ObsidianCompiler clean
	$(MAKE) --directory=AssembleurDir clean	
	rm -rf compiled-tests
	rm -f micro
	rm -f ram
	rm -f MicroClassique
	rm -f micro-fast
	rm -f micro-flash

.FORCE:

.PHONY: all clean run toplevel main debug tests assembleur
