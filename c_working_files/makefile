25: 25.o runtime.o
	gcc runtime.o 25.o -o 25

26: 26.o runtime.o
	gcc runtime.o 26.o -o 26

25.o:
	cargo run -- ../examples/25.art -o 25.o

26.o:
	cargo run -- ../examples/26.art -o 26.o

runtime.o:
	gcc -c runtime.c -o runtime.o -O0 -g

clean:
	-rm runtime.o 25 25.o 26 26.o

.PHONY: clean art
