25: a.out
	./a.out

art:
	cargo build
	cp ../target/debug/artemis art

runtime.o:
	gcc -c runtime.c -o runtime.o -O0 -g -static

a.o: art
	./art -o a.o ../examples/25.art

a.out: runtime.o a.o
	gcc runtime.o a.o -o a.out

clean:
	-rm art a.out runtime.o a.o

.PHONY: clean art
