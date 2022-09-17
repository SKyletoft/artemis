runtime.o: runtime.c
	gcc -c runtime.c -o runtime.o -O0 -g

callable_test: callable_object.o callable_object_x64.o
	gcc callable_object_x64.o callable_object.o -o callable_test

callable_object.o: callable_object.c
	gcc callable_object.c -c -g

callable_object_x64.o: callable_object_x64.S
	nasm callable_object_x64.S -f elf64 -o callable_object_x64.o -g

test: callable_test
	./callable_test

clean:
	-rm runtime.o
	-rm callable_test
	-rm callable_object.o
	-rm callable_object_x64.o

.PHONY: clean
