c_working_files/runtime.o: c_working_files/runtime.c
	make runtime.o -C c_working_files

c_working_files/26: c_working_files/26.c
	gcc c_working_files/26.c -o c_working_files/26

clean:
	make clean -C c_working_files 
	-rm a.out a.out.S *.o

.PHONY: clean
