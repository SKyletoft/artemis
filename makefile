c_working_files/runtime.o: c_working_files/runtime.c
	make runtime.o -C c_working_files

clean:
	make clean -C c_working_files 

.PHONY: clean
