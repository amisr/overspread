compile:
	gcc -c -Wall -Werror -fPIC spec_worker.c -o spec_worker.o
	gcc -shared -o libspec.so spec_worker.o

clean:
	rm -f libspec.a libspec.so *.o 

