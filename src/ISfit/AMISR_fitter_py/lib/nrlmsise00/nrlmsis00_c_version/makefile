#CFLAGS = -Wall -g -DINLINE
CFLAGS = -Wall -ansi -D_GNU_SOURCE -D_DARWIN
MFLAGS = $(CFLAGS) -lm
CC = gcc $(CFLAGS)
MCC = gcc $(MFLAGS)
OBJS = nrlmsise-00.o nrlmsise-00_data.o

nrlmsise-test    :     nrlmsise-00.o nrlmsise-00_test.o nrlmsise-00_data.o
	$(MCC) -o nrlmsise-test nrlmsise-00.o nrlmsise-00_test.o \
                     nrlmsise-00_data.o

nrlmsise-00.o :		nrlmsise-00.c nrlmsise-00.h
	$(CC) -c  nrlmsise-00.c

nrlmsise-00_test.o :	nrlmsise-00_test.c nrlmsise-00.h
	$(CC) -c  nrlmsise-00_test.c

nrlmsise-00_data.o :	nrlmsise-00_data.c nrlmsise-00.h
	$(CC) -c  nrlmsise-00_data.c

msislib : $(OBJS)
	libtool -static -o libnrlmsise-00.a $(OBJS)
	gcc -dynamiclib -all_load -flat_namespace -single_module -o libnrlmsise-00.dylib libnrlmsise-00.a

clean   :
	rm -rf  nrlmsise-test nrlmsise-00.o nrlmsise-00_test.o \
                   nrlmsise-00_data.o
