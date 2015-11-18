 cc -Wall -ansi -D_GNU_SOURCE -D_DARWIN -c spec_worker.c 
libtool -static -o libspec.a spec_worker.o
 gcc -dynamiclib -all_load -flat_namespace -single_module -o libspec.dylib libspec.a