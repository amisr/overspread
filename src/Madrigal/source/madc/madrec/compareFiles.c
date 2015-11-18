/*  $Id: compareFiles.c,v 1.2 2002/03/22 16:56:27 jmh Exp $ */

/*
modification history
--------------------
00a,21Feb00         original
*/

/* 
USAGE: compareFiles
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef short Int16;

int
main (argc, argv)
    int     argc;
    char    *argv[];
{
    FILE *f1p, *f2p;
    int nwords=0, ndiff=0, maxlines=800, all=0;
    Int16 w1=0, w2=0;

    if (argc == 3) {
        f1p = fopen(argv[1], "r");
        f2p = fopen(argv[2], "r");
    } else {
        (void) printf("Usage: compareFiles file1 file2\n");
        return(1);
    }

    nwords = 0;
    ndiff = 0;
    for (;;) {
	if (fread(&w1, (size_t) 2, (size_t) 1, f1p) != 1) break;
	if (fread(&w2, (size_t) 2, (size_t) 1, f2p) != 1) break;
	nwords++;
	if (w1 != w2 || all) {
            if (ndiff == 1 && !all)
	        (void) printf("***> The files are different <***\n");
            ndiff++;
	    (void) printf("%6d %6d %6d\n", nwords-1, w1, w2);
	    if (ndiff >= maxlines) return(0);
	}
    }
    if (nwords > 0) {
	(void) printf("    OK - %d words compared\n", nwords);
    } else {
	(void) printf("    ***> ERROR - %d words compared <***\n", nwords);
	return(0);
    }
    (void) fclose(f1p);
    (void) fclose(f2p);
    
    return(0);
}
