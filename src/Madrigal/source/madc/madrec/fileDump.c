/* fileDump.c */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <madrec.h>
#include <cedar.h>

int
main (argc, argv)
int	argc;
char	*argv[];
{
    int i, j;
    long pos1, pos2, fileSize;
    FILE *fp;
    char infile[100];
    Int16 word;

    if (argc != 4) {
        printf("Usage: fileDump fileName pos1 pos2\n");
        exit(1);
    }
    (void) strcpy(infile, argv[1]);

    pos1 = atoi(argv[2]);
    pos2 = atoi(argv[3]);
    
    printf("infile,pos1,pos2 = %s %ld %ld\n", infile,pos1,pos2);


    if ((fp = fopen(infile, "r")) == (FILE *)NULL) {
        printf("Error opening %s\n", infile);
    }

    (void) fseek(fp, 0L, SEEK_END);
    fileSize = ftell(fp)/2;

    if (pos2 >= fileSize) pos2=fileSize-1;
    (void) fseek(fp, 2*pos1, SEEK_SET);
    j = 0;
    printf("\n %10ld", pos1);
    for (i=pos1; i<=pos2; i++) {
        if (fread16(&word, (size_t) 2, (size_t) 1, fp) != 1) {
            printf("fread16 error\n");
        }
        printf("%6d ", word);
        j++;
        if (j == 10) {
            printf("\n %10d", i+1);
            j = 0;
        }
    }
    printf("\n");
    return(0);
}
