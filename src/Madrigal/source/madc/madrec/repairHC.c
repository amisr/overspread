/*
repairHC.c

modification history
--------------------
01,22Feb2002         original

repairHC reads a binary catalog or header record written in the
non-standard binary format used before February 2002, and converts that
record to standard CEDAR ASCII. This program also serves to define that
format. It's key features are a 45 word (16-bit words) block at the
beginning of the record, of which the initial words are a standard
CEDAR-format prolog, followed by a text section. The text section
contains 80-character blocks, as in the CEDAR standard for catalog and
header records. However, if the block is padded with blank characters,
the first blank in the terminating sequence of strings is replaced by a
CR (ASCII 13) character.

*/

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
    int i=0, j=0, k=0, fileSize=0, nblocks=0, block=0, nwritten=0,
        ltot=0, krec=0, nlines=0;
    Int16 *block1p, *block2p;
    FILE *fpin, *fpout;
    char c, *buf, infile[100], outfile[100];
    Int16 word=0;

    if (argc !=3) {
        printf("Usage: repairHC sjcFile outFIle\n");
        exit(1);
    }
    (void) strcpy(infile, argv[1]);
    (void) strcpy(outfile, argv[2]);

    /* Open input file */ 
    if ((fpin = fopen(infile, "r")) == (FILE *)NULL) {
        printf("Error opening %s\n", infile);
    }

    /* Open output file */
    if ((fpout = fopen(outfile, "w")) == (FILE *)NULL) {
        printf("Error opening %s\n", outfile);
    }

    /* Get size of input file and generate two memory blocks
       of the same size */
    (void) fseek(fpin, 0L, SEEK_END);
    fileSize = ftell(fpin);
    (void) fseek(fpin, 0L, SEEK_SET);
    block1p = (Int16 *)malloc(fileSize);
    block2p = (Int16 *)malloc(fileSize);
    nblocks = fileSize/13440;
    printf("Input file = %s - size=%d, nblocks=%d\n", 
        infile,fileSize,nblocks);

    /* Copy the input file into the first memory block stripping the
       extra Madrigal format words from the file */
    block = 0;
    k = 0;
    for (i=0; i<fileSize/2; i++) {
        if (fread16(&word, (size_t) 2, (size_t) 1, fpin) != 1) {
            printf("fread16 error - i=%d\n", i);
            return(1);
        }
        if (i == block*6720) {
            block++;
        }
        j = i - 6720*(block-1);
        if (j == 0 || j == 1 || j == 2 || j == 6719) {
            printf("skipping word %d %d %d %d\n", i,j,block,word);
            continue;
        } else {
            block1p[k] = word;
            k++;
        }
    }
    nwritten = k-1;
    printf("Wrote %d words to memory\n", nwritten);

    /* Determine number of lines in record */
    j = 0;    /* index into record */
    k = 0;    /* index into line */
    nlines = 0;    /* numbar of lines */
    for (i=0; i<nwritten; i++) {
        word = block1p[i];
        if (j == 0) {
            ltot = (int) word;
            if (ltot == 0) break;
        } else if (j == 16) {
            nlines++;
        } else if (j >= 45) {
            k = k + 2;
            if (k == 80) {
                nlines++;
                k = 0;
            }
        }
        j++;
        if (j == ltot) {
            j = 0;
        }
    }
    printf("The %s contains %d lines\n", infile, nlines);

    /* Extract the logical records */
    j = 0;    /* index into record */
    k = 0;    /* index into line */
    for (i=0; i<nwritten; i++) {
        word = block1p[i];
        if (j == 0) {
            ltot = (int) word;
            printf("ltot, nlines = %d %d\n", ltot, nlines);
            if (ltot == 0) break;
            fprintf(fpout, "%6d", nlines);
        } else if (j > 0 && j < 16) {
            if (j == 1) {
                krec = word;
                if (krec == 2001) {    /* Catalog record */
                    printf("Catalog record\n");
                    word = 2101;
                 } else if (krec == 3002) {    /* Header record */
                    printf("Header record\n");
                    word = 3101;
                }
            } else if (j == 12) {
                word = 16;
            }
            fprintf(fpout, "%6d", word);
        } else if (j == 16) {
            fputc('\n', fpout);
        } else if (j >= 45) {
            buf = (char *) &word;
            c = buf[0];
            if (c<32 || c>126) c='.';
            if (c == 10 || c == 13) {
                c=' ';
            }
            fputc(c,fpout);
            k++;
            c = buf[1];
            if (c<32 || c>126) c='.';
            if (c == 10 || c == 13) {
                c=' ';
            }
            fputc(c, fpout);
            k++;
            if (k == 80) {
                fputc('\n', fpout);
                k = 0;
            }
        }
        block2p[j] = block1p[i];
        j++;
        if (j == ltot) {
            printf("End of record - i, j = %d %d\n", i, j);
            j = 0;
        }
    }
    printf("Wrote %d lines to %s\n", nlines, outfile);
    return(0);
}
