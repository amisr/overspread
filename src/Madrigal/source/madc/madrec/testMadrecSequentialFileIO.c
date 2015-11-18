/*  $Id: testMadrecSequentialFileIO.c,v 1.4 2002/12/26 16:15:31 brideout Exp $ */

/*
modification history
--------------------
01a,27Mar00         Original
*/

/* 
*    Usage: testMadrecIO.c inputFile outputFile outputFileFormat
*
*    This program copies a Cedar file to another file containing the same
*    Cedar records. There are five supported formats:
*        0 - Madrigal
*        1 - Blocked Binary
*        2 - Cbf
*        3 - Unblocked Binary
*        4 - Ascii
*    The format of the input file is detected automatically. The format
*    of the output file is an input parameter. For example:
*        translateCedarFile mil980120g.002 mil980120g.cbf 2
*    translates mil980120g.002 into a Cray Blocked Format (cbf) file.
*   
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <madrec.h>
#include <cedar.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    Madrec *madrecrp,    /* Input file - any type */
           *madrecwp;    /* Output File - type 0-4 */
    char *infile, *outfile;
    int iotype, i, stat;
    static char *err2="Usage: testMadrecSequentialFileIO inputFile outputFile outputFileFormat";
    static char *err3="Error: Output file format must be in range 0-4";

    /* Get the input parameters */
    if (argc == 4) {
        infile = argv[1];
        outfile = argv[2];
        iotype = atoi(argv[3]);
        if (iotype < 0 || iotype > 4) {
            fprintf(stderr, "%s\n", err3);
            return(1);
        } else {
            iotype = 20 + iotype;
        }
    } else {
        fprintf(stderr, "%s\n", err2);
        return(1);
    }

  
    /* Create a madrec object for the input file */
    if ((madrecrp = madrecCreate()) == (Madrec *) NULL) {
        fprintf(stderr, "create madrecr: %s\n", madrecGetError(madrecrp));
        return(1);
    }

    /* Connect the input madrec object to a madrigal file */
    /* Chose opcode 1 to autodetect the file type */
    if (madrecOpen(madrecrp, 1, infile) != 0) {
        fprintf(stderr, "open madrecr: %s\n", madrecGetError(madrecrp));
        return(1);
    }

    /* Create a madrec object for the output file */
    if ((madrecwp = madrecCreate()) == (Madrec *) NULL) {
        fprintf(stderr, "create madrecw: %s\n", madrecGetError(madrecwp));
        return(1);
    }

    /* Connect the output madrec object to a madrigal file */
    if (madrecOpen(madrecwp, iotype, outfile) != 0) {
        fprintf(stderr, "open madrecw: %s\n", madrecGetError(madrecwp));
        return(1);
    }

    i = 0;
    while ((stat=madrecGetNextRec(madrecrp)) == 0) {
        if (madrecCopy(madrecrp, madrecwp) != 0) {
            fprintf(stderr, "copy madrecr: %s\n", madrecGetError(madrecrp));
            fprintf(stderr, "copy madrecw: %s\n", madrecGetError(madrecwp));
            return(1);
        }
        if ( madrecPutNextRec(madrecwp) != 0) {
            fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecwp));
            return(1);
        }
        i++;
    }

    if (stat != 0 && stat != -1) {
        printf("madrecGetNextRec error %d\n", stat);
        return(1);
    }

    printf("%d records copied from %s to %s\n", i, infile, outfile);

    if (madrecClose(madrecrp) != 0) {        
        fprintf(stderr, "close madrecr: %s\n", madrecGetError(madrecrp));
        return(1);
    }
    madrecDestroy(madrecrp);
    if (madrecClose(madrecwp) != 0) {        
        fprintf(stderr, "close madrecw: %s\n", madrecGetError(madrecwp));
        return(1);
    }
    madrecDestroy(madrecwp);

    return(0);
}
