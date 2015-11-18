/*  $Id: testMadrecMemIO.c,v 1.6 2002/12/26 16:15:31 brideout Exp $ */

/*
modification history
--------------------
01a,28Jan02         Original
*/

/* 
*    Usage: testMadrecMemIO inputFile
*
*    This program tests the madrec interface to unblocked binary file
*    memory images (file type 25).
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
           *madrecwp,    /* Output File - type 0-4 */
           *madrecmp;    /* unblocked memory image */
    char *infile, *outfile;
    int iotype, i, stat, recno, yr, mo, dy, hr, mn, sc, cs;
    static char *err2="Usage: testMadrecMemIO inputFile outputFile outputFileFormat";
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

    /* Create a madrec object for the memory image */
    if ((madrecmp = madrecCreate()) == (Madrec *) NULL) {
        fprintf(stderr, "create madrecm: %s\n", madrecGetError(madrecmp));
        return(1);
    }

    /* Connect the memory madrec object to an unblocked memory image */
    if (madrecOpen(madrecmp, 25, " ") != 0) {
        fprintf(stderr, "open madrecm: %s\n", madrecGetError(madrecwp));
        return(1);
    }

    /* Read the entire input file and rewind */
    i = 0;
    while ((stat=madrecGetNextRec(madrecrp)) == 0) {
        /*
        printf("i,start key,end key, pos = %d %d %d %d %d\n", i,
            cedarGetStartIndex(madrecrp->recordp),
            cedarGetEndIndex(madrecrp->recordp),
            ftell(madrecrp->fp), madrecrp->pos);
        */
         /* cedarPrintProlog(madrecrp->recordp); */
        i++;
    }
    printf("%d records read from %s\n", i, madrecrp->filnam);
    madrecRewind(madrecrp);
    printf("%s rewound\n", madrecrp->filnam);

    /* Copy the input file to memory */
    printf("Copy the input file to memory\n");
    madrecRewind(madrecrp);
    i = 0;
    while ((stat=madrecGetNextRec(madrecrp)) == 0) {
        cedarGetStartTime(madrecrp->recordp, &yr, &mo, &dy, &hr, &mn, &sc, &cs);
        printf("%d %f %f %d %d %d %d %d %d %d\n", i,
            cedarGetStartIndex(madrecrp->recordp),
            cedarGetEndIndex(madrecrp->recordp),
            yr, mo, dy, hr, mn, sc, cs);
        if (madrecCopy(madrecrp, madrecmp) != 0) {
            fprintf(stderr, "copy madrecr: %s\n", madrecGetError(madrecrp));
            fprintf(stderr, "copy madrecm: %s\n", madrecGetError(madrecmp));
            return(1);
        }
        if ( madrecPutNextRec(madrecmp) != 0) {
            fprintf(stderr, "putNextRec madrecm: %s\n", madrecGetError(madrecmp));
            return(1);
        }
        i++;
    }
    if (stat != 0 && stat != -1) {
        printf("madrecGetNextRec error %d\n", stat);
        return(1);
    }
    printf("%d records copied from %s to %s\n", i, infile, "memory");

    /* Print memory image record information */
    printf("Read the memory image\n");
    madrecRewind(madrecmp);
    while ((stat=madrecGetNextRec(madrecmp)) == 0) {
        cedarGetStartTime(madrecmp->recordp, &yr, &mo, &dy, &hr, &mn, &sc, &cs);
        printf("%i %f %f %d %d %d %d %d %d %d\n", i,
            cedarGetStartIndex(madrecmp->recordp),
            cedarGetEndIndex(madrecmp->recordp),
            yr, mo, dy, hr, mn, sc, cs);
    }

    /* Generate and print input file key table */
    madrecGenKeys(madrecmp);
    printf("Memory file key table\n");
    madrecPrintKeys(madrecmp);

    /* specify a record number */
    recno = 8;

    /* Get the specified record */
    madrecGetRecByRecno(madrecmp, recno);
    printf("getRecordByRecno: %s\n", madrecGetError(madrecmp));

    /* Print the record */
    cedarGetStartTime(madrecmp->recordp, &yr, &mo, &dy, &hr, &mn, &sc, &cs);
    printf("%d %f %f %d %d %d %d %d %d %d\n", i,
        cedarGetStartIndex(madrecmp->recordp),
        cedarGetEndIndex(madrecmp->recordp),
        yr, mo, dy, hr, mn, sc, cs);

    /* Print memory image record information */
    printf("Read the memory image\n");
    madrecRewind(madrecmp);
    while ((stat=madrecGetNextRec(madrecmp)) == 0) {
        printf("i,start key,end key, pos = %d %f %f %d\n", i,
            cedarGetStartIndex(madrecmp->recordp),
            cedarGetEndIndex(madrecmp->recordp),
            madrecmp->pos);
    }

    /* Copy the memory image to the output file */
    printf("Copy the memory image to the output file\n");
    madrecRewind(madrecmp);
    i = 0;
    while ((stat=madrecGetNextRec(madrecmp)) == 0) {
        printf("i,start key,end key, pos = %d %f %f %d\n", i,
            cedarGetStartIndex(madrecmp->recordp),
            cedarGetEndIndex(madrecmp->recordp),
            madrecrp->pos);
        if (madrecCopy(madrecmp, madrecwp) != 0) {
            fprintf(stderr, "copy madrecm: %s\n", madrecGetError(madrecmp));
            fprintf(stderr, "copy madrecw: %s\n", madrecGetError(madrecwp));
            return(1);
        }
        printf("i,start key,end key, pos = %d %f %f %d\n", i,
            cedarGetStartIndex(madrecwp->recordp),
            cedarGetEndIndex(madrecwp->recordp),
            madrecwp->pos);
        if (madrecPutNextRec(madrecwp) != 0) {
            fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecwp));
            return(1);
        }
        i++;
    }
    if (stat != 0 && stat != -1) {
        printf("madrecGetNextRec error %d\n", stat);
        return(1);
    }

    printf("%d records copied from %s to %s\n", i, "memory", outfile);

    /* Close and delete madrec objects */
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
    if (madrecClose(madrecmp) != 0) {        
        fprintf(stderr, "close madrecm: %s\n", madrecGetError(madrecmp));
        return(1);
    }
    madrecDestroy(madrecmp);

    return(0);
}
