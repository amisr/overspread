/*  $Id: testMadrecRandomFileIO.c,v 1.6 2002/12/26 16:15:31 brideout Exp $ */

/*
modification history
--------------------
01a,28Jan02         Original
*/

/* 
*    Usage: testMadrecRandomFileIO inputFile
*
*    This program tests madrec random IO. It reads input file and saves
*    the start time, end time and a checksum for each record. It then
*    does 1000 random reads and verifies  the start time, end time and 
*    checksum.  The random record number generate produces a few
*    out-of-bounds records, e.g. negative record numbers or record
*    numbers greater than the number of records in the file. This
*    program verifies that these can be caught correctly.
*   
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <madrec.h>
#include <cedar.h>
#include <date.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    Madrec *madrecrp,    /* Input file - any type */
           *madrecwp,    /* Output File - type 0-4 */
           *madrecmp;    /* unblocked memory image */
    char *infile, *outfile;
    int iotype, i, j, stat, nrec, recno, ltot, nin, nout, lprint,
        qerror=0,startIndex[10000], endIndex[10000];
    Int16 word, checkSum[10000], checkSumr;
    static char *err2="Usage: testMadrecRandomFileIO inputFile outputFile outputFileFormat printFlag";
    static char *err3="Error: Output file format must be in range 0-4";
    /* temp only */
    double testKey;

    /* Get the input parameters */
    if (argc == 5) {
        infile = argv[1];
        outfile = argv[2];
        iotype = atoi(argv[3]);
        lprint = atoi(argv[4]);
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

    /* Read the entire input file and rewind */
    i = 0;
    while ((stat=madrecGetNextRec(madrecrp)) == 0) {
        if (i > 9999) {
            fprintf(stderr, "input file too large\n");
            return(1);
        }
        startIndex[i] = cedarGetStartIndex(madrecrp->recordp);
        endIndex[i] = cedarGetEndIndex(madrecrp->recordp);
        ltot = cedarGetLtot(madrecrp->recordp);
        checkSum[i] = 0;
        for (j=0; j<ltot; j++) {
            word = (Int16)(madrecrp->recordp)[j];
            checkSum[i] = checkSum[i]^word;
        }
        i++;
    }
    nrec = i;
    if (lprint) {
        printf("%d records read from %s\n", i, madrecrp->filnam);
    }
    madrecRewind(madrecrp);
    if (lprint) {
        printf("%s rewound\n", madrecrp->filnam);
    }

    /* Generate and print input file key table */
    /* madrecGenKeys(madrecrp); */
    /* madrecPrintKeys(madrecrp); */

    nin = 0;
    nout = 0;
    srand(999);
    for (i=0; i<nrec; i++) {

        /* Expand record range to test catch of out-of-bound records */
        recno = (nrec+10)*((double)rand()/(double)RAND_MAX) - 5;
        if (lprint) {
            printf("Record %4d - ", recno);
        }

        /* Get the specified record */
        if ((stat=madrecGetRecByRecno(madrecrp, recno)) != 0) {
            if (recno < 0 || recno > nrec-1) {
                if (lprint) {
                    printf("OK - caught out-of-bounds record. stat = %d\n", stat);
                }
                nout++;
                continue;
            }
            printf("getRecordByRecno: %s\n", madrecGetError(madrecrp));
            printf("Error reading record %d\n", recno);
            return(1);
        }

        /* Get checksum */
        ltot = cedarGetLtot(madrecrp->recordp);
        checkSumr = 0;
        for (j=0; j<ltot; j++) {
            word = (Int16)(madrecrp->recordp)[j];
            checkSumr = checkSumr^word;
        }

        /* Print record information*/
        /*
        printf("recno,start key,end key, pos = %d %d %d %d %d\n", recno,
            cedarGetStartIndex(madrecrp->recordp),
            cedarGetEndIndex(madrecrp->recordp),
            ftell(madrecrp->fp), madrecrp->pos);
        */
        if (startIndex[recno] == cedarGetStartIndex(madrecrp->recordp) &&
            endIndex[recno] == cedarGetEndIndex(madrecrp->recordp) &&
            checkSum[recno] == checkSumr) {
            if (lprint) {
                printf("OK\n");
            }
            nin++;
        } else {
            qerror = 1;
            if (lprint) {
                printf("Error\n");
            }
        }

    }

    if (!qerror) {
        printf("    OK - Read %d records - %d good records, %d bad records, %d out-of-bounds records\n",
            i, nin, nrec-nin-nout, nout);
    } else {
        printf("    ERROR - Read %d records - %d good records, %d bad records, %d out-of-bounds records\n",
            i, nin, nrec-nin-nout, nout);
    }

    /* temp only - test of madrecGetRecByKey */
    madrecRewind(madrecrp);
    /* generate a key for 1/20/1998 15:30:00 */
    testKey = dmadptr(1998, 120, 1530, 0);
    /* Get the specified record */
    if ((stat=madrecGetRecByKey(madrecrp, testKey)) != 0){
        fprintf(stderr, "madrecGetRecByKey: %s\n", madrecGetError(madrecrp));
    } else
        printf("    OK - madrecGetRecByKey passed.\n");

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
     madrecDestroy(madrecmp);

    return(0);
}
