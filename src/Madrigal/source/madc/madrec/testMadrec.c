/*  $Id: testMadrec.c,v 1.11 2003/07/17 19:02:28 brideout Exp $ */

/*
modification history
--------------------
00a,22Apr96         original
*/

/* 
* USAGE: testMadrec
*    This program is a simple example illustrating the use of the madrec
*    library. It is hardwired to open and process
*    $MADROOT/experiments/1998/mlh/20jan98/mil980120g.002. 
* 
*    Supported formats:
*        0 - Madrigal
*        1 - Blocked Binary
*        2 - Cbf
*        3 - Unblocked Binary
*        4 - Ascii
*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <date.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    Madrec *madrecp;
    double key; 
    int status;
    char madfile[128] = "";

    /* Get the name of the madrigal file */
    if (argc == 1) {
        cedarGetMadroot(madfile);
        strcat(madfile, "/experiments/1998/mlh/20jan98/mil980120g.002");
    } else if (argc == 2) {
        strcpy(madfile, argv[1]);
    } else {
        return(-1);
    }

    /* Create a madrec object */
    madrecp = madrecCreate();
    printf("create: %s\n", madrecGetError(madrecp));

    /* Connect the madrec object to a madrigal file */
    madrecOpen(madrecp, 30, madfile);
    printf("open: %s\n", madrecGetError(madrecp));
    if (strcmp(madrecGetError(madrecp), "No errors"))
    {
        printf("The standard test files need to be installed for this test to run - see www.openmadrigal.org.\n");
        return -1;
    }

    /* specify a record by date and time */
    key = getKey(1998, 1, 20, 15, 0, 0);

    /* Get the specified record */
    status = madrecGetRecByKey(madrecp, key);
    printf("getRecordByKey - time 1500: %s\n", madrecGetError(madrecp));

    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n\n", madrecGetError(madrecp));

    /* get a record by recno */
    status = madrecGetRecByRecno(madrecp, 27);
    printf("getRecByRecno - rec 27 time 1441: %s\n", madrecGetError(madrecp));

    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n\n", madrecGetError(madrecp));

    /* specify a record by new date and time */
    key = getKey(1998, 1, 20, 16, 0, 0);

    /* Get the specified record */
    status = madrecGetRecByKey(madrecp, key);
    printf("getRecordByKey - time 1600: %s\n", madrecGetError(madrecp));

    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n\n", madrecGetError(madrecp));

    /* now try sorting */
    madrecGetSortedRecnoList(madrecp);
    /* print the list 
    for (i=0; i<madrecp->nrecords; i++)
    {
        printf("Next record: %i\n", madrecp->sortedRecnoList[i]);
    }*/

    /* now print records according to sort order */
    status = madrecGetRecByRecno(madrecp, madrecp->sortedRecnoList[0]);
    printf("madrecGetRecByRecno for sorted rec 0: %s\n", madrecGetError(madrecp));
    
    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n\n", madrecGetError(madrecp));

    status = madrecGetRecByRecno(madrecp, madrecp->sortedRecnoList[10]);
    printf("madrecGetRecByRecno for sorted rec 10 - time 1405: %s\n", madrecGetError(madrecp));
    
    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n\n", madrecGetError(madrecp));
    
    /* Disconnect the madrigal file */
    madrecClose(madrecp);
    printf("close: %s\n", madrecGetError(madrecp));

    /* Destroy the madrec object */
    madrecDestroy(madrecp);

    return(0);
}
