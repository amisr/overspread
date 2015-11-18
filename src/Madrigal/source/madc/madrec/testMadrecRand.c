/*  $Id: testMadrecRand.c,v 1.7 2002/12/26 16:15:31 brideout Exp $ */

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
    char *madfile;
    double key;

    /* Get the name of the madrigal file */
    if (argc == 1) {
        madfile = (char *)malloc(128);
        cedarGetMadroot(madfile);
        strcat(madfile, "/experiments/1998/mlh/20jan98/mil980120g.002");
    } else if (argc == 2) {
        madfile = (char *)malloc(128);
        strcpy(madfile, argv[1]);
    } else {
        return(-1);
    }

    /* Create a madrec object */
    madrecp = madrecCreate();
    printf("create: %s\n", madrecGetError(madrecp));

    /* Connect the madrec object to a madrigal file */
    madrecOpen(madrecp, 1, madfile);
    printf("open: %s\n", madrecGetError(madrecp));

    /* specify a record by date and time */
    key = getKey(2001, 3, 3, 24, 59, 60);

    /* Get the specified record */
    madrecGetRecByKey(madrecp, key);
    printf("getRecordByKey: %s\n", madrecGetError(madrecp));

    /* Print the record */
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n", madrecGetError(madrecp));

    /* Disconnect the madrigal file */
    madrecClose(madrecp);
    printf("close: %s\n", madrecGetError(madrecp));

    /* Destroy the madrec object */
    madrecDestroy(madrecp);

    return(0);
}
