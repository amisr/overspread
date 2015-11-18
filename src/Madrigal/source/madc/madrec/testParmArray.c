/*  $Id: testParmArray.c,v 1.4 2002/12/26 16:15:31 brideout Exp $ */

/*
modification history
--------------------
00a,22Apr00         original
*/

/* 
* USAGE: testParmArray cedarFile fileType
*    e.f.: testParmArray mil991102g.001 3
*    This program is a simple example illustrating the use of the madrec
*    library. It is hardwired to open and process
*    $MADROOT/experiments/1998/mlh/20jan98/mil980120g.002. 
* 
*    Supported formats:
*
*       Open Cedar file for sequential reading:
*         1 - Determine file type automatically
*        10 - Madrigal
*        11 - Blocked Binary
*        12 - Cbf
*        13 - Unblocked Binary
*        14 - Ascii
*
*       Open Cedar file for random reading using memory image of file; 
*         3 - Determine file type automatically
*        30 - Madrigal file
*        31 - Blocked Binary file
*        32 - Cbf file
*        33 - Unblocked Binary file
*        34 - Ascii file
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

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    Madrec *madrecp;
    char madfile[128];
    int iotype, i, j, k, nlines, nrecs, ind;

    double *parp;
    Ffspec fspec;
    int pparms[7] = {34, 120, 132, 142, 550, 10, 160};

    int fparms[4] = {34, 120, 142, 110};
    double fmin[4] = { 2.0, 200.0,  80.0, 300.0};
    double fmax[4] = { 2.5, 600.0,  89.0, 400.0};


    /* Get the name of the madrigal file */
    if (argc != 3) {
        cedarGetMadroot(madfile);
        strcat(madfile, "/experiments/1998/mlh/20jan98/mil980120g.002");
        iotype = 1;
    } else {
        strcpy(madfile, argv[1]);
        iotype = atoi(argv[2]);
    }

    /* Create a madrec object */
    madrecp = madrecCreate();
    printf("create: %s\n", madrecGetError(madrecp));

    /* Connect the madrec object to a madrigal file */
    madrecOpen(madrecp, iotype, madfile);
    printf("open: %s\n", madrecGetError(madrecp));

    /* Specify cedarGetParmArray arguments */
    fspec.nparms = 7;
    fspec.pparms = pparms;  
    fspec.nfilters = 4;
    fspec.fparms = fparms;
    fspec.fmin = fmin;
    fspec.fmax = fmax;


    /* Read the file */
    nrecs = 999999;
    for (j=0; j<nrecs; j++) {
        ind = madrecGetNextRec(madrecp);
        if (ind != 0) {
            if (ind == -1)
                break;
            printf("Error reading cedar file\n");
            return(0);
        }
        parp = cedarGetParmArray(madrecp->recordp, &fspec, &nlines);
        for (i=0; i<nlines; i++) {
            for (k=0; k<fspec.nparms; k++) {
                printf("%12.5e", parp[i+nlines*k]);
            }
            printf("\n");
        }
        free(parp);
    }

    /* Print the record */
    /*
    cedarPrintRecord(madrecp->recordp);
    printf("printRecord: %s\n", madrecGetError(madrecp));
    */

    /* Disconnect the madrigal file */
    madrecClose(madrecp);
    printf("close: %s\n", madrecGetError(madrecp));

    /* Destroy the madrec object */
    madrecDestroy(madrecp);

    return(0);
}
