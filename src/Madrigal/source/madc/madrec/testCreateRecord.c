/*  $Id: testCreateRecord.c,v 1.4 2005/04/05 20:11:44 brideout Exp $ */

/*
modification history
--------------------
01a,1Feb02         Original
*/

/* 
*    Usage: testCreateRecord
*    This program
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
    Madrec *madrecp;    /* Output File - type 0-4 */
    int iotype=0, stat=0, record=0, i=0;
    int lprol=0, jpar=0, mpar=0, nrow=0, krec=0, kinst=0, kindat=0, ibyr=0, 
        ibmo=0, ibdy=0, ibh=0, ibm=0, ibs=0, ibcs=0, ieyr=0, iemo=0, iedy=0, 
        ieh=0, iem=0, ies=0, iecs=0;
    double kp[8] = {2.0, 3.3, 4.0, 3.0, 8.0, 7.7, 6.3, 5.0};
    double ap[8] = {10.0, 15.0, 16.0, 15.0, 24.0, 21.0, 18.0, 15.0};
    char text[161] = "";


    /* Get the input parameters */
/*
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
*/
  

    /* Create a madrec object for the output file */
    if ((madrecp = madrecCreate()) == (Madrec *) NULL) {
        fprintf(stderr, "create madrecw: %s\n", madrecGetError(madrecp));
        return(1);
    }

    /* Connect the output madrec object to a madrigal file */
    iotype = 20;
    stat = madrecOpen(madrecp, iotype, "madout");
    fprintf(stderr, "open madrecw: %s\n", madrecGetError(madrecp));
    
    /* first, add a catalog record - lenght must be 80*n */
    /* create some text */
    text[160] = '\0';
    for (i=0; i<160; i++)
        text[i] = ' ';
    strcpy(text, "This is a catalog record line 0");
    text[strlen("This is a catalog record line 0")] = ' ';
    strcpy(text + 80, "This is a catalog record line 1");
    text[80 + strlen("This is a catalog record line 1")] = ' ';
    madrecp->recordp = cedarCreateCatalogRecord(31, 30007,
                                2001, 8, 20,
                                0, 0, 0, 0,
                                2001, 8, 21,
                                23, 59, 59, 99,
				text);
				
    /* append some more text to the catalog record */
    for (i=0; i<160; i++)
        text[i] = ' ';
    strcpy(text, "This is a catalog record line 2");
    text[strlen("This is a catalog record line 2")] = ' ';
    strcpy(text + 80, "This is a catalog record line 3");
    text[80 + strlen("This is a catalog record line 3")] = ' ';
    stat = cedarAppendCatalogRecord(&(madrecp->recordp), text);
				
    stat = madrecPutNextRec(madrecp);
    fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecp));
    free(madrecp->recordp);
    
    /* next, add a header record - lenght must be 80*n */
    /* create some text */
    for (i=0; i<160; i++)
        text[i] = ' ';
    strcpy(text, "This is a header record line 0");
    text[strlen("This is a header record line 0")] = ' ';
    strcpy(text + 80, "This is a header record line 1");
    text[80 + strlen("This is a header record line 1")] = ' ';
    madrecp->recordp = cedarCreateHeaderRecord(31, 30007,
                                2001, 8, 20,
                                0, 0, 0, 0,
                                2001, 8, 21,
                                23, 59, 59, 99,
				2,2,
				text);
				
    /* append some more text to the header record */
    for (i=0; i<160; i++)
        text[i] = ' ';
    strcpy(text, "This is a header record line 2");
    text[strlen("This is a header record line 2")] = ' ';
    strcpy(text + 80, "This is a header record line 3");
    text[80 + strlen("This is a header record line 3")] = ' ';
    stat = cedarAppendHeaderRecord(&(madrecp->recordp), text);
				
    stat = madrecPutNextRec(madrecp);
    fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecp));
	
    lprol = 16;
    jpar = 3;
    mpar = 2;
    nrow = 8;
    krec = 1002;
    kinst = 210;
    kindat = 30007;
    ibyr = 2001;
    ibmo = 8;
    ibdy = 20;
    ibh = 0;
    ibm = 0;
    ibs = 0;
    ibcs = 0;
    ieyr = 2001;
    iemo = 8;
    iedy = 20;
    ieh = 23;
    iem = 59;
    ies = 59;
    iecs = 59;
    for (record=0; record<5; record++) {

        ibdy++;
        iedy++;

	/* Create a Cedar record in the madrec object */
        if (madrecp->recordp != (Int16 *)NULL) {
            free(madrecp->recordp);
        }
	madrecp->recordp = cedarCreateRecord(lprol, jpar, mpar, nrow, krec,
					     kinst, kindat, ibyr, ibmo, ibdy, 
					     ibh, ibm, ibs, ibcs, ieyr,
					     iemo, iedy, ieh, iem, ies,
					     iecs);
    
	/* Set 1d parameters */
	stat = cedarSet1dParm(madrecp->recordp, 340, 12.0, 0);
	stat = cedarSet1dParm(madrecp->recordp, 354, 1.5e-20, 1);
	stat = cedarSet1dParm(madrecp->recordp, 356, 1.2e-20, 2);
    
	/* Set 2d parms */
	stat = cedarSet2dParm(madrecp->recordp, 310, kp, 0);
	stat = cedarSet2dParm(madrecp->recordp, 335, ap, 1);
    
	/* cedarPrintRecord(madrecp->recordp); */
    
	stat = madrecPutNextRec(madrecp);
	fprintf(stderr, "putNextRec madrecw: %s\n", madrecGetError(madrecp));

    }

    stat = madrecClose(madrecp);      
    fprintf(stderr, "close madrecw: %s\n", madrecGetError(madrecp));

    madrecDestroy(madrecp);

    return(0);
}
