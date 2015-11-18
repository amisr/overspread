/*  $Id: Mad.c,v 1.14 2009/04/29 19:10:39 brideout Exp $ */

/*
modification history
0.00a, 23apr95,jmh    original
*/

/*
DESCRIPTION
*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <tcl.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <date.h>

Tcl_HashTable mad_table;
char buf[100000];
char buf1[1024];

int
Mad_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    int MadCmd(), CedarCodeCmd(), MadGetKey(), MadJday(), MadJdater();

    Tcl_CreateCommand(interp, "mad", MadCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "cedarCode", CedarCodeCmd,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "getKey", MadGetKey,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "jday", MadJday,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "jdater", MadJdater,
        (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_InitHashTable(&mad_table, TCL_STRING_KEYS);

    return TCL_OK;
}


/*************************************************************************
                              Mad Command
*************************************************************************/

int
MadCmd(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[])
{
    static unsigned int id=1;
    int new;
    Tcl_HashEntry *entryp;
    Madrec *madrecp;
    Madrec *madrecCreate();
    int Mad_ObjectCmd();
    void Mad_DestroyCmd();

    /* Check command line arguments */
    if (argc != 1 && argc != 2) {
        sprintf (interp->result,
                 "mad requires 0 or 1 arguments");
        return TCL_ERROR;
    }

    /* Create mad object */
    madrecp = madrecCreate();

    /* Set mad key */
    sprintf(interp->result, "mad%u", id);
    id++;

    /* Create hash table entry */
    entryp = Tcl_CreateHashEntry(&mad_table, interp->result, &new);
    Tcl_SetHashValue(entryp, madrecp);

    /* Create new tcl command */
    Tcl_CreateCommand(interp, interp->result, Mad_ObjectCmd,
        (ClientData) madrecp, (Tcl_CmdDeleteProc *) Mad_DestroyCmd);

    /* If command has argument, set it to the mad key */
    if (argc == 2) {
        Tcl_SetVar(interp, argv[1], interp->result, 0);
    }

    return TCL_OK;
}


void Mad_DestroyCmd(ClientData clientData)
{
    Madrec *madrecp;
    int madrecDestroy(Madrec *);

    /* Destroy mad file */
    madrecp = (Madrec *) clientData;
    (void) madrecDestroy(madrecp);
}


int Mad_ObjectCmd(ClientData clientData, Tcl_Interp *interp,
                  int argc, char *argv[])
{
    Madrec *madrecp;
    int status=0, jpar=0, mpar=0, i=0, j=0;
    char **pcode1d, **pcode2d;
    int parcode1d[32], parcode2d[32];
    Ffspec fspec;
    int npparm=0, nfparm=0, nfmin=0, nfmax=0, nlines=0;
    char **pparm, **fparm, **fmin, **fmax;
    double *parp;
    Int16 *cedarp=(Int16 *)NULL;
    int Mad_CopyCmd(Madrec *madrec1p, char *mad2),
        Mad_GetCmd(Tcl_Interp *interp, Madrec *madrecp, int argc, char *argv[]),
        Mad_SetCmd(Tcl_Interp *interp, Madrec *madrecp, int argc, char *argv[]);

    madrecp = (Madrec *) clientData;

    if (argc < 2) {
        sprintf(interp->result, "%s requires at least one argument", 
                argv[0]);
        return TCL_ERROR;
    }


    if (!strcmp(argv[1], "destroy")) {
        Tcl_DeleteCommand(interp, argv[0]);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "open")) {
        if (argc != 4) {
            sprintf(interp->result,
                    "%s open requires two arguments", argv[0]);
            return TCL_ERROR;
        }
        status = madrecOpen(madrecp, atoi(argv[2]), argv[3]);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "close")) {
        status = madrecClose(madrecp);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "checkFile")) {
        status = madrecCheckFile(madrecp);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }
    
    else if (!strcmp(argv[1], "hasCatalog")) {
        status = madrecHasCatalog(madrecp);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            interp->result = "1";
            return TCL_OK;
        }
    }
    
    else if (!strcmp(argv[1], "hasHeader")) {
        status = madrecHasHeader(madrecp);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            interp->result = "1";
            return TCL_OK;
        }
    }

    else if (!strcmp(argv[1], "getNextRecord")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s getNextRecord requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = madrecGetNextRec(madrecp);
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "putNextRecord")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s putNextRecord requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = madrecPutNextRec(madrecp);
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "getPreviousRecord")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s getPreviousRecord requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = madrecGetPreviousRec(madrecp);
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "getRecordByRecno")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s getRecordByRecno requires one argument", argv[0]);
            return TCL_ERROR;
        }
        status = madrecGetRecByRecno(madrecp, atoi(argv[2]));
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "getRecordByKey")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s getRecordByKey requires one argument", argv[0]);
            return TCL_ERROR;
        }
        status = madrecGetRecByKey(madrecp, atoi(argv[2]));
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "rewind")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s rewind requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = madrecRewind(madrecp);
        if (status == 0 || status == -1) {
            sprintf(interp->result, "%d", status);
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "copy")) {
        status = Mad_CopyCmd(madrecp, argv[2]);
        if (status == 100) {
            sprintf(interp->result,
                    "\"%s %s\" has bad source mad - %s",
                    argv[0],argv[1], argv[2]);
            return TCL_ERROR;
        }
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "checkRecord")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s checkRecord requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = cedarCheckRecord(madrecp->recordp);
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }
/*
    else if (!strcmp(argv[1], "parmCodeArray")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        jpar = cedarGetJpar(madrecp->recordp);
        mpar = cedarGetMpar(madrecp->recordp);
        nrow = cedarGetNrow(madrecp->recordp);
        kpar = cedarGetKpar(madrecp->recordp);
        outp = cedarGetParmCodeArray(madrecp->recordp);
        buf[0] = '\0';
        for (i=0; i<jpar+mpar+kpar; i++) {
            sprintf(buf1, "%6d ", outp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }
*/
    else if (!strcmp(argv[1], "parmArray")) {
        if (argc != 6) {
            sprintf(interp->result,
                    "%s parmArray requires four arguments", argv[0]);
            return TCL_ERROR;
        }
        if (Tcl_SplitList(interp, argv[2], &npparm, &pparm) != TCL_OK) {
            return TCL_ERROR;
        }
        if (Tcl_SplitList(interp, argv[3], &nfparm, &fparm) != TCL_OK) {
            return TCL_ERROR;
        }
        if (Tcl_SplitList(interp, argv[4], &nfmin, &fmin) != TCL_OK) {
            return TCL_ERROR;
        }
        if (Tcl_SplitList(interp, argv[5], &nfmax, &fmax) != TCL_OK) {
            return TCL_ERROR;
        }
        if (nfparm != nfmin || nfparm != nfmax) {
            sprintf(interp->result,
                    "%s filter parm, min and max lists must be same length",
                    argv[0]);
            return TCL_ERROR;
        }

        fspec.nparms = npparm;
        fspec.nfilters = nfparm;
        fspec.pparms = (int *)malloc(npparm*sizeof(int));
        for (i=0; i<npparm; i++) {
            fspec.pparms[i] = atoi(pparm[i]);
        }
        fspec.fparms = (int *)malloc(nfparm*sizeof(int));
        for (i=0; i<nfparm; i++) {
            fspec.fparms[i] = atoi(fparm[i]);
        }
        fspec.fmin = (double *)malloc(nfmin*sizeof(double));
        for (i=0; i<nfmin; i++) {
            fspec.fmin[i] = atof(fmin[i]);
        }
        fspec.fmax = (double *)malloc(nfmax*sizeof(double));
        for (i=0; i<nfmax; i++) {
            fspec.fmax[i] = atof(fmax[i]);
        }
        parp = cedarGetParmArray(madrecp->recordp, &fspec, &nlines);

        buf[0] = '\0';
        for (i=0; i<nlines; i++) {
            for (j=0; j<fspec.nparms; j++) {
                sprintf(buf1, "%13.5e", parp[i+nlines*j]);
                strcat(buf, buf1);
            }
        }
        sprintf(buf1, "\n");
        interp->result = buf;

        Tcl_Free((char *)pparm);
        Tcl_Free((char *)fparm);
        Tcl_Free((char *)fmin);
        Tcl_Free((char *)fmax);
        free(fspec.pparms);
        free(fspec.fparms);
        free(fspec.fmin);
        free(fspec.fmax);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "printProlog")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s printProlog requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        status = cedarPrintProlog(madrecp->recordp);
        if (status != 0) {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_OK;
        }
    }

    else if (!strcmp(argv[1], "createRecord")) {
        if (argc != 23) {
            sprintf(interp->result,
                    "%s createRecord requires 21 arguments", argv[0]);
            return TCL_ERROR;
        }
        if (madrecp->recordp != (Int16 *)NULL) {
            free(madrecp->recordp);
        }
        madrecp->recordp =
            cedarCreateRecord(
                atoi(argv[ 2]),atoi(argv[ 3]),atoi(argv[ 4]),atoi(argv[ 5]),
                atoi(argv[ 6]),atoi(argv[ 7]),atoi(argv[ 8]),atoi(argv[ 9]),
                atoi(argv[10]),atoi(argv[11]),atoi(argv[12]),atoi(argv[13]),
                atoi(argv[14]),atoi(argv[15]),atoi(argv[16]),atoi(argv[17]),
                atoi(argv[18]),atoi(argv[19]),atoi(argv[20]),atoi(argv[21]),
                atoi(argv[22]));


        status = 0;
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "printRecord")) {
        if (argc < 2 || argc > 5) {
            sprintf(interp->result,
                    "%s printRecord requires zero or one arguments", 
                    argv[0]);
            return TCL_ERROR;
        }
        if (argc == 2) {
            status = cedarPrintRecord(madrecp->recordp);
        }
        else if (argc == 3) {
            printf("argv[2] = %s\n", argv[2]);
            if (!strcmp(argv[2], "-d")) {
                status = cedarDecimalPrintRecord(madrecp->recordp);
            }
            else if (!strcmp(argv[2], "-h")) {
                status = cedarHexPrintRecord(madrecp->recordp);
            }
        }
        else {
            if (argc == 4 || argc == 5) {
                if (Tcl_SplitList(interp, argv[argc-2], &jpar,
                        &pcode1d) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (jpar > 32) jpar=32;
                for (i=0; i<jpar; i++) {
                    parcode1d[i] = atoi(pcode1d[i]);
                }               
                if (Tcl_SplitList(interp, argv[argc-1], &mpar,
                        &pcode2d) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (mpar > 32) mpar=32;
                for (i=0; i<mpar; i++) {
                    parcode2d[i] = atoi(pcode2d[i]);
                }
                /* 
                madrecp->recordp[16] = (Int16)madrecGetBlockNumber(madrecp);
                */
                cedarp = 0;
            }
            if (argc == 4) {
               status = cedarPrintRecord(cedarp);
            }
            else if (argc == 5) {
                if (!strcmp(argv[2], "-d")) {
                    status = cedarDecimalPrintRecord(cedarp);
                }
                else if (!strcmp(argv[2], "-h")) {
                    status = cedarHexPrintRecord(cedarp);
                }
            }
        }
        if (status == 0) {
            interp->result = "0";
            return TCL_OK;
        } else {
            sprintf(interp->result, madrecGetError(madrecp));
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "get")) {
	if (argc < 3) {
	    sprintf(interp->result,
                    "%s %s requires at least two arguments", 
                    argv[0], argv[1]);
	    return TCL_ERROR;
	}
        status = Mad_GetCmd(interp, madrecp, argc-2, &argv[2]);
        if (status == 0) {
            interp->result = buf;
            return TCL_OK;
        }
        if (status == 100) {
            sprintf(interp->result,
                    "\"%s %s %s\" has wrong number of arguments",
                    argv[0],argv[1],argv[2]);
            return TCL_ERROR;
        }
        else if (status == 200) {
            sprintf(interp->result,
                    "Unknown \"%s %s\" argument - %s",
                    argv[0],argv[1],argv[2]);
            return TCL_ERROR;
        } else {
            return TCL_ERROR;
        }
    }

    else if (!strcmp(argv[1], "set")) {
	if (argc < 4) {
	    sprintf(interp->result,
                    "%s %s requires at least three arguments", 
                    argv[0], argv[1]);
	    return TCL_ERROR;
	}
        status = Mad_SetCmd(interp, madrecp, argc-2, &argv[2]);
        if (status == 0) {
            interp->result = buf;
            return TCL_OK;
        }
        if (status == 100) {
            sprintf(interp->result,
                    "\"%s %s %s\" has wrong number of arguments",
                    argv[0],argv[1],argv[2]);
            return TCL_ERROR;
        }
        else if (status == 200) {
            sprintf(interp->result,
                    "Unknown \"%s %s\" argument - %s",
                    argv[0],argv[1],argv[2]);
            return TCL_ERROR;
        } else {
            return TCL_ERROR;
        }
    }

    else {
        sprintf(interp->result, "Unknown %s argument - %s",
                argv[0],argv[1]);
        return TCL_ERROR;
    }

    sprintf(interp->result, "Fell off end of Mad_ObjectCmd");
    return TCL_ERROR;
}


int
Mad_GetCmd(Tcl_Interp *interp, Madrec *madrecp, int argc, char *argv[])
{
    int status, nrow, i, year, month, day,
        hour, minute, second, centisecond, jpar, mpar;
    Int16 *soutp;
    int out, *outp;
    double dout, *doutp;
    char *cout;

    /* Get mad parameters */

    if (!strcmp(argv[0], "blockNumber")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        /*
        out = madrecGetBlockNumber(madrecp);
        */
        out = 666;
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "numBlocks")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        /*
        out = madrecGetNumBlocks(madrecp);
        */
        out = 666;
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "fileType")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = madrecGetFileType(madrecp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "missing")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        dout = madrecGetMissing(madrecp);
        sprintf(buf, "%e", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "error")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        cout = madrecGetError(madrecp);
        sprintf(buf, "%s", cout);
        status = 0;
    }

    else if (!strcmp(argv[0], "numParms")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = madrecGetNumParms(madrecp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "parmsList")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        outp = madrecGetParmsList(madrecp);
        buf[0] = '\0';
        for (i=0; i<madrecGetNumParms(madrecp); i++) {
            sprintf(buf1, "%5d ", outp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }

    else if (!strcmp(argv[0], "parmLoc")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        outp = madrecGetParmLoc(madrecp);
        buf[0] = '\0';
        for (i=0; i<madrecGetNumParms(madrecp); i++) {
            sprintf(buf1, "%2d ", outp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }

    else if (!strcmp(argv[0], "parmMin")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        doutp = madrecGetParmMin(madrecp);
        buf[0] = '\0';
        for (i=0; i<madrecGetNumParms(madrecp); i++) {
            sprintf(buf1, "%13.5e ", doutp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }

    else if (!strcmp(argv[0], "parmMax")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        doutp = madrecGetParmMax(madrecp);
        buf[0] = '\0';
        for (i=0; i<madrecGetNumParms(madrecp); i++) {
            sprintf(buf1, "%13.5e ", doutp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }

    else if (!strcmp(argv[0], "sortedRecnoList")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        outp = madrecGetSortedRecnoList(madrecp);
        buf[0] = '\0';
        for (i=0; i<madrecp->nrecords; i++) {
            sprintf(buf1, "%5d ", outp[i]);
            strcat(buf, buf1);
        }
        status = 0;
    }

    else if (!strcmp(argv[0], "startTime")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        status = cedarGetStartTime(madrecp->recordp, &year, &month, &day,
                                &hour, &minute, &second, &centisecond);
        sprintf(buf, "%4d %2d %2d %2d %2d %2d %2d",
               year, month, day, hour, minute, second, centisecond);
        status = 0;
    }

    else if (!strcmp(argv[0], "endTime")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        status = cedarGetEndTime(madrecp->recordp, &year, &month, &day,
                                &hour, &minute, &second, &centisecond);
        sprintf(buf, "%4d %2d %2d %2d %2d %2d %2d",
               year, month, day, hour, minute, second, centisecond);
        status = 0;
    }

    else if (!strcmp(argv[0], "startIndex")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        dout = cedarGetStartIndex(madrecp->recordp);
        sprintf(buf, "%f", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "endIndex")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        dout = cedarGetEndIndex(madrecp->recordp);
        sprintf(buf, "%f", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "ltot")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetLtot(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "krec")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetKrec(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "kinst")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetKinst(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "kindat")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetKindat(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "ibyr")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIbyr(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "ibdt")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIbdt(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "ibhm")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIbhm(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "ibcs")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIbcs(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "ieyr")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIeyr(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "iedt")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIedt(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "iehm")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIehm(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "iecs")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetIecs(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "lprol")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetLprol(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "jpar")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetJpar(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "mpar")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetMpar(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "nrow")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetNrow(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "kpar")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        out = cedarGetKpar(madrecp->recordp);
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "word")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        out = cedarGetWord(madrecp->recordp, atoi(argv[1]));
        sprintf(buf, "%d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "startJday")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        dout = cedarGetStartJday(madrecp->recordp);
        sprintf(buf, "%.6f", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "endJday")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        dout = cedarGetEndJday(madrecp->recordp);
        sprintf(buf, "%.6f", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "header")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        buf[0] = '\0';
        nrow = cedarGetNrow(madrecp->recordp);
        sprintf(buf1, "%4d", nrow);
        strcat(buf, buf1);

        out = cedarGetIbyr(madrecp->recordp);
        year = out;
        sprintf(buf1, "%5d", year);
        strcat(buf, buf1);

        out = cedarGetIbdt(madrecp->recordp);
        month = out/100;
        sprintf(buf1, "%3d", month);
        strcat(buf, buf1);
        day = out - 100*month;
        sprintf(buf1, "%3d", day);
        strcat(buf, buf1);

        out = cedarGetIbhm(madrecp->recordp);
        hour = out/100;
        sprintf(buf1, "%3d", hour);
        strcat(buf, buf1);
        minute = out - 100*hour;
        sprintf(buf1, "%3d", minute);
        strcat(buf, buf1);

        out = cedarGetIbcs(madrecp->recordp);
        second = out/100;
        sprintf(buf1, "%3d", second);
        strcat(buf, buf1);

        out = cedarGetIeyr(madrecp->recordp);
        year = out;
        sprintf(buf1, "%5d", year);
        strcat(buf, buf1);

        out = cedarGetIedt(madrecp->recordp);
        month = out/100;
        sprintf(buf1, "%3d", month);
        strcat(buf, buf1);
        day = out - 100*month;
        sprintf(buf1, "%3d", day);
        strcat(buf, buf1);

        out = cedarGetIehm(madrecp->recordp);
        hour = out/100;
        sprintf(buf1, "%3d", hour);
        strcat(buf, buf1);
        minute = out - 100*hour;
        sprintf(buf1, "%3d", minute);
        strcat(buf, buf1);

        out = cedarGetIecs(madrecp->recordp);
        second = out/100;
        sprintf(buf1, "%3d", second);
        strcat(buf, buf1);

        dout = cedarGet1dParm(madrecp->recordp, 132);
        sprintf(buf1, "%7.1f", dout);
        strcat(buf, buf1);

        dout = cedarGet1dParm(madrecp->recordp, 133);
        sprintf(buf1, "%7.1f", dout);
        strcat(buf, buf1);

        dout = cedarGet1dParm(madrecp->recordp, 142);
        sprintf(buf1, "%5.1f", dout);
        strcat(buf, buf1);

        dout = cedarGet1dParm(madrecp->recordp, 143);
        sprintf(buf1, "%5.1f", dout);
        strcat(buf, buf1);

        status = 0;
    }

    else if (!strcmp(argv[0], "parcodes1d")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        jpar = cedarGetJpar(madrecp->recordp);
        outp = cedarGet1dParcodes(madrecp->recordp);
        buf[0] = '\0';
        for (i=0; i<jpar; i++) {
            sprintf(buf1, "%5d ", outp[i]);
            strcat(buf, buf1);
        }
        free(outp);
        status = 0;
    }

    else if (!strcmp(argv[0], "parcodes2d")) {
        if (argc != 1) {
            status = 100;
            return status;
        }
        mpar = cedarGetMpar(madrecp->recordp);
        outp = cedarGet2dParcodes(madrecp->recordp);
        buf[0] = '\0';
        for (i=0; i<mpar; i++) {
            sprintf(buf1, "%5d ", outp[i]);
            strcat(buf, buf1);
        }
        free(outp);
        status = 0;
    }

    else if (!strcmp(argv[0], "parm1d")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        dout = cedarGet1dParm(madrecp->recordp, atoi(argv[1]));
        sprintf(buf, "%12.5e", dout);
        status = 0;
    }

    else if (!strcmp(argv[0], "parm2d")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        nrow = cedarGetNrow(madrecp->recordp);
        doutp = cedarGet2dParm(madrecp->recordp, atoi(argv[1]));
        buf[0] = '\0';
        for (i=0; i<nrow; i++) {
            sprintf(buf1, "%12.5e ", doutp[i]);
            strcat(buf, buf1);
        }
        free(doutp);
        status = 0;
    }

    else if (!strcmp(argv[0], "1dInt")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        out = cedarGet1dInt(madrecp->recordp, atoi(argv[1]));
        sprintf(buf, "%6d", out);
        status = 0;
    }

    else if (!strcmp(argv[0], "2dInt")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        nrow = cedarGetNrow(madrecp->recordp);
        soutp = cedarGet2dInt(madrecp->recordp, atoi(argv[1]));
        buf[0] = '\0';
        for (i=0; i<nrow; i++) {
            sprintf(buf1, "%6hd ", soutp[i]);
            strcat(buf, buf1);
        }
        free(soutp);
        status = 0;
    }

    else {
        status = 200;
    }

    return status;
}


int
Mad_SetCmd(Tcl_Interp *interp, Madrec *madrecp, int argc, char *argv[])
{
    int status, i, nrow, nfparm;
    char **fparm;
    double *parmp;
    Int16 *intp;

    /* Set mad parameters */

    if (!strcmp(argv[0], "krec")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        (void) cedarSetKrec(madrecp->recordp, atoi(argv[1]));
        status = 0;
    }

    else if (!strcmp(argv[0], "kinst")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        (void) cedarSetKinst(madrecp->recordp, atoi(argv[1]));
        status = 0;
    }

    else if (!strcmp(argv[0], "kindat")) {
        if (argc != 2) {
            status = 100;
            return status;
        }
        (void) cedarSetKindat(madrecp->recordp, atoi(argv[1]));
        status = 0;
    }

    else if (!strcmp(argv[0], "startTime")) {
        if (argc != 8) {
            status = 100;
            return status;
        }
        (void) cedarSetStartTime(madrecp->recordp,
            atoi(argv[1]),atoi(argv[2]),atoi(argv[3]),atoi(argv[4]),
            atoi(argv[5]),atoi(argv[6]),atoi(argv[7]));
        status = 0;
    }

    else if (!strcmp(argv[0], "endTime")) {
        if (argc != 8) {
            status = 100;
            return status;
        }
        (void) cedarSetEndTime(madrecp->recordp,
            atoi(argv[1]),atoi(argv[2]),atoi(argv[3]),atoi(argv[4]),
            atoi(argv[5]),atoi(argv[6]),atoi(argv[7]));
        status = 0;
    }

    else if (!strcmp(argv[0], "1dParm")) {
        if (argc != 4) {
            status = 100;
            return status;
        }
        (void) cedarSet1dParm(madrecp->recordp,
            atoi(argv[1]),atof(argv[2]),atoi(argv[3]));
        status = 0;
    }

    else if (!strcmp(argv[0], "2dParm")) {
        if (argc != 4) {
            status = 100;
            return status;
        }
        if (Tcl_SplitList(interp, argv[2], &nfparm, &fparm) != TCL_OK) {
            sprintf(interp->result,
                "Improperly formatted 2d parameter list");
            status = -1;
            return status;
        }
        nrow = cedarGetNrow(madrecp->recordp);

        if (nfparm != nrow) {
            sprintf(interp->result,
                "This CEDAR record requires %d 2d parameters - %d were specified",
                 nrow, nfparm);
            status = -1;
            return status;
        }
        parmp = (double *)malloc(nfparm*sizeof(double));
        for (i=0; i<nfparm; i++) {
            parmp[i] = atof(fparm[i]);
        }
        (void) cedarSet2dParm(madrecp->recordp,
                              atoi(argv[1]),parmp,atoi(argv[3]));
        
        free(parmp);
        status = 0;
    }

    else if (!strcmp(argv[0], "1dInt")) {
        if (argc != 4) {
            status = 100;
            return status;
        }
        (void) cedarSet1dInt(madrecp->recordp,
            atoi(argv[1]),atoi(argv[2]),atoi(argv[3]));
        status = 0;
    }

    else if (!strcmp(argv[0], "2dInt")) {
        if (argc != 4) {
            status = 100;
            return status;
        }
        if (Tcl_SplitList(interp, argv[2], &nfparm, &fparm) != TCL_OK) {
            sprintf(interp->result,
                "Improperly formatted 2d parameter list");
            status = -1;
            return status;
        }
        nrow = cedarGetNrow(madrecp->recordp);

        if (nfparm != nrow) {
            sprintf(interp->result,
                "This CEDAR record requires %d 2d parameters - %d were specified",
                 nrow, nfparm);
            status = -1;
            return status;
        }
        intp = (Int16 *)malloc(nfparm*sizeof(int));
        for (i=0; i<nfparm; i++) {
            intp[i] = (Int16) atoi(fparm[i]);
        }
        (void) cedarSet2dInt(madrecp->recordp,
                              atoi(argv[1]),intp,atoi(argv[3]));
        
        free(intp);
        status = 0;
    }

    else {
        status = 200;
    }

    return status;
}

int Mad_CopyCmd(Madrec *madrec1p, char *mad2)
{
    int status;
    Tcl_HashEntry *entryp;
    Madrec *madrec2p;
    int madrec_copy();

    /* Get hash table entry for destination mad */
    entryp = Tcl_FindHashEntry(&mad_table, mad2);
    if (entryp == NULL) {        
        status = 100;
        return status;
    }
    madrec2p = (Madrec *)Tcl_GetHashValue(entryp);
    if (madrec2p == NULL) {
        status = 100;
        return status;
    }

    /* Copy mad */
    status = madrecCopy(madrec1p, madrec2p);

    return status;
}


/*************************************************************************
                        CedarCode Command
*************************************************************************/

int
CedarCodeCmd(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[])
{
    static unsigned int id=1;
    int cedarCode=0;
    int CedarCode_ObjectCmd();
    void CedarCode_DestroyCmd();

    /* Check command line arguments */
    if (argc != 1 && argc != 2) {
        sprintf (interp->result,
                 "cedarCode requires 0 or 1 arguments");
        return TCL_ERROR;
    }

    /* Set cedarCode key */
    sprintf(interp->result, "cedarCode%u", id);
    id++;

    /* Create new tcl command */
    Tcl_CreateCommand(interp, interp->result, CedarCode_ObjectCmd,
        (ClientData) &cedarCode, (Tcl_CmdDeleteProc *) Mad_DestroyCmd);

    /* If command has argument, set it to the cedarCode key */
    if (argc == 2) {
        Tcl_SetVar(interp, argv[1], interp->result, 0);
    }

    /* Read the Cedar parameter code table */
    if (cedarReadParCodes() == 0)
        return TCL_OK;
    else
        return TCL_ERROR;
}


void CedarCode_DestroyCmd(clientData)
ClientData clientData;
{
    return;
}


int CedarCode_ObjectCmd(ClientData clientData, Tcl_Interp *interp,
                        int argc, char *argv[])
{

    char * strp = NULL;

    if (argc < 2) {
        sprintf(interp->result, "%s requires at least one argument", 
                argv[0]);
        return TCL_ERROR;
    }

    if (!strcmp(argv[1], "destroy")) {
        Tcl_DeleteCommand(interp, argv[0]);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "numCodes")) {
        if (argc != 2) {
            sprintf(interp->result,
                    "%s numCodes requires zero arguments", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%d", cedarGetNumParCodes());
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "code")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s code requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%d", cedarGetParCode(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "codeIndex")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s codeIndex requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%d", cedarGetParCodeIndex(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "type")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s type requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%s", cedarGetParCodeType(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "scaleFactor")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s scaleFactor requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%e", cedarGetParScaleFactor(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "units")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s units requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%s", cedarGetParUnits(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "description")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s description requires one argument", argv[0]);
            return TCL_ERROR;
        }
        strp = cedarGetParDescription(atoi(argv[2]));
        sprintf(interp->result, "%s", strp);
        free(strp);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "int16Description")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s int16Description requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%s", cedarGetParInt16Description(atoi(argv[2])));
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "mnemonic")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s mnemonic requires one argument", argv[0]);
            return TCL_ERROR;
        }
        strp = cedarGetParMnemonic(atoi(argv[2]));
        sprintf(interp->result, "%s", strp);
        free(strp);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "format")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s format requires one argument", argv[0]);
            return TCL_ERROR;
        }
        strp = cedarGetParFormat(atoi(argv[2]));
        sprintf(interp->result, "%s", strp);
        return TCL_OK;
    }

    else if (!strcmp(argv[1], "width")) {
        if (argc != 3) {
            sprintf(interp->result,
                    "%s width requires one argument", argv[0]);
            return TCL_ERROR;
        }
        sprintf(interp->result, "%d", cedarGetParWidth(atoi(argv[2])));
        return TCL_OK;
    } else {
        return TCL_ERROR;
    }

}


int
MadGetKey(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[])
{
    int key;
    if (argc != 7 && argc != 8) {
	sprintf(interp->result,
		"getKey requires six or seven arguments");
	return TCL_ERROR;
    }
    key = getKey(atoi(argv[1]),atoi(argv[2]),atoi(argv[3]),
                 atoi(argv[4]),atoi(argv[5]),atoi(argv[6]));
    sprintf(interp->result, "%d", key);
    return TCL_OK;
}


int
MadJday(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[])
{
    int jDay;
    if (argc != 4) {
	sprintf(interp->result,
		"jday requires 3 arguments (day,month,year)");
	return TCL_ERROR;
    }
    jDay = jday(atoi(argv[1]),atoi(argv[2]),atoi(argv[3]));
    sprintf(interp->result, "%d", jDay);
    return TCL_OK;
}


int
MadJdater(ClientData clientData, Tcl_Interp *interp, int argc, char *argv[])
{
    int jDater, day, month, year;
    if (argc != 2) {
	sprintf(interp->result,
		"jdater requires 1 arguments (Julian day number)");
	return TCL_ERROR;
    }
    jDater = jdater(atoi(argv[1]),&day,&month,&year);
    sprintf(interp->result, "%d %d %d", day, month, year);
    return TCL_OK;
}
