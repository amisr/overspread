/*  $Id: madrec.c,v 1.26 2005/04/27 14:49:51 brideout Exp $ */

/*
modification history
--------------------
00a,22Apr96         original
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


/***********************************************************************
*
* madrecCreate     creates a madrec object
*
*   arguments:
*       None
*
*   returns:
*       pointer to the new madrec object
*
*/

Madrec *
madrecCreate ()
{
    Madrec *madrecp;

    madrecp = (Madrec *)malloc(sizeof(Madrec));
    madrecp->filnam = (char *)NULL;
    madrecp->fp = (FILE *)NULL;
    madrecp->fd = 0;
    madrecp->file_status = 0;
    madrecp->iotype = 0;
    madrecp->fileSize = 0;
    madrecp->filep = (Int16 *)NULL;
    madrecp->pos = 0;
    madrecp->nrecords = 0;
    madrecp->currentRecord = -1;
    madrecp->indexp = (CedarIndex *)NULL;
    madrecp->madrigalParms.blockSize = 6720;
    madrecp->madrigalParms.sigWords = 0;
    madrecp->madrigalParms.blockIndex = 0;
    madrecp->madrigalParms.blockp = (Int16 *)NULL;
    madrecp->madrigalParms.prevRec = 0;
    madrecp->madrigalParms.thisRec = 0;
    madrecp->blockedBinaryParms.pos = -1;          /* must be 1 for put */
    madrecp->blockedBinaryParms.lBlock = 2;
    madrecp->blockedBinaryParms.maxBlock = 8000;
    madrecp->blockedBinaryParms.lbuf = 2;
    madrecp->blockedBinaryParms.blockp = (Int16 *)NULL;
    madrecp->cbfParms.blockSize = 4096;
    madrecp->cbfParms.pos = 0;                     /* must be 1 for put */
    madrecp->cbfParms.cosRecordp = (Int16 *)NULL;
    madrecp->cbfParms.lCosBlock = 0;
    madrecp->cbfParms.fwi = 0;
    madrecp->cbfParms.initPos8 = 0;
    madrecp->cbfParms.initFwi = 0;
    madrecp->cbfParms.initPos = 0;
    madrecp->cbfParms.initLCosBlock = 0;
    madrecp->cbfParms.lbuf = 2;
    madrecp->cbfParms.blockNumber = 0;
    madrecp->cbfParms.previousFileIndex = -1;
    madrecp->cbfParms.previousRecordIndex = -1;
    madrecp->cbfParms.lastControlWord = -1;
    madrecp->unblockedBinaryParms.placeHolder = 0;
    madrecp->asciiParms.placeHolder = 0;
    madrecp->recordp = (Int16 *)NULL;
    madrecp->recordpInMem = 0;
    madrecp->earliestStartTime = 0.0;
    madrecp->latestEndTime = 0.0;
    madrecp->numParms = 0;
    madrecp->numKinst = 0;
    madrecp->numKindat = 0;
    madrecp->parmsListp = (int *)NULL;
    madrecp->parmLocp = (int *)NULL;
    madrecp->parmMinp = (double *)NULL;
    madrecp->parmMaxp = (double *)NULL;
    madrecp->parmMissing = (int *)NULL;
    madrecp->pflag = 0;
    madrecp->startJday0 = 0;
    madrecp->lastError = (char *)malloc(strlen(NO_ERR_STR)+1);
    (void) strcpy(madrecp->lastError, "No errors");
    madrecp->sortedRecnoList = (int *)NULL;
    return(madrecp);
}


/***********************************************************************
*
* madrecDestroy    destroys a madrec object
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns
*       0
*
*/

int
madrecDestroy (Madrec *madrecp)
{

    if (madrecp->filnam != (char *)NULL)
        free(madrecp->filnam);
    if (madrecp->filep != (Int16 *)NULL)
        free(madrecp->filep);
    if (madrecp->indexp != (CedarIndex *)NULL)
        free(madrecp->indexp);
    if (madrecp->sortedRecnoList != (int *)NULL)
        free(madrecp->sortedRecnoList);
    if (madrecp->madrigalParms.blockp != (Int16 *)NULL)
        free(madrecp->madrigalParms.blockp);
    if (madrecp->blockedBinaryParms.blockp != (Int16 *)NULL)
        free(madrecp->blockedBinaryParms.blockp);
    if (madrecp->cbfParms.cosRecordp != (Int16 *)NULL)
        free(madrecp->cbfParms.cosRecordp);
    if (madrecp->recordp != (Int16 *)NULL && !(madrecp->recordpInMem))
        free(madrecp->recordp);
    if (madrecp->parmsListp != (int *)NULL)
        free(madrecp->parmsListp);
    if (madrecp->parmLocp != (int *)NULL)
        free(madrecp->parmLocp);
    if (madrecp->parmMinp != (double *)NULL)
        free(madrecp->parmMinp);
    if (madrecp->parmMaxp != (double *)NULL)
        free(madrecp->parmMaxp);
    if (madrecp->parmMissing != (int *)NULL)
        free(madrecp->parmMissing);
    free(madrecp->lastError);
    free(madrecp);
    return(0);
}


/***********************************************************************
*
* madrecOpen     opens a madrec data file
*
*   arguments:
*       madrecp - pointer to the madrec object
*       iotype  - file type as described below
*       filnam  - file name
*
*   The following file types (iotype) are supported:
*
*       Open Cedar file for sequential reading:
*             1 - Determine file type automatically
*            10 - Madrigal file
*            11 - Blocked Binary file
*            12 - Cbf file
*            13 - Unblocked Binary file
*            14 - Ascii file
*
*       Create Cedar file for update; discard previous contents if any:
*             2 - Madrigal file
*            20 - Madrigal file
*            21 - Blocked Binary file
*            22 - Cbf file
*            23 - Unblocked Binary file
*            24 - Ascii file
*
*       Create Cedar file in memory for sequential and random read and write. 
*            30 - Determine file type automatically
*            40 - Madrigal file
*            41 - Blocked Binary file
*            42 - Cbf file
*            43 - Unblocked Binary file
*            44 - Ascii file 
*
*       Fast create Cedar file in memory for sequential and random read and write.
*          Does not calculate min and and max parameter values 
*            50 - Determine file type automatically
*            60 - Madrigal file
*            61 - Blocked Binary file
*            62 - Cbf file
*            63 - Unblocked Binary file
*            64 - Ascii file
*
*   returns:
*       0 - File opened successfully
*       1 - Invalid file type (iotype)
*       2 - unable to open data file
*       3 - data file already open
*       4 - input file name too long
*       5 - error writing file to memory
*
*/

int
madrecOpen (Madrec *madrecp, int iotype, char *filnam)
{
    int stat=0, ind=0;
    long fileType=0;
    int memPos = 0; /* used only in loading files into memory */
    static const char *err1="madrecOpen error - invalid file type";
    static const char *err2="madrecOpen error - unable to open file";
    static const char *err3="madrecOpen error - file already open";
    static const char *err4="madrecOpen error - file name too long";
    static const char *err5="madrecOpen error - error writing file to memory";
    static const char *err6="madrecOpen error - file not a valid CEDAR file";

    if (madrecp->file_status == 1) {
        (void) madrecSetError(madrecp,err3);
        return(3);
    }

    /* Set madrigal file name */
    if (iotype == 1 || iotype == 2 || iotype == 3 ||
        (iotype>= 10 && iotype <= 14) || (iotype>= 20 && iotype <= 24) ||
        (iotype>= 30 && iotype <= 64)) {
        if (strlen(filnam) >= MAXFCH) {
            (void) madrecSetError(madrecp,err4);
            return(4);
        }
        madrecp->filnam = (char *)malloc(strlen(filnam)+1);
        (void) strcpy(madrecp->filnam, filnam);
    }
    else {
       (void)  madrecSetError(madrecp,err1);
        return(1);
    }

    /* Determine file type */
    if (iotype == 1) {
        fileType = cedarFileType(filnam, MADRIGALBLOCKSIZE, CBFBLOCKSIZE);
        if (fileType == -1) {
	    (void) madrecSetError(madrecp,err6);
            return(6);
        } else if (fileType == -2) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        iotype = 10*iotype + fileType;
    }
    madrecp->iotype = iotype;

    /* Open data input file for reading */
    if (iotype>= 10 && iotype <= 14) {

        /* These initial values differ for reading and writing */
        madrecp->cbfParms.pos = 0;
        madrecp->blockedBinaryParms.pos = -1;

        if ((madrecp->fd = open(filnam, O_RDONLY)) == -1) {
	   (void)  madrecSetError(madrecp,err2);
            return(2);
        }
        if ((madrecp->fp = fdopen (madrecp->fd, "r")) == NULL) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }

    /* Open new output file for update */
    } else if (iotype == 2 || (iotype>= 20 && iotype <= 24)) {

        (void) remove(filnam);

        /* These initial values differ for reading and writing */
        madrecp->cbfParms.pos = 1;
        madrecp->blockedBinaryParms.pos = 1;

        if ((madrecp->fd = open(filnam, O_RDWR|O_CREAT, 0664)) == -1) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        if ((madrecp->fp = fdopen (madrecp->fd, "w+")) == NULL) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }


    /* Open data input file for random reading using memory image of file */
    } else if (iotype>= 30 && iotype <= 44) {

	/* Determine file type automatically if iotype == 30*/
        if (iotype == 30) {
            fileType = cedarFileType(filnam, MADRIGALBLOCKSIZE, CBFBLOCKSIZE);
            if (fileType == -1) {
	        (void) madrecSetError(madrecp,err6);
                return(6);
            } else if (fileType == -2) {
	        (void) madrecSetError(madrecp,err2);
                return(2);
            }
            iotype = 40 + fileType;
        }
	madrecp->iotype = iotype;

        /* These initial values differ for reading and writing */
        madrecp->cbfParms.pos = 0;
        madrecp->blockedBinaryParms.pos = -1;

        if ((madrecp->fd = open(filnam, O_RDONLY)) == -1) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        if ((madrecp->fp = fdopen (madrecp->fd, "r")) == NULL) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        madrecp->iotype = madrecp->iotype - 30; /* Temporary to read file */
        madrecp->nrecords = 0;
        madrecp->recordpInMem = 0;
        memPos = 0; /* keeps track of how much allocated mem used so far */

        while ((stat=madrecGetNextRec(madrecp)) == 0) {

            ind = putMemFastNextCedarUnblockedRecord(&madrecp->filep,
                                                     &madrecp->recordp,
                                                     &madrecp->fileSize,
                                                     &madrecp->pos,
                                                     &memPos);
            if (ind != 0) {
	        (void) madrecSetError(madrecp,err5);
                return(5);
            }
            madrecp->nrecords++;

            ind = cedarUpdateParmsList(madrecp->recordp, 
                                       &madrecp->numParms,
                                       &madrecp->parmsListp,
                                       &madrecp->parmLocp,
                                       &madrecp->parmMinp,
                                       &madrecp->parmMaxp,
                                       &madrecp->parmMissing,
                                       &madrecp->startJday0,
                                       &madrecp->earliestStartTime,
                                       &madrecp->latestEndTime,
                                       &madrecp->numKinst,
                                       madrecp->kinstArr,
                                       &madrecp->numKindat,
                                       madrecp->kindatArr);

        }
        /* realloc madrecp->filep since putMemFastNextCedarUnblockedRecord */
        /* allocates in large blocks for speed                             */
        madrecp->filep = (Int16 *)realloc(madrecp->filep, (memPos + 1)*2);
        madrecp->fileSize = (memPos+1)*2;

        madrecp->iotype = madrecp->iotype + 30; /* Reset to correct value */

        if (stat != -1) {
	    (void) madrecSetError(madrecp,err5);
            return(5);
        }

    /* Rapidly open data input file for random reading using memory image of file */
    /* Skip calculation of min and max of all parameters                          */
    } else if (iotype>= 50 && iotype <= 64) {

	/* Determine file type automatically if iotype == 50*/
        if (iotype == 50) {
            fileType = cedarFileType(filnam, MADRIGALBLOCKSIZE, CBFBLOCKSIZE);
            if (fileType == -1) {
	        (void) madrecSetError(madrecp,err6);
                return(6);
            } else if (fileType == -2) {
	        (void) madrecSetError(madrecp,err2);
                return(2);
            }
            iotype = 60 + fileType;
        }
	madrecp->iotype = iotype;

        /* These initial values differ for reading and writing */
        madrecp->cbfParms.pos = 0;
        madrecp->blockedBinaryParms.pos = -1;

        if ((madrecp->fd = open(filnam, O_RDONLY)) == -1) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        if ((madrecp->fp = fdopen (madrecp->fd, "r")) == NULL) {
	    (void) madrecSetError(madrecp,err2);
            return(2);
        }
        madrecp->iotype = madrecp->iotype - 50; /* Temporary to read file */
        madrecp->nrecords = 0;
        madrecp->recordpInMem = 0;
        memPos = 0; /* keeps track of how much allocated mem used so far */

        while ((stat=madrecGetNextRec(madrecp)) == 0) {

            ind = putMemFastNextCedarUnblockedRecord(&madrecp->filep,
                                                     &madrecp->recordp,
                                                     &madrecp->fileSize,
                                                     &madrecp->pos,
                                                     &memPos);
            if (ind != 0) {
	        (void) madrecSetError(madrecp,err5);
                return(5);
            }
            madrecp->nrecords++;

        }
        /* realloc madrecp->filep since putMemFastNextCedarUnblockedRecord */
        /* allocates in large blocks for speed                             */
        madrecp->filep = (Int16 *)realloc(madrecp->filep, (memPos + 1)*2);
        madrecp->fileSize = (memPos+1)*2;

        madrecp->iotype = madrecp->iotype + 50; /* Reset to correct value */

        if (stat != -1) {
	    (void) madrecSetError(madrecp,err5);
            return(5);
        }

    } else {
        madrecp->file_status = 1;
        (void) madrecSetError(madrecp,err1);
        return(1);
    }
    
    madrecp->file_status = 1;
    madrecp->iotype = iotype;
    (void) fseek (madrecp->fp, 0L, SEEK_SET);

    return(0);
}


/***********************************************************************
*
* madrecClose    closes a madrec data file
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - File closed successfully
*       1 - error closing file
*       2 - file not open
*       3 - error flushing file
*
*/

int
madrecClose (Madrec *madrecp)
{
    static const char *err1="madrecClose error - error closing file";
    static const char *err2="madrecClose error - file not open";
    static const char *err3="madrecClose error - error flushing file";

    if (madrecp->file_status == 1) {

        if (madrecp->iotype == 21) {    /* Flush blocked binary  */
            if(flushCedarBlockedRecord(madrecp->fp, &madrecp->recordp,
                                &madrecp->blockedBinaryParms.maxBlock,
                                &madrecp->blockedBinaryParms.lbuf,
                                &madrecp->blockedBinaryParms.pos,
                                &madrecp->blockedBinaryParms.blockp) != 0) {
                (void) madrecSetError(madrecp,err3);
                return(3);
            }
        }

        if (madrecp->iotype == 22) {    /* Flush Cbf  */
            if(flushCedarCbfRecord(madrecp->fp, &madrecp->recordp,
                                madrecp->cbfParms.blockSize,
                                &madrecp->cbfParms.lbuf,
                                &madrecp->cbfParms.pos,
                                &madrecp->cbfParms.cosRecordp,
                                &madrecp->cbfParms.blockNumber,
                                &madrecp->cbfParms.previousFileIndex,
                                &madrecp->cbfParms.previousRecordIndex,
                                &madrecp->cbfParms.lastControlWord) != 0) {
                (void) madrecSetError(madrecp,err3);
                return(3);
            }
            if(endFileCedarCbfRecord(madrecp->fp, &madrecp->recordp,
                                madrecp->cbfParms.blockSize,
                                &madrecp->cbfParms.previousFileIndex,
                                &madrecp->cbfParms.lastControlWord) != 0) {
                (void) madrecSetError(madrecp,err1);
                return(3);
            }
            if(endDataCedarCbfRecord(madrecp->fp, &madrecp->recordp,
                                madrecp->cbfParms.blockSize,
                                &madrecp->cbfParms.lastControlWord) != 0) {
                (void) madrecSetError(madrecp,err1);
                return(3);
            }
        }

        if (madrecp->fp != (FILE *)NULL && fclose(madrecp->fp) != 0) {
            (void) madrecSetError(madrecp,err1);
            return(1);
        }
        madrecp->fp = (FILE *)NULL;
        if (madrecp->filnam != (char *)NULL) {
            free(madrecp->filnam);
            madrecp->filnam = (char *)NULL;
        }
        madrecp->fd = 0;
        madrecp->file_status = 0;
        madrecp->iotype = 0;
        madrecp->fileSize = 0;
        madrecp->pos = 0;
        madrecp->nrecords = 0;
        madrecp->currentRecord = -1;
        (void) madrecDeleteKeys (madrecp);
        madrecp->madrigalParms.blockSize = 6720;
        madrecp->madrigalParms.sigWords = 0;
        madrecp->madrigalParms.blockIndex = 0;
        if (madrecp->madrigalParms.blockp != (Int16 *)NULL) {
            free(madrecp->madrigalParms.blockp);
            madrecp->madrigalParms.blockp = (Int16 *)NULL;
        }
        madrecp->madrigalParms.prevRec = 0;
        madrecp->madrigalParms.thisRec = 0;
        madrecp->blockedBinaryParms.pos = 1;
        madrecp->blockedBinaryParms.lBlock = -1;
        madrecp->blockedBinaryParms.maxBlock = 0;
        madrecp->blockedBinaryParms.lbuf = 2;
        if (madrecp->blockedBinaryParms.blockp != (Int16 *)NULL) {
            free(madrecp->blockedBinaryParms.blockp);
            madrecp->blockedBinaryParms.blockp = (Int16 *)NULL;
        }
        madrecp->cbfParms.blockSize = 4096;
        madrecp->cbfParms.pos = 0;                     /* must be 1 for put */
        if (madrecp->cbfParms.cosRecordp != (Int16 *) NULL) {
            free(madrecp->cbfParms.cosRecordp);
            madrecp->cbfParms.cosRecordp = (Int16 *)NULL;
        }
        madrecp->cbfParms.lCosBlock = 0;
        madrecp->cbfParms.fwi = 0;
        madrecp->cbfParms.initPos8 = 0;
        madrecp->cbfParms.initFwi = 0;
        madrecp->cbfParms.initPos = 0;
        madrecp->cbfParms.initLCosBlock = 0;
        madrecp->cbfParms.lbuf = 2;
        madrecp->cbfParms.blockNumber = 0;
        madrecp->cbfParms.previousFileIndex = -1;
        madrecp->cbfParms.previousRecordIndex = -1;
        madrecp->cbfParms.lastControlWord = -1;
        madrecp->unblockedBinaryParms.placeHolder = 0;
        madrecp->asciiParms.placeHolder = 0;
        return(0);
    }
    else {
	(void) madrecSetError(madrecp,err2);
        madrecp->fd = 0;
        madrecp->file_status = 0;
        return(2);
    }
}


/***********************************************************************
*
* madrecGetNextRec   reads a cedar record and fills a Madrec structure
*                    with the information in the record.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - Record read successfully
*       1 - Illegal file type (bad iotype in madrec)
*      -n - Error in CedarIO package
*
*/

int
madrecGetNextRec (Madrec *madrecp)
{
    int ind=0;
    static const char *err1="madrecGetNextRec error - illegal file type";
    char err[132];

    if (madrecp->iotype == 10) {    /* Read Madrigal */
        ind = getNextMadrigalRecord(madrecp->fp, &madrecp->recordp,
                                    madrecp->madrigalParms.blockSize,
                                    &madrecp->madrigalParms.sigWords);
        if (ind != 0 && ind != -1) {
           if (ind == -6)
               (void) sprintf(err, "Impossibly short record found in getNextMadrigalRecord\n");
           else if (ind == -9)
               (void) sprintf(err, "lprol != 21 found in getNextMadrigalRecord\n");
           else
               (void) sprintf(err, "Error %d in getNextMadrigalRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }
        madrecp->pos = ftell(madrecp->fp)/2;

    } else if (madrecp->iotype == 11) {    /* Read blocked binary */
        ind = getNextCedarBlockedRecord(madrecp->fp, &madrecp->recordp,
                                        &madrecp->blockedBinaryParms.lBlock,
                                        &madrecp->blockedBinaryParms.pos);
        if (ind != 0 && ind != -1) {
           (void) sprintf(err, "Error %d in getNextCedarBlockedRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }
        madrecp->pos = ftell(madrecp->fp)/2;

    } else if (madrecp->iotype == 12) {    /* Read Cbf */
        /* Cbf files can contain multiple Cedar files. Here we ignore ind=-1
           which indicates end of Cedar file. The end of the Cbf file is
           indicated by ind=-2. This is changed to ind=-1 to indicate
           end of file. Thus, all the Cedar records in the Cbf file appear
           as a single file. Typically header records will appear at the
           beginning of each of the Cedar files included in the Cbf file */
        while ((
        ind = getNextCedarCbfRecord(madrecp->fp, &madrecp->recordp, 0,
                                    madrecp->cbfParms.blockSize,
                                    &madrecp->cbfParms.initPos8,
                                    &madrecp->cbfParms.initFwi,
                                    &madrecp->cbfParms.initPos,
                                    &madrecp->cbfParms.initLCosBlock,
                                    &madrecp->cbfParms.lCosBlock,
                                    &madrecp->cbfParms.pos,
                                    &madrecp->cbfParms.fwi,
                                    &madrecp->cbfParms.cosRecordp))==-1);
        if (ind != 0 && ind != -1 && ind != -2) {
           (void) sprintf(err, "Error %d in getNextCedarCbfRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }
        madrecp->pos = ftell(madrecp->fp)/2;
        if (ind == -2) {
            ind = -1;
        }

    } else if (madrecp->iotype == 13) {    /* Read unblocked binary */
        ind = getNextCedarUnblockedRecord(madrecp->fp, &madrecp->recordp);
        if (ind != 0 && ind != -1) {
           (void) sprintf(err, "Error %d in getNextCedarUnblockedRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }
        madrecp->pos = ftell(madrecp->fp)/2;

    } else if (madrecp->iotype == 14) {    /* Read ascii */
        ind = getNextCedarAsciiRecord(madrecp->fp, &madrecp->recordp);
        if (ind != 0 && ind != -1) {
           if (ind == -4)
               (void) sprintf(err, "Integer out of 16 bit range found in getNextCedarAsciiRecord\n");
           else
               (void) sprintf(err, "Error %d in getNextCedarAsciiRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }
        madrecp->pos = ftell(madrecp->fp);

    } else if (madrecp->iotype == 40 ||
               madrecp->iotype == 41 ||
               madrecp->iotype == 42 ||
               madrecp->iotype == 43 ||
               madrecp->iotype == 44 ||
               madrecp->iotype == 60 ||
               madrecp->iotype == 61 ||
               madrecp->iotype == 62 ||
               madrecp->iotype == 63 ||
               madrecp->iotype == 64) {    /* Read memory */

        ind = getMemNextCedarUnblockedRecord(&madrecp->filep,
                                             &madrecp->recordp,
                                             &madrecp->pos,
                                             &madrecp->recordpInMem);
        if (ind != 0 && ind != -1) {
           (void) sprintf(err, "Error %d in getMemNextCedarUnblockedRecord\n", ind);
           (void) madrecSetError(madrecp,err);
        }

    } else {
        ind = 1;
        (void) madrecSetError(madrecp,err1);
    }

    madrecp->currentRecord++;
    return(ind);
}


/***********************************************************************
*
* madrecPutNextRec   writes a cedar record.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*       1 - Illegal file type (bad iotype in madrec)
*      -n - Error in CedarIO package
*
*/

int
madrecPutNextRec (Madrec *madrecp)
{
    int ind=0;
    static const char *err1="madrecPutNextRec error - illegal file type";
    static const char *err2="madrecPutNextRec error - error in cedarIO package";

    if (madrecp->iotype ==  2 ||
        madrecp->iotype == 20) {    /* Write Madrigal  */
        ind = putNextMadrigalRecord(madrecp->fp, &madrecp->recordp,
                                    madrecp->madrigalParms.blockSize,
                                    &madrecp->madrigalParms.blockIndex,
                                    &madrecp->madrigalParms.blockp,
                                    &madrecp->madrigalParms.prevRec,
                                    &madrecp->madrigalParms.thisRec);

    } else if (madrecp->iotype == 21) {    /* Write blocked binary  */
        ind = putNextCedarBlockedRecord(madrecp->fp, &madrecp->recordp,
                                        &madrecp->blockedBinaryParms.maxBlock,
                                        &madrecp->blockedBinaryParms.lbuf,
                                        &madrecp->blockedBinaryParms.pos,
                                        &madrecp->blockedBinaryParms.blockp);

    } else if (madrecp->iotype == 22) {    /* Write Cbf  */
        ind = putNextCedarCbfRecord(madrecp->fp, &madrecp->recordp,
                                    madrecp->cbfParms.blockSize,
                                    &madrecp->cbfParms.lbuf,
                                    &madrecp->cbfParms.pos,
                                    &madrecp->cbfParms.cosRecordp,
                                    &madrecp->cbfParms.blockNumber,
                                    &madrecp->cbfParms.previousFileIndex,
                                    &madrecp->cbfParms.previousRecordIndex,
                                    &madrecp->cbfParms.lastControlWord);

    } else if (madrecp->iotype == 23) {    /* Write unblocked binary  */
        ind = putNextCedarUnblockedRecord(madrecp->fp, &madrecp->recordp);

    } else if (madrecp->iotype == 24) {    /* Write ascii  */
        ind = putNextCedarAsciiRecord(madrecp->fp, &madrecp->recordp);

    } else if (madrecp->iotype == 40 ||
               madrecp->iotype == 41 ||
               madrecp->iotype == 42 ||
               madrecp->iotype == 43 ||
               madrecp->iotype == 44 ||
               madrecp->iotype == 60 ||
               madrecp->iotype == 61 ||
               madrecp->iotype == 62 ||
               madrecp->iotype == 63 ||
               madrecp->iotype == 64) {    
        /* Write memory */
        ind = putMemNextCedarUnblockedRecord(&madrecp->filep,
                                             &madrecp->recordp,
                                             &madrecp->fileSize,
                                             &madrecp->pos);

    } else {
        ind = 1;
        (void) madrecSetError(madrecp,err1);
    }

    if (ind != 0 && ind != -1 && ind != 1) {
        (void) madrecSetError(madrecp,err2);
    }

    return(ind);
}


/***********************************************************************
*
* madrecRewind   rewinds a cedar record.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*       1 - Illegal file type (bad iotype in madrec)
*      -n - Error in CedarIO package
*
*/

int
madrecRewind (Madrec *madrecp)
{
    int iotype=0, nrecords=0;
    char *filnam;
    static const char *err1="madrecRewind error - No file to rewind";

    if (madrecp->iotype > 24 && madrecp->iotype < 65) {    /* Rewind memory file  */
        madrecp->pos = 0;
    
    } else {    /* Just close and open file */
        iotype = madrecp->iotype;
        if (madrecp->filnam != (char *)NULL) {
            filnam = (char *)malloc(strlen(madrecp->filnam)+1);
            (void) strcpy(filnam, madrecp->filnam);
            nrecords = madrecp->nrecords;
            (void) madrecClose(madrecp);
            (void) madrecOpen(madrecp, iotype, filnam);
            free(filnam);
            madrecp->nrecords = nrecords;
        } else {
            (void) madrecSetError(madrecp,err1);
            return(1);
        }
    }

    return(0);
}


/***********************************************************************
*
* madrecGetPreviousRec   reads an madrigal record and fills a Mad 
*                        structure with the information in the record.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*
*/

int
madrecGetPreviousRec(Madrec *madrecp)
{
    int ind=0;
    ind = madrecGetRecByRecno(madrecp, madrecp->currentRecord - 1);
    return(ind);
}


/***********************************************************************
*
* madrecGetRecByRecno   reads a madrigal record and fills a Mad 
*                          structure with the information in the record.
*                          The record number is specified by the second
*                          argument. The first record is recno=0.
*
*   arguments:
*       madrecp - pointer to the madrec object
*       recno   - the record number to get. The first record is recno=0.
*
*   returns:
*       0 - Record read successfully
*      -1 - Specified record not in file
*
*/

int
madrecGetRecByRecno(Madrec *madrecp, int recno)
{
    int i=0, ind=0;
    static const char *err1="madrecGetRecByRecno error - Specified record is not in file";
    static const char *err2="madrecGetRecByRecno error - Operation not supported for this file type";
    static const char *err3="madrecGetRecByRecno error - error in cedarIO package";

    if (recno < 0) {
        (void) madrecSetError(madrecp,err1);
        return(-1);
    }

    /* Generate key table if necessary */
    if (madrecp->indexp==(CedarIndex *)NULL && madrecp->iotype!=12) {
        ind = madrecGenKeys(madrecp);
    }

    if ((recno < 0  || recno>madrecp->nrecords-1) && madrecp->iotype!=12) {
        (void) madrecSetError(madrecp,err1);
        return(-1);
    }

    if (madrecp->iotype == 10) {
	madrecp->madrigalParms.blockSize = madrecp->indexp[recno].int1;
	madrecp->madrigalParms.sigWords = madrecp->indexp[recno].int2;
        madrecp->pos = madrecp->indexp[recno].pos;
        (void) fseek(madrecp->fp, 2*madrecp->pos, SEEK_SET);
        ind = getNextMadrigalRecord(madrecp->fp, &madrecp->recordp,
                                    madrecp->madrigalParms.blockSize,
                                    &madrecp->madrigalParms.sigWords);
        if (ind != 0 && ind != -1) {
           (void) madrecSetError(madrecp,err3);
           return(ind);
        }
        madrecp->pos = ftell(madrecp->fp)/2;
        madrecp->currentRecord = recno;

    } else if (madrecp->iotype == 11) {

	madrecp->blockedBinaryParms.pos = madrecp->indexp[recno].int1;
	madrecp->blockedBinaryParms.lBlock = madrecp->indexp[recno].int2;
        madrecp->pos = madrecp->indexp[recno].pos;
        (void) fseek(madrecp->fp, 2*madrecp->pos, SEEK_SET);
        ind = getNextCedarBlockedRecord(madrecp->fp, &madrecp->recordp,
                                        &madrecp->blockedBinaryParms.lBlock,
                                        &madrecp->blockedBinaryParms.pos);
        if (ind != 0 && ind != -1) {
            (void) madrecSetError(madrecp,err3);
            return(ind);
        }
        madrecp->currentRecord = recno;
        madrecp->pos = ftell(madrecp->fp)/2;

    } else if (madrecp->iotype == 12) {

        /* Start non-working code */

        /* The problem seems to be with reading the cosRecord. */
        /*
        if (0) {
	madrecp->cbfParms.cosRecordp = madrecp->indexp[recno].int16p1;
	madrecp->cbfParms.blockSize = madrecp->indexp[recno].int1;
	madrecp->cbfParms.pos = madrecp->indexp[recno].int2;
	madrecp->cbfParms.lCosBlock = madrecp->indexp[recno].int3;
	madrecp->cbfParms.fwi = madrecp->indexp[recno].int4;
	madrecp->cbfParms.initPos8 = madrecp->indexp[recno].int5;
	madrecp->cbfParms.initFwi = madrecp->indexp[recno].int6;
	madrecp->cbfParms.initPos = madrecp->indexp[recno].int7;
	madrecp->cbfParms.initLCosBlock = madrecp->indexp[recno].int8;
        madrecp->pos = madrecp->indexp[recno].pos;
        if (madrecp->cbfParms.cosRecordp != (Int16 *)NULL) {
            free(madrecp->cbfParms.cosRecordp);
            madrecp->cbfParms.cosRecordp = (Int16 *)NULL;
        }
       pos = ftell(madrecp->fp);
       (void) printf("*%3d %8d %8d %8d %8d %8d %8d %8d %8d %8d %8d\n",
       madrecp->currentRecord,
       madrecp->cbfParms.blockSize,
       madrecp->cbfParms.initPos8,
       madrecp->cbfParms.initFwi,
       madrecp->cbfParms.initPos,
       madrecp->cbfParms.initLCosBlock,
       madrecp->cbfParms.lCosBlock,
       madrecp->cbfParms.pos,
       madrecp->cbfParms.fwi,
       madrecp->cbfParms.cosRecordp[0],
       pos);
        madrecp->cbfParms.fwi = madrecp->cbfParms.initFwi;
        ind = getNextCedarCbfRecord(madrecp->fp, &madrecp->recordp, 1,
                                    madrecp->cbfParms.blockSize,
                                    &madrecp->cbfParms.initPos8,
                                    &madrecp->cbfParms.initFwi,
                                    &madrecp->cbfParms.initPos,
                                    &madrecp->cbfParms.initLCosBlock,
                                    &madrecp->cbfParms.lCosBlock,
                                    &madrecp->cbfParms.pos,
                                    &madrecp->cbfParms.fwi,
                                    &madrecp->cbfParms.cosRecordp);
       madrecp->currentRecord = recno;
       pos = ftell(madrecp->fp);
       (void) printf("*%3d %8d %8d %8d %8d %8d %8d %8d %8d %8d %8d\n",
       madrecp->currentRecord,
       madrecp->cbfParms.blockSize,
       madrecp->cbfParms.initPos8,
       madrecp->cbfParms.initFwi,
       madrecp->cbfParms.initPos,
       madrecp->cbfParms.initLCosBlock,
       madrecp->cbfParms.lCosBlock,
       madrecp->cbfParms.pos,
       madrecp->cbfParms.fwi,
       madrecp->cbfParms.cosRecordp[0],
       pos);
        if (ind != 0 && ind != -1) {
            (void) madrecSetError(madrecp,err3);
            return(ind);
        }
        madrecp->pos = ftell(madrecp->fp)/2;

        }
        */

        /*** End non-working code ***/

        /* For now, just emulate random read by reading sequentially to
           the specified recoprd */
        (void) madrecRewind(madrecp);
        for (i=0; i<=recno; i++) {
            ind = getNextCedarCbfRecord(madrecp->fp, &madrecp->recordp,
                                        0,
                                        madrecp->cbfParms.blockSize,
                                        &madrecp->cbfParms.initPos8,
                                        &madrecp->cbfParms.initFwi,
                                        &madrecp->cbfParms.initPos,
                                        &madrecp->cbfParms.initLCosBlock,
                                        &madrecp->cbfParms.lCosBlock,
                                        &madrecp->cbfParms.pos,
                                        &madrecp->cbfParms.fwi,
                                        &madrecp->cbfParms.cosRecordp);
            if (ind != 0 && ind != -1 && ind != -2) {
                (void) madrecSetError(madrecp,err3);
                return(ind);
            }
            if (ind == -1 || ind == -2) {
                (void) madrecSetError(madrecp,err1);
                madrecp->currentRecord = recno;
                madrecp->pos = ftell(madrecp->fp)/2;
                return(-1);
            }
        }
        madrecp->currentRecord = recno;


    } else if (madrecp->iotype == 13) {
        madrecp->pos = madrecp->indexp[recno].pos;
        (void) fseek(madrecp->fp, 2*madrecp->pos, SEEK_SET);
        ind = getNextCedarUnblockedRecord(madrecp->fp, &madrecp->recordp);
        if (ind != 0 && ind != -1) {
            (void) madrecSetError(madrecp,err3);
        }
        madrecp->pos = ftell(madrecp->fp)/2;
        madrecp->currentRecord = recno;

    } else if (madrecp->iotype == 14) {
        madrecp->pos = madrecp->indexp[recno].pos;
        (void) fseek(madrecp->fp, madrecp->pos, SEEK_SET);
        ind = getNextCedarAsciiRecord(madrecp->fp, &madrecp->recordp);
        if (ind != 0 && ind != -1) {
            (void) madrecSetError(madrecp,err3);
            return(ind);
        }
        madrecp->pos = ftell(madrecp->fp);
        madrecp->currentRecord = recno;

    } else if (madrecp->iotype == 40 || 
               madrecp->iotype == 41 || 
               madrecp->iotype == 42 || 
               madrecp->iotype == 43 || 
               madrecp->iotype == 44 ||
               madrecp->iotype == 60 || 
               madrecp->iotype == 61 || 
               madrecp->iotype == 62 || 
               madrecp->iotype == 63 || 
               madrecp->iotype == 64) {
        madrecp->pos = madrecp->indexp[recno].pos;

        ind = editMemNextCedarUnblockedRecord(&madrecp->filep,
                                             &madrecp->recordp,
                                             &madrecp->pos,
                                             &madrecp->recordpInMem);


        if (ind != 0 && ind != -1) {
            (void) madrecSetError(madrecp,err3);
            return(ind);
        }
        
        madrecp->currentRecord = recno;

    } else {
        (void) madrecSetError(madrecp,err2);
        return(-2);
    }

    return(0);
}


/***********************************************************************
*
* madrecGetRecordByKey   reads an madrigal record and fills a Mad 
*                        structure with the information in the record.
*                        The record is the first data record for which key is
*                        greater than or equal to the start key of the
*                        record, and less than the start time of the
*                        following record. Thus, if the specified key
*                        corresponds to a time within a record, the
*                        first such record is returned. Header or catalog
*                        records are never returned.
*                    
*
*   arguments:
*       madrecp - pointer to the madrec object
*       key - time in seconds since 1/1/1950
*
*   returns:
*       0 - if record found
*      -1 - if record not found
*
*/

int
madrecGetRecByKey(Madrec *madrecp, double key)
{
    int i=0;
    int low = 0;
    int mid = 0;
    int high = 0;
    double endKey=0.0;
    int beforeFirstDataRec = 0;
    static const char *err1="madrecGetRecByKey error - Specified key is beyond last record in file";

    /* Generate key table if necessary */
    if (madrecp->indexp==(CedarIndex *)NULL) { 
        (void) madrecGenKeys(madrecp);
    }

    /* check if key is before first data record endKey.  If so, set
       beforeFirstDataRec =  1 to return first data record. */
    for (i=0; i<madrecp->nrecords-1; i++) {
        /* skip header and catalog records to find first data record */
        if (!madrecp->indexp[i].isData) 
            continue;
        else if (key <= madrecp->indexp[i].endKey)
        {
           beforeFirstDataRec = 1;
        }
        break;
    }
    
    /* set low to skip non-data records */
    low = i;
    high = madrecp->nrecords-1;

    if (beforeFirstDataRec) {
        /* the correct i has already been found */
        mid = i;
    } else if (key > madrecp->indexp[madrecp->nrecords-1].endKey) {
        /* key after last record, error */
        (void) madrecSetError(madrecp,err1);
        return(-1);
    } else {
        
        /* speed up using binary search */
        while (low <= high)
        {
            
            mid = (low + high)/2;
            /* skip catalog or header records */
            while (!madrecp->indexp[mid].isData)
            {
                /* if this is the last record, then throw an error */
                if (mid == madrecp->nrecords-1)
                {
                   (void) madrecSetError(madrecp,err1);
                   return(-1); 
                }
                else   
                    high++;
                    mid = (low + high)/2;
            }
            
            /* we have a data record , see if its the right one */
            if (mid == madrecp->nrecords-1) {
                endKey = madrecp->indexp[mid].endKey;
            } else {
                endKey = madrecp->indexp[mid+1].startKey;
            }
            if (key >= madrecp->indexp[mid].startKey && key < endKey)
            {
                /* right record found */
                break;
            }
            else if (key < madrecp->indexp[mid].startKey)
            {
                /* too high */
                high = mid - 1;
            }
            else
            {
                /* too low */
                low = mid + 1;
            }
        
        }
    }

    (void) madrecGetRecByRecno(madrecp, madrecp->indexp[mid].recNo);

    return(0);
}


/***********************************************************************
*
* madrecGenKeys   Generates madrec key array. All information needed to
*                 access records randomly by key or record number is
*                 saved.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*
*/


int
madrecGenKeys (Madrec *madrecp)
{
    int i=0;

    (void) madrecRewind(madrecp);

    madrecp->indexp = (CedarIndex *) malloc(3*sizeof(CedarIndex));

    /* Set intial iotype-independent state parameters */
    madrecp->indexp[0].pos = 0;
    madrecp->indexp[0].posPrev = -1;
    /* Set initial state parameters for this iotype */
    if (madrecp->iotype == 10) {
	madrecp->indexp[0].int1 = madrecp->madrigalParms.blockSize;
	madrecp->indexp[0].int2 = madrecp->madrigalParms.sigWords;
    } else if (madrecp->iotype == 11) {
	madrecp->indexp[0].int1 = madrecp->blockedBinaryParms.pos;
	madrecp->indexp[0].int2 = madrecp->blockedBinaryParms.lBlock;
    } else if (madrecp->iotype == 12) {
	madrecp->indexp[0].int16p1 = madrecp->cbfParms.cosRecordp;
	madrecp->indexp[0].int1 = madrecp->cbfParms.blockSize;
	madrecp->indexp[0].int2 = madrecp->cbfParms.pos;
	madrecp->indexp[0].int3 = madrecp->cbfParms.lCosBlock;
	madrecp->indexp[0].int4 = madrecp->cbfParms.fwi;
	madrecp->indexp[0].int5 = madrecp->cbfParms.initPos8;
	madrecp->indexp[0].int6 = madrecp->cbfParms.initFwi;
	madrecp->indexp[0].int7 = madrecp->cbfParms.initPos;
	madrecp->indexp[0].int8 = madrecp->cbfParms.initLCosBlock;
    }

    /* Read file and store state parameters for each record */
    i = 1;
    while ((madrecGetNextRec(madrecp)) == 0) {
        /* Set iotype-independent state parameters */
        madrecp->indexp[i-1].recNo = i - 1;
        madrecp->indexp[i-1].startKey = cedarGetStartIndex(madrecp->recordp);
        madrecp->indexp[i-1].endKey = cedarGetEndIndex(madrecp->recordp);
        madrecp->indexp[i].pos = madrecp->pos;
        madrecp->indexp[i].posPrev =  madrecp->indexp[i-1].pos;
        if (cedarGetKrec(madrecp->recordp) == HEADERBIN || cedarGetKrec(madrecp->recordp) == HEADERASCII ||
            cedarGetKrec(madrecp->recordp) == CATALOGBIN || cedarGetKrec(madrecp->recordp) == CATALOGASCII)
            madrecp->indexp[i-1].isData = 0;
        else
            madrecp->indexp[i-1].isData = 1;
	if (cedarGetKrec(madrecp->recordp) == CATALOGBIN || cedarGetKrec(madrecp->recordp) == CATALOGASCII)
            madrecp->indexp[i-1].isCatalog = 1;
        else
            madrecp->indexp[i-1].isCatalog = 0;
        /* Set state parameters for this iotype */
        if (madrecp->iotype == 10) {
            madrecp->indexp[i].int1 = madrecp->madrigalParms.blockSize;
            madrecp->indexp[i].int2 = madrecp->madrigalParms.sigWords;
        } else if (madrecp->iotype == 11) {
            madrecp->indexp[i].int1 = madrecp->blockedBinaryParms.pos;
            madrecp->indexp[i].int2 = madrecp->blockedBinaryParms.lBlock;
        } else if (madrecp->iotype == 12) {
            madrecp->indexp[i-1].int16p1 = madrecp->cbfParms.cosRecordp;
            madrecp->indexp[i-1].int1 = madrecp->cbfParms.blockSize;
            madrecp->indexp[i-1].int2 = madrecp->cbfParms.pos;
            madrecp->indexp[i-1].int3 = madrecp->cbfParms.lCosBlock;
            madrecp->indexp[i-1].int4 = madrecp->cbfParms.fwi;
            madrecp->indexp[i-1].int5 = madrecp->cbfParms.initPos8;
            madrecp->indexp[i-1].int6 = madrecp->cbfParms.initFwi;
            madrecp->indexp[i-1].int7 = madrecp->cbfParms.initPos;
            madrecp->indexp[i-1].int8 = madrecp->cbfParms.initLCosBlock;
        }
        /*
        (void) printf("*** %4d %8d %4d %6d %f %f %8d %8d %i\n",
            i, madrecp->pos, madrecp->nrecords,
            madrecp->indexp[i-1].recNo,
            madrecp->indexp[i-1].startKey,
            madrecp->indexp[i-1].endKey,
            madrecp->indexp[i-1].pos,
            madrecp->indexp[i-1].posPrev,
	    madrecp->indexp[i-1].isCatalog);
        */
        i++;
        madrecp->indexp = (CedarIndex *)realloc(madrecp->indexp,
                                                (i+2)*sizeof(CedarIndex));
    }
    madrecp->nrecords = i - 1;

    return(0);
}


/***********************************************************************
*
* madrecDeleteKeys   Deletes madrec key array.
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*
*/


int
madrecDeleteKeys (Madrec *madrecp)
{
    madrecp->pos = 0;
    madrecp->nrecords = 0;
    madrecp->currentRecord = 0;
    if (madrecp->indexp != (CedarIndex *)NULL) {
        free(madrecp->indexp);
    }
    madrecp->indexp = (CedarIndex *)NULL;
    return(0);
}


/***********************************************************************
*
* madrecPrintKeys   Prints madrec file key table
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*
*/


int
madrecPrintKeys (Madrec *madrecp)
{
    int i=0;

(void) printf("madrecp->nrecords = %d\n",  madrecp->nrecords);
    for (i=0; i<madrecp->nrecords; i++) {
        (void) printf("%5d %f %f %7d %7d %7d %7d %7d %7d\n", 
            madrecp->indexp[i].recNo,
            madrecp->indexp[i].startKey,
            madrecp->indexp[i].endKey,
            madrecp->indexp[i].pos,
            madrecp->indexp[i].posPrev,
            madrecp->indexp[i].int5,
            madrecp->indexp[i].int6,
            madrecp->indexp[i].int7,
            madrecp->indexp[i].int8);
    }

    return(0);
}


/***********************************************************************
*
* madrecCheckFile    checks the structure of a madrec data file
*
  Block (Physical record) structure:
  
  6720 16bit words (Int16)
  
  word[0]       = Total number of significant words in the block
		  record (all blocks are 13440 bytes long)
    
  word[1]       = Pointer to the first word of the first logical
		  record contained in the block.
  
		  (set to zero if the block doesn't contain
		   any complete logical records i.e. it just
		   contains the last part of a logical record.)
  
  word[2]       = Pointer to the first word of the last logical
		  record contained in block.
  
  word[word[0]-1] = Checksum.
  
  Logical Records:
  
  word[0 - 15]  = Same as NCAR binary logical records.
  word[16]      = Pointer to word 1 of previous logical record.
		   -could be contained in previous block.
		   -Set to zero in the first logical record of the file.
*/

int
madrecCheckFile(Madrec *madrecp)
{
    /*
    int i=0, nread=0, print=0, int j=0, recf=0, rp=0, ltot=0;
    static const char *err1="madrecCheckFile error - Bad block - 1";
    static const char *err2="madrecCheckFile error - Bad record - 1";
    static const char *err3="madrecCheckFile error - Bad record - 2";
    static const char *err4="madrecCheckFile error - Bad checksum";
    */

    return(0);
}


/***********************************************************************
*
* madrecCopy   Copies logical record from one madrec object to another
*
*   arguments:
*       madrec1p - pointer to the source madrec object
*       madrec2p - pointer to the destination madrec object
*
*   returns:
*       0 - Record copied successfully
*       1 - Empty source record
*
*/

int
madrecCopy (Madrec *madrec1p, Madrec *madrec2p)
{
    int ltot=0, i=0;
    static const char *err1="madrecCopy error - empty source record";

    if (madrec1p->recordp == (Int16 *)NULL) {
        (void) madrecSetError(madrec1p,err1);
        return(1);
    }
    ltot = madrec1p->recordp[0];

    if (madrec2p->recordp != (Int16 *)NULL && !(madrec2p->recordpInMem)) {
        free(madrec2p->recordp);
    }
    madrec2p->recordp = (Int16 *)malloc(ltot*sizeof(Int16));

    for (i=0; i<ltot; i++) {
        madrec2p->recordp[i] = madrec1p->recordp[i];
    }
    return(0);
}


/***********************************************************************
*
* madrecGetFileType   Gets file type
*
*   arguments:
*       madrecp - pointer to madrec object
*
*/
int
madrecGetFileType (Madrec *madrecp)
{
    return(cedarFileType(madrecp->filnam, MADRIGALBLOCKSIZE, CBFBLOCKSIZE));
}


/***********************************************************************
*
* madrecSetError   sets madrec error
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       0 - 
*
*/


int
madrecSetError (Madrec *madrecp, const char *error)
{

    free(madrecp->lastError); 
    madrecp->lastError = (char *)malloc(strlen(error)+1);
    (void) strcpy(madrecp->lastError, error);
    /*if (madrecp->pflag != 0)
        (void) fprintf(stderr, "%s\n", error);*/
    madrecp->pflag = 1;
    return(0);
}


/***********************************************************************
*
* madrecGetError   gets last madrec error
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       error string 
*
*/

char *
madrecGetError (Madrec *madrecp)
{
    return(madrecp->lastError);
}


/***********************************************************************
*
* madrecGetMissing   gets missing data value
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       double precision missing value 
*
*/

double
madrecGetMissing (Madrec *madrecp)
{
    return(missing);
}


/***********************************************************************
*
* madrecGetNumParms   gets number of different parameters in file
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       number of distint parameters
*
*/

int
madrecGetNumParms (Madrec *madrecp)
{
    return(madrecp->numParms);
}


/***********************************************************************
*
* madrecGetParmsList   gets list (int array) of different parameters
*                      codes in file
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       codes of distinct parameter 
*
*  This returned array is a pointer to an internal structure in Madrec;
*  it does not need to be freed by the user.
*
*/

int *
madrecGetParmsList (Madrec *madrecp)
{
    return(madrecp->parmsListp);
}
    

/***********************************************************************
*
* madrecGetParmLoc   gets location of parameter
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns:
*       parameter location:
*           1 - 1D array
*           2 - 2D array
*           3 - Derived
*
*/

int *
madrecGetParmLoc (Madrec *madrecp)
{
    return(madrecp->parmLocp);
}
    

/***********************************************************************
*
* madrecGetParmMin   gets minimum value of parameter
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns - Minimum value of parameter in entire file
*
*/

double *
madrecGetParmMin (Madrec *madrecp)
{
    return(madrecp->parmMinp);
}
    

/***********************************************************************
*
* madrecGetParmMax   gets maximum value of parameter
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns - Maximum value of parameter in entire file
*
*/

double *
madrecGetParmMax (Madrec *madrecp)
{
    return(madrecp->parmMaxp);
}

/***********************************************************************
*
* madrecHasCatalog   returns 1 if file has catalog record, 0 otherwise
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns - 1 if file has catalog record, 0 otherwise
*
*/

int madrecHasCatalog(Madrec *madrecp)
{
    int i;

    /* Generate key table if necessary */
    if (madrecp->indexp==(CedarIndex *)NULL) {
        madrecGenKeys(madrecp);
    }
    
    for (i=0; i < madrecp->nrecords; i++) 
    { 
        if (madrecp->indexp[i].isCatalog == 1)
	    return(1);
    }
    
    /* none found */
    return(0);
}


/***********************************************************************
*
* madrecHasHeader   returns 1 if file has header record, 0 otherwise
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns - 1 if file has header record, 0 otherwise
*
*/

int madrecHasHeader(Madrec *madrecp)
{
    int i;

    /* Generate key table if necessary */
    if (madrecp->indexp==(CedarIndex *)NULL) {
        madrecGenKeys(madrecp);
    }
    
    for (i=0; i < madrecp->nrecords; i++) 
    { 
        if (madrecp->indexp[i].isData == 0 && madrecp->indexp[i].isCatalog == 0)
	    return(1);
    }
    
    /* none found */
    return(0);
}

/***********************************************************************
*
* madrecGetSortedRecnoList   gets int array of recno's sorted by start time key
*
*   arguments:
*       madrecp - pointer to the madrec object
*
*   returns - int array of recno's sorted by start time key, length = nrecords
*
*/

int * madrecGetSortedRecnoList (Madrec *madrecp)
{
    CedarIndex * sortedCedarIndex;
    int i;

    /* if sortedRecnoList already exists, return it */
    if (madrecp->sortedRecnoList != (int *)NULL)
        return(madrecp->sortedRecnoList);

    /* Generate key table if necessary */
    if (madrecp->indexp==(CedarIndex *)NULL) {
        madrecGenKeys(madrecp);
    }

    /* create a temporary copy of madrecp->indexp that we can sort */
    sortedCedarIndex = malloc(madrecp->nrecords * sizeof(CedarIndex));
    memcpy(sortedCedarIndex, madrecp->indexp, madrecp->nrecords * sizeof(CedarIndex));

    /* sort sortedCedarIndex */
    qsort(sortedCedarIndex, 
          madrecp->nrecords,
          sizeof(CedarIndex),
          compareCedarIndices);

    /* allocate memory for madrecp->sortedRecnoList */
    madrecp->sortedRecnoList = malloc(madrecp->nrecords * sizeof(int));

    /* copy sorted recno from sortedCedarIndex to madrecp->sortedRecnoList */
    for (i=0; i < madrecp->nrecords; i++) 
    { 
            madrecp->sortedRecnoList[i] = sortedCedarIndex[i].recNo;
    }

    /* free temporary sorted copy of madrecp->indexp */
    free(sortedCedarIndex);

    return (madrecp->sortedRecnoList);
    
}


/***********************************************************************
*
* compareCedarIndices   a private method to compare one CedarIndex to another
*
*   arguments:
*       void * cedarIndex1 - void pointer to the first CedarIndex
*       void * cedarIndex2 - void pointer to the second CedarIndex
*
*   returns - if cedarIndex1 before cedarIndex2, return -1
*             if cedarIndex1 same as cedarIndex2, return 0
*             if cedarIndex1 after cedarIndex2, return 1
*
*/
int compareCedarIndices(const void * index1, const void * index2)
{
    CedarIndex * cedarIndexp1;
    CedarIndex * cedarIndexp2;

    /* cast both pointers to * CedarIndex */
    cedarIndexp1 = (CedarIndex *)index1;
    cedarIndexp2 = (CedarIndex *)index2;

    if (cedarIndexp1->startKey < cedarIndexp2->startKey)
    {
        return -1;
    }
    else if (cedarIndexp1->startKey > cedarIndexp2->startKey)
    {
        return 1;
    }
    /* otherwise startKeys are equal - now compare endKeys 
       To make header record come first, a later end key comes first */
    if (cedarIndexp1->endKey > cedarIndexp2->endKey)
    {
        return -1;
    }
    else if (cedarIndexp1->endKey < cedarIndexp2->endKey)
    {
        return 1;
    }
    /* both startKey and endKey are equal */
    return 0;
}

