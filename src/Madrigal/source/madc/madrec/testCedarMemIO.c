/*  $Id: testCedarMemIO.c,v 1.10 2008/08/15 19:02:22 brideout Exp $ */

/*
modification history
--------------------
00a,21Feb00         original
*/

/* 
* USAGE: testCedarMemIO
* This program test Cedar Memory IO.
* 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <cedarIO.h>
#include <cedar.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{
    int i=0, ind1=0, ind2=0, nrec=0, debug=0;
    FILE *finp, *foutp;
    char madfile[128] = "";
    Int16 *cedarRecordp=(Int16 *)NULL;
    int fileType=0;
    void timestart(), timestop();
    char command[128];
    int recordpInMem = 0;

    /* madrigal state variables */
    int madrigalBlockSize=6720;
    int sigWords=0, blockIndex=0, prevRec=0, thisRec=0;
    Int16 *blockp=(Int16 *)NULL;

    /* Cbf state parameters */
    int cbfBlockSize=4096;

    /* Memory file state parameters */
    Int16 *cedarFile1p=(Int16 *)NULL, *cedarFile2p=(Int16 *)NULL;
    int mempos1, mempos2;
    int fileSize1, fileSize2;

    /* Get the name of the input madrigal file */
    cedarGetMadroot(madfile);
    if (argc == 1) {
        strcat(madfile, "/experiments/1998/mlh/20jan98/mil980120g.002");
    } else if (argc == 2) {
        strcpy(madfile, argv[1]);
    } else {
        return(-1);
    }

    fileType = cedarFileType(madfile, madrigalBlockSize, cbfBlockSize);
    if (debug) 
        (void) printf("%s - fileType = %d\n", madfile,fileType);

    /*** Input file to Madrigal ***/
    if ((finp = fopen(madfile, "r")) == (FILE *)NULL) {
        printf("Input file %s does not exist\n", madfile);
        return(1);
    }

    foutp = fopen("file1", "w");
    sigWords = 0;
    blockIndex = 0;
    blockp=(Int16 *)NULL;
    prevRec = 0;
    thisRec = 0;
    nrec = 0;
    while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
                                        madrigalBlockSize,
                                        &sigWords)) == 0) {
        ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
                                     madrigalBlockSize,
                                     &blockIndex,
                                     &blockp,
                                     &prevRec,
                                     &thisRec);
        nrec++;
    }
    if (debug) 
        (void) printf("Madrigal to Madrigal - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        if (debug) 
            (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        if (debug) 
            (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file1", madrigalBlockSize, cbfBlockSize);
    if (debug) 
        (void) printf("file1 - fileType = %d\n", fileType);


    /*** Madrigal to Memory ***/
    finp = fopen("file1", "r");
    sigWords = 0;
    mempos1 = 0;
    /*
    (void) fseek(finp, 0L, SEEK_END);
    fileSize = ftell(finp);
    if (debug) 
        printf("fileSize = %d\n", fileSize);
    (void) fseek(finp, 0L, SEEK_SET);
    cedarFile1p = malloc(fileSize+2);
    */
    fileSize1 = 0;
    nrec = 0;
    while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
                                        madrigalBlockSize,
                                        &sigWords)) == 0) {
        ind2 = putMemNextCedarUnblockedRecord(&cedarFile1p, &cedarRecordp,
                                              &fileSize1, &mempos1);
        nrec++;
    }
    if (debug) 
        (void) printf("Madrigal to Memory - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        if (debug) 
            (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    (void) fclose(finp);


    /*** Memory to Memory ***/
    if (debug)
        timestart();
    for (i=0; i<100; i++) {
        if (cedarFile2p != (Int16 *)NULL) free(cedarFile2p);
        mempos1 = 0;
        mempos2 = 0;
        /*
        cedarFile2p = malloc(fileSize+2);
        */
        fileSize2 = 0;
        nrec = 0;
        while((ind1 = getMemNextCedarUnblockedRecord(&cedarFile1p, 
                                                     &cedarRecordp,
                                                     &mempos1,
                                                     &recordpInMem)) == 0) {
            ind2 = putMemNextCedarUnblockedRecord(&cedarFile2p, &cedarRecordp,
                                                  &fileSize2,
                                                  &mempos2);
            nrec++;
        }
        /* (void) printf("Memory to Memory - %d records copied\n", nrec); */
        if (cedarRecordp != (Int16 *)NULL) {
            if (debug) 
                (void) printf("  Freeing cedarRecordp\n");
            free(cedarRecordp);
            cedarRecordp = (Int16 *)NULL;
        }
    }
    if (debug) 
        timestop("100 memory to memory copies");


    /*** Memory to Madrigal ***/
    foutp = fopen("file2", "w");
    blockIndex = 0;
    blockp = (Int16 *)NULL;
    prevRec = 0;
    thisRec = 0;
    mempos2 = 0;
    nrec = 0;
    while((ind1 = getMemNextCedarUnblockedRecord(&cedarFile2p, &cedarRecordp,
                                                 &mempos2, &recordpInMem)) == 0) {
        ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
                                     madrigalBlockSize,
                                     &blockIndex,
                                     &blockp,
                                     &prevRec,
                                     &thisRec);
        nrec++;
    }
    if (cedarFile2p != (Int16 *)NULL) free(cedarFile2p);
    if (debug) 
        (void) printf("Memory to Madrigal- %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        if (debug) 
            (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        if (debug) 
          (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(foutp);
    fileType = cedarFileType("file2", madrigalBlockSize, cbfBlockSize);
    if (debug) {
        (void) printf("file2 - fileType = %d\n", fileType);
        (void) printf("Diff %s and file2 - They could be different\n", madfile);
        sprintf(command, "diff %s file2", madfile);
        printf("Executing %s\n", command);
        system(command);
        (void) printf("Diff file1 and file2 - They should be the same\n");
        sprintf(command, "diff file1 file2");
        printf("Executing %s\n", command);
        system(command);
        printf("If there was no error message after the second diff command, the test ran successfully\n");
    }

    free(cedarRecordp);
    if (cedarFile1p != NULL)
        free(cedarFile1p);

    return(0);
}

struct tms {
    time_t tms_utime;
    time_t tms_stime;
    time_t tms_cutime;
    time_t tms_cstime;
};
long times();
#define TICKS 60.
static struct tms tbuf1;
static long real1;

void timestart()    /* start timer */
{
    real1 = times(&tbuf1);
}

void timestop(char *msg)    /* Stop timer */
{
    struct tms tbuf2;
    long real2;

    real2 = times(&tbuf2);

    fprintf(stderr, "%s: real %.2f; user %.2f; sys %.2f\n",msg,
      (real2-real1)/TICKS,
      (tbuf2.tms_utime-tbuf1.tms_utime)/TICKS,
      (tbuf2.tms_stime-tbuf1.tms_stime)/TICKS);
}

    
