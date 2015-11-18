/*  $Id: testCedarIO.c,v 1.5 2002/03/22 16:56:27 jmh Exp $ */
/*
modification history
--------------------
00a,21Feb00         original
01a,06nov01         input file now an argument
*/

/* 
USAGE: testCedarIO MadrigalFileName
       e.g.: testCedarIO /opt/madrigal/experiments/1998/mlh/23mar98/mil980323g.003

testCedarIO is the main test program for the low-level Cedar IO
routines in cedarIO.c. It takes the name of a Madrigal format file as
input. A series of 11 translations through all CEDAR file types is then
carried out. The translated files are in file1,file2,...file11. If all
translations are successful, file11 will be identical to file1. The
input file may be different because the words after the last record in
the last block of the Madrigal file are undefined. However, the input
file and file11 should be the same length. The translations are:
    Madrigal to Madriga l  
    Madrigal to Ascii
    Ascii to Cbf
    Cbf to Blocked Binary
    Blocked Binary to Unblocked Binary
    Unblocked Binary to Madrigal
    Madrigal to Unblocked Binary
    Unblocked Binary to Blocked Binary
    Blocked Binary to Cbf
    Cbf to Ascii
    Ascii to Madrigal

*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{

    int ind1, ind2, nrec;
    char infile[100];
    FILE *finp, *foutp;
    Int16 *cedarRecordp=(Int16 *)NULL;
    int fileType;

    /* madrigal state variables */
    int madrigalBlockSize=6720;
    int sigWords=0, blockIndex=0, prevRec=0, thisRec=0;
    Int16 *blockp=(Int16 *)NULL;

    /* Cbf state parameters */
    int cbfBlockSize=4096;
    int forceCosRead=0, initPos8, initPos=0, initLCosBlock=0,
        lCosBlock=0, gpos=0, initFwi=0, fwi=0;
    Int16 *cosRecordp=(Int16 *) NULL;
    int lbuf=2;    /* Are two extra words in Cos block */
    int ppos=1;    /* First word of Cos block is it's length */
    int blockNumber=0;
    long lastControlWord=-1;
    int previousFileIndex=-1, previousRecordIndex=-1;

    /* Blocked binary state parameters */
    Int16 binaryBlockSize = -1;
    Int16 *binaryBlockp=(Int16 *) NULL;
    int maxBlock=0, lblock=2, binaryPos1=-1, binaryPos2=1;

    if (argc == 2) {
        strcpy(infile, argv[1]);
    } else {
        (void) printf("Usage: testCedarIO input_file\n");
        return(1);
    }

    /* Check file type of start file */
    (void) printf("Input file = %s\n", infile);
    fileType = cedarFileType(infile, madrigalBlockSize, cbfBlockSize);
    (void) printf("%s - fileType = %d\n", infile,fileType);

    /*** Madrigal to Madrigal ***/       
    finp = fopen(infile, "r");
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
    (void) printf("Madrigal to Madrigal - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file1", madrigalBlockSize, cbfBlockSize);
    (void) printf("file1 - fileType = %d\n", fileType);


    /*** Madrigal to Ascii ***/
    finp = fopen("file1", "r");
    foutp = fopen("file2", "w");
    sigWords = 0;
    nrec = 0;
    while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
                                        madrigalBlockSize,
                                        &sigWords)) == 0) {
        ind2 = putNextCedarAsciiRecord(foutp, &cedarRecordp);
        nrec++;
    }
    (void) printf("Madrigal to Ascii - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file2", madrigalBlockSize, cbfBlockSize);
    (void) printf("file2 - fileType = %d\n", fileType);


    /*** Ascii to Cbf ***/
    finp = fopen("file2", "r");
    foutp = fopen("file3", "w+");
    lbuf = 2;
    ppos = 1;
    cosRecordp = (Int16 *) NULL;
    blockNumber = 0;
    previousFileIndex = -1;
    previousRecordIndex = -1;
    lastControlWord = -1;
    nrec = 0;
    while((ind1 = getNextCedarAsciiRecord(finp, &cedarRecordp)) == 0) {
        ind2 = putNextCedarCbfRecord(foutp, &cedarRecordp,
                                     cbfBlockSize,
                                     &lbuf,
                                     &ppos,
                                     &cosRecordp,
                                     &blockNumber,
                                     &previousFileIndex,
                                     &previousRecordIndex,
                                     &lastControlWord);
        nrec++;
    }
    flushCedarCbfRecord(foutp, &cedarRecordp,
                        cbfBlockSize,
                        &lbuf,
                        &ppos,
                        &cosRecordp,
                        &blockNumber,
                        &previousFileIndex,
                        &previousRecordIndex,
                        &lastControlWord);
    endFileCedarCbfRecord(foutp, &cedarRecordp,
                          cbfBlockSize,
                          &previousFileIndex,
                          &lastControlWord);
    endDataCedarCbfRecord(foutp, &cedarRecordp,
                          cbfBlockSize,
                          &lastControlWord);
    (void) printf("Ascii to Cbf - %d records copied\n", nrec);
     if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (cosRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cosRecordp\n");
        free(cosRecordp);
        cosRecordp = (Int16 *)NULL;
    }
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file3", madrigalBlockSize, cbfBlockSize);
    (void) printf("file3 - fileType = %d\n", fileType);


    /*** Cbf to Blocked Binary ***/
    /* N.B.: Only copies first file */
    finp = fopen("file3", "r");
    foutp = fopen("file4", "w");
    lCosBlock = 0;
    gpos = 0;
    fwi = 0;
    cosRecordp = (Int16 *) NULL;
    lblock = 2;
    binaryPos2 = 1;
    binaryBlockp = (Int16 *) NULL;
    nrec = 0;  
    while((ind1 = getNextCedarCbfRecord(finp, &cedarRecordp,
                                        forceCosRead,
                                        cbfBlockSize,
                                        &initPos8,
                                        &initFwi,
                                        &initPos,
                                        &initLCosBlock,
                                        &lCosBlock,
                                        &gpos,
                                        &fwi,
                                        &cosRecordp)) == 0) {
        ind2 = putNextCedarBlockedRecord(foutp, &cedarRecordp,
                                         &maxBlock,
                                         &lblock,
                                         &binaryPos2,
                                         &binaryBlockp);
        nrec++;
    }
    flushCedarBlockedRecord(foutp, &cedarRecordp,
                            &maxBlock,
                            &lblock,
                            &binaryPos2,
                            &binaryBlockp);
    (void) printf("Cbf to Blocked Binary - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (binaryBlockp != (Int16 *)NULL) {
        (void) printf("  Freeing binaryBlockp\n");
        free(binaryBlockp);
        blockp = (Int16 *)NULL;
    } 
    if (cosRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cosRecordp\n");
        free(cosRecordp);
        cosRecordp = (Int16 *)NULL;
    }
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file4", madrigalBlockSize, cbfBlockSize);
    (void) printf("file4 - fileType = %d\n", fileType);


    /*** Blocked Binary to Unblocked Binary ***/
    finp = fopen("file4", "r");
    foutp = fopen("file5", "w");
    binaryBlockSize = -1;
    binaryPos1 = -1;
    nrec = 0;
    while((ind1 = getNextCedarBlockedRecord(finp, &cedarRecordp,
                                            &binaryBlockSize,
                                            &binaryPos1)) == 0) {
        ind2 = putNextCedarUnblockedRecord(foutp, &cedarRecordp);
        nrec++;
    }
    (void) printf("Blocked Binary to Unblocked Binary - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file5", madrigalBlockSize, cbfBlockSize);
    (void) printf("file5 - fileType = %d\n", fileType);


    /*** Unblocked Binary to Madrigal ***/
    finp = fopen("file5", "r");
    foutp = fopen("file6", "w");
    blockIndex = 0;
    blockp = (Int16 *)NULL;
    prevRec = 0;
    thisRec = 0;
    nrec = 0;
    while((ind1 = getNextCedarUnblockedRecord(finp, &cedarRecordp)) == 0) {
        ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
                                     madrigalBlockSize,
                                     &blockIndex,
                                     &blockp,
                                     &prevRec,
                                     &thisRec);
        nrec++;
    }
    (void) printf("Unblocked Binary to Madrigal- %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file6", madrigalBlockSize, cbfBlockSize);
    (void) printf("file6 - fileType = %d\n", fileType);


    /*** Madrigal to Unblocked Binary***/
    finp = fopen("file6", "r");
    foutp = fopen("file7", "w");
    sigWords = 0;
    nrec = 0;
    while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
                                    madrigalBlockSize,
                                    &sigWords)) == 0) {
        ind2 = putNextCedarUnblockedRecord(foutp, &cedarRecordp);
        nrec++;
    }
    (void) printf("Madrigal to Unblocked Binary - %d records copied\n", nrec);
    if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file7", madrigalBlockSize, cbfBlockSize);
    (void) printf("file7 - fileType = %d\n", fileType);


    /*** Unblocked Binary to Blocked Binary ***/
    finp = fopen("file7", "r");
    foutp = fopen("file8", "w");
    maxBlock = 0;
    lblock = 2;
    binaryPos2 = 1;
    binaryBlockp = (Int16 *) NULL;
    nrec = 0;   
    while((ind1 = getNextCedarUnblockedRecord(finp, &cedarRecordp)) == 0) {
        ind2 = putNextCedarBlockedRecord(foutp, &cedarRecordp,
                                         &maxBlock,
                                         &lblock,
                                         &binaryPos2,
                                         &binaryBlockp);
        nrec++;
    }
    (void) printf("Unblocked Binary to Blocked Binary - %d records copied\n", nrec);
    flushCedarBlockedRecord(foutp, &cedarRecordp,
                            &maxBlock,
                            &lblock,
                            &binaryPos2,
                            &binaryBlockp);
     if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
     if (binaryBlockp != (Int16 *)NULL) {
        (void) printf("  Freeing binaryBlockp\n");
        free(binaryBlockp);
        binaryBlockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file8", madrigalBlockSize, cbfBlockSize);
    (void) printf("file8 - fileType = %d\n", fileType);


    /*** Blocked Binary to Cbf ***/
    finp = fopen("file8", "r");
    foutp = fopen("file9", "w+");
    binaryBlockSize = -1;
    binaryPos1 = -1;
    lbuf = 2;
    ppos = 1;
    cosRecordp = (Int16 *) NULL;
    blockNumber = 0;
    previousFileIndex = -1;
    previousRecordIndex = -1;
    lastControlWord = -1;
    nrec = 0;   
    while((ind1 = getNextCedarBlockedRecord(finp, &cedarRecordp,
                                            &binaryBlockSize,
                                            &binaryPos1)) == 0) {
        ind2 = putNextCedarCbfRecord(foutp, &cedarRecordp,
                                     cbfBlockSize,
                                     &lbuf,
                                     &ppos,
                                     &cosRecordp,
                                     &blockNumber,
                                     &previousFileIndex,
                                     &previousRecordIndex,
                                     &lastControlWord);
        nrec++;
    }
    (void) printf("Blocked Binary to Cbf - %d records copied\n", nrec);
    flushCedarCbfRecord(foutp, &cedarRecordp,
                        cbfBlockSize,
                        &lbuf,
                        &ppos,
                        &cosRecordp,
                        &blockNumber,
                        &previousFileIndex,
                        &previousRecordIndex,
                        &lastControlWord);
    endFileCedarCbfRecord(foutp, &cedarRecordp,
                          cbfBlockSize,
                          &previousFileIndex,
                          &lastControlWord);
    endDataCedarCbfRecord(foutp, &cedarRecordp,
                          cbfBlockSize,
                          &lastControlWord);
     if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (cosRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cosRecordp\n");
        free(cosRecordp);
        cosRecordp = (Int16 *)NULL;
    }
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file9", madrigalBlockSize, cbfBlockSize);
    (void) printf("file9 - fileType = %d\n", fileType);


    /*** Cbf to Ascii ***/
    /* N.B.: Only copies first file */
    finp = fopen("file9", "r");
    foutp = fopen("file10", "w");
    lCosBlock = 0;
    gpos = 0;
    fwi = 0;
    cosRecordp = (Int16 *) NULL;
    nrec = 0;  
    while((ind1 = getNextCedarCbfRecord(finp, &cedarRecordp,
                                        forceCosRead,
                                        cbfBlockSize,
                                        &initPos8,
                                        &initFwi,
                                        &initPos,
                                        &initLCosBlock,
                                        &lCosBlock,
                                        &gpos,
                                        &fwi,
                                        &cosRecordp)) == 0) {
        ind2 = putNextCedarAsciiRecord(foutp, &cedarRecordp);
        nrec++;
    }
    (void) printf("Cbf to Ascii - %d records copied\n", nrec);
     if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (cosRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cosRecordp\n");
        free(cosRecordp);
        cosRecordp = (Int16 *)NULL;
    }
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file10", madrigalBlockSize, cbfBlockSize);
    (void) printf("file10 - fileType = %d\n", fileType);


    /*** Ascii to Madrigal ***/
    finp = fopen("file10", "r");
    foutp = fopen("file11", "w");
    blockIndex = 0;
    prevRec = 0;
    thisRec = 0;
    nrec = 0;
    while((ind1 = getNextCedarAsciiRecord(finp, &cedarRecordp)) == 0) {
        ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
                                     madrigalBlockSize,
                                     &blockIndex,
                                     &blockp,
                                     &prevRec,
                                     &thisRec);
        nrec++;
    }
    (void) printf("Ascii to Madrigal - %d records copied\n", nrec);
     if (cedarRecordp != (Int16 *)NULL) {
        (void) printf("  Freeing cedarRecordp\n");
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        (void) printf("  Freeing blockp\n");
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);
    fileType = cedarFileType("file11", madrigalBlockSize, cbfBlockSize);
    (void) printf("file11 - fileType = %d\n", fileType);

    printf ("diff file1 file11 to verify that all translations are correct.\n");
    printf("file11 should be the same length as the input file.\n");
    printf("compareFiles may be used to compare the input file to file11.\n");
    printf("The files should differ only in the last block and file11 should\n");
    printf("   always be 0 when the files differ.\n");

    return(0);
}
