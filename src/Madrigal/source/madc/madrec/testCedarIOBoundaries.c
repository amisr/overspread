/*  $Id: testCedarIOBoundaries.c,v 1.2 2002/03/22 16:56:28 jmh Exp $ */

/*
modification history
--------------------
00a,21Feb00         original
*/

/* 

USAGE: testCedarIOBoundries madrigalFileName numberOfMadrigalCopies numberOfCbfCopies
       e.g.: testCedarIOBoundries /opt/madrigal/experiments/1998/mlh/23mar98/mil980323g.003 3 3

The Madrigal and CBF formats are blocked and each block contains
control words in addition to CEDAR records. In practice, the block
lengths are fixed, but the read and write routines in cedarIO.c allow
an arbitrary block size (beyond the minimum possible size). This
program test the blocking by repeatedly doing two transformations:

   Madrigal (standard block size) -> Madrigal(random block size) -> Madrigal(standard block size)
   Madrigal (standard block size) -> CBF (random block size) -> Madrigal (standard block size)

The first and third files, file1 and file2, are then compared.
Relatively small random block sizes are used to enhance the liklihood
of encountering edge effects.

*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <cedarIO.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{

    int i=0, nwords=0, ind1=0, ind2=0, nrec=0, nmad=0, ncbf=0;
    char infile[100];
    Int16 w1=0, w2=0;
    FILE *finp, *foutp;
    Int16 *cedarRecordp=(Int16 *)NULL;

    /* madrigal state variables */
    int madrigalBlockSize=6720, madrigalBlockSizex=0;
    int sigWords=0, blockIndex=0, prevRec=0, thisRec=0;
    Int16 *blockp=(Int16 *)NULL;

    /* Cbf state parameters */
    int cbfBlockSize=4096, cbfBlockSizex=0;
    int forceCosRead=0, initPos8=0, initFwi=0, initPos=0, initLCosBlock=0,
        lCosBlock=0, gpos=0, fwi=0;
    Int16 *cosRecordp=(Int16 *) NULL;
    int lbuf=2;    /* Are two extra words in Cos block */
    int ppos=1;    /* First word of Cos block is it's length */
    int blockNumber=0;
    long lastControlWord=-1;
    int previousFileIndex=-1, previousRecordIndex=-1;

    if (argc == 4) {
        strcpy(infile, argv[1]);
        nmad = atoi(argv[2]);
        ncbf = atoi(argv[3]);
    } else {
        (void) printf("Usage: testCedarIOBoundries madrigalFileName numberOfMadrigalCopies numberOfCbfCopies\n");
        return(1);
    }

    /* Madrigal to Madrigal */       
    finp = fopen(infile, "r");
    foutp = fopen("file1", "w");
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
        if (ind1 != 0 || ind2 != 0)
            (void) printf("Error: ind1,ind2 = %d %d\n", ind1,ind2);
        nrec++;
    }
    /* (void) printf("Initialization - %d records copied\n", nrec); */
    if (cedarRecordp != (Int16 *)NULL) {
        free(cedarRecordp);
        cedarRecordp = (Int16 *)NULL;
    } 
    if (blockp != (Int16 *)NULL) {
        free(blockp);
        blockp = (Int16 *)NULL;
    } 
    (void) fclose(finp);
    (void) fclose(foutp);


    for (i=0; i<nmad; i++) {
    
	madrigalBlockSizex = 20 + 1000*((float)rand())/RAND_MAX;
	/* printf("madrigalBlockSizex = %d\n",  madrigalBlockSizex); */
    
	/* Madrigal to Madrigal */       
	finp = fopen("file1", "r");
	foutp = fopen("file2", "w");
	blockIndex = 0;
	blockp=(Int16 *)NULL;
	prevRec = 0;
	thisRec = 0;
	nrec = 0;
	while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
					    madrigalBlockSize,
					    &sigWords)) == 0) {
	    ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
					 madrigalBlockSizex,
					 &blockIndex,
					 &blockp,
					 &prevRec,
					 &thisRec);
	    nrec++;
	}
	/* (void) printf("    %d records copied\n", nrec); */
	if (cedarRecordp != (Int16 *)NULL) {
	    free(cedarRecordp);
	    cedarRecordp = (Int16 *)NULL;
	} 
	if (blockp != (Int16 *)NULL) {
	    free(blockp);
	    blockp = (Int16 *)NULL;
	} 
	(void) fclose(finp);
	(void) fclose(foutp);
	
    
	/* Madrigal to Madrigal */       
	finp = fopen("file2", "r");
	foutp = fopen("file3", "w");
	blockIndex = 0;
	blockp=(Int16 *)NULL;
	prevRec = 0;
	thisRec = 0;
	nrec = 0;
	while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
					    madrigalBlockSizex,
					    &sigWords)) == 0) {
	    ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
					 madrigalBlockSize,
					 &blockIndex,
					 &blockp,
					 &prevRec,
					 &thisRec);
	    nrec++;
	}
	/* (void) printf("    %d records copied\n", nrec); */
	if (cedarRecordp != (Int16 *)NULL) {
	    free(cedarRecordp);
	    cedarRecordp = (Int16 *)NULL;
	} 
	if (blockp != (Int16 *)NULL) {
	    free(blockp);
	    blockp = (Int16 *)NULL;
	} 
	(void) fclose(finp);
	(void) fclose(foutp);

        finp = fopen("file1", "r");
        foutp = fopen("file3", "r");
        nwords = 0;
        for (;;) {
            if (fread(&w1, (size_t) 2, (size_t) 1,  finp) != 1) break;
            if (fread(&w2, (size_t) 2, (size_t) 1, foutp) != 1) break;
            nwords++;
             if (w1 != w2) {
                printf("***> The files are different - %6d %6d %6d\n <***", 
                    i,w1,w2);
                return(0);
            }
        }
        printf("    OK - madrigal - blockSize = %4d, %d words compared\n", 
            madrigalBlockSizex, nwords);
	(void) fclose(finp);
	(void) fclose(foutp);

    }
    
    for (i=0; i<ncbf; i++) {

	cbfBlockSizex = 20 + 492*((float)rand())/RAND_MAX;
        cbfBlockSizex = 8*cbfBlockSizex ;  /* Must be multiple of 8 bytes */
	/* printf("cbfBlockSizex = %d\n",  cbfBlockSizex); */
    
	/* Madrigal to Cbf */
	finp = fopen("file1", "r");
	foutp = fopen("file2", "w+");
	lbuf = 2;
	ppos = 1;
	cosRecordp = (Int16 *) NULL;
	blockNumber = 0;
	previousFileIndex = -1;
	previousRecordIndex = -1;
	lastControlWord = -1;
	nrec = 0;   
	while((ind1 = getNextMadrigalRecord(finp, &cedarRecordp,
					    madrigalBlockSize,
					    &sigWords)) == 0) {
	    ind2 = putNextCedarCbfRecord(foutp, &cedarRecordp,
					 cbfBlockSizex,
					 &lbuf,
					 &ppos,
					 &cosRecordp,
					 &blockNumber,
					 &previousFileIndex,
					 &previousRecordIndex,
					 &lastControlWord);
	    nrec++;
	}
	/* (void) printf("Madrigal to Cbf - %d records copied\n", nrec); */
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
	if (cosRecordp != (Int16 *)NULL) {
	    free(cosRecordp);
	    cosRecordp = (Int16 *)NULL;
	}
	(void) fclose(finp);
	(void) fclose(foutp);
	
	/* Cbf to Madrigal */
	/* N.B.: Only copies first file */
	finp = fopen("file2", "r");
	foutp = fopen("file3", "w");
        forceCosRead = 0;
	lCosBlock = 0;
	gpos = 0;
	fwi = 0;
	cosRecordp = (Int16 *) NULL;
	blockIndex = 0;
	blockp=(Int16 *)NULL;
	prevRec = 0;
	thisRec = 0;
	nrec = 0;  
	while((ind1 = getNextCedarCbfRecord(finp, &cedarRecordp,
                                            forceCosRead,
					    cbfBlockSizex,
                                            &initPos8,
                                            &initFwi,
                                            &initPos,
                                            &initLCosBlock,
					    &lCosBlock,
					    &gpos,
					    &fwi,
					    &cosRecordp)) == 0) {
	    ind2 = putNextMadrigalRecord(foutp, &cedarRecordp,
					 madrigalBlockSize,
					 &blockIndex,
					 &blockp,
					 &prevRec,
					 &thisRec);
	    nrec++;
	}
	/* (void) printf("Cbf to Madrigal - %d records copied\n", nrec); */
	if (cedarRecordp != (Int16 *)NULL) {
	    free(cedarRecordp);
	    cedarRecordp = (Int16 *)NULL;
	} 
	if (blockp != (Int16 *)NULL) {
	    free(blockp);
	    blockp = (Int16 *)NULL;
	} 
	if (cosRecordp != (Int16 *)NULL) {
	    free(cosRecordp);
	    cosRecordp = (Int16 *)NULL;
	}
	(void) fclose(finp);
	(void) fclose(foutp);

        finp = fopen("file1", "r");
        foutp = fopen("file3", "r");
        nwords = 0;
        for (;;) {
            if (fread(&w1, (size_t) 2, (size_t) 1,  finp) != 1) break;
            if (fread(&w2, (size_t) 2, (size_t) 1, foutp) != 1) break;
            nwords++;
             if (w1 != w2) {
                printf("***> The files are different - %6d %6d %6d <***\n", 
                    i,w1,w2);
                return(0);
            }
        }
        if (nwords > 0) {
            printf("    OK - cbf -      blockSize = %4d, %d words compared\n",
                cbfBlockSizex, nwords);
        } else {
            printf("    ***> ERROR - %d words compared <***\n", nwords);
            return(0);
        }
	(void) fclose(finp);
	(void) fclose(foutp);

    }
    
    return(0);
}
