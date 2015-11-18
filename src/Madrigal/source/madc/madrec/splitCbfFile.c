/*  $Id: splitCbfFile.c,v 1.2 2002/06/11 17:21:58 brideout Exp $ */
/*
modification history
--------------------
00a,02apr02         original
*/

/* 
USAGE: splitCbfFile cbfFileName
       e.g.: splitCbfFile sts660125a.cbf

splitCbfFile splits the files in a multi-file CBF file into individual
files.  This needs to be done prior to entering CBF files from NCAR
into the Madrigal Database. This is a rare example of a production program
which uses the low-level cedarIO routines. The reason is that above
this level the cbf distiction between end-of-file and end-of-data is
lost.

*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>
#include <cedar.h>

int
main (argc, argv)
     int     argc;
     char    *argv[];
{

    int ind1, ind2, nrec;
    char infile[100];
    FILE *foutp = NULL;
    FILE *finp, *flp;
    Int16 *cedarRecordp=(Int16 *)NULL;
    int fileType;

    /* madrigal state variables */
    int madrigalBlockSize=6720;

    /* Cbf state parameters */
    int cbfBlockSize=4096;
    int forceCosRead=0, initPos8, initPos=0, initLCosBlock=0,
        lCosBlock=0, gpos=0, initFwi=0, fwi=0;
    Int16 *cosRecordp=(Int16 *) NULL;

    int n, ibyr, ibdt, ibymd, newfile, kinst, month, day,
        fileCategory, action;

    char outfile[128], tag[6], staName[4], expDir[128], expName[128],
         y[3], m[3], d[3], fileList[100];
    char months[12][4];
    kinst = 0;
    sprintf(months[0], "%s", "jan");
    sprintf(months[1], "%s", "feb");
    sprintf(months[2], "%s", "mar");
    sprintf(months[3], "%s", "apr");
    sprintf(months[4], "%s", "may");
    sprintf(months[5], "%s", "jun");
    sprintf(months[6], "%s", "jul");
    sprintf(months[7], "%s", "aug");
    sprintf(months[8], "%s", "sep");
    sprintf(months[9], "%s", "oct");
    sprintf(months[10], "%s", "nov");
    sprintf(months[11], "%s", "dec");

    if (argc == 3) {
        strcpy(infile, argv[1]);
        strcpy(fileList, argv[2]);
    } else {
        (void) printf("Usage: splitCbfFile inputFile outputFile\n");
        return(1);
    }
    flp = fopen(fileList, "w");

    /* Check file type of start file */
    (void) printf("Input file = %s\n", infile);
    fileType = cedarFileType(infile, madrigalBlockSize, cbfBlockSize);
    (void) printf("%s - fileType = %d\n", infile,fileType);

 
    /*** Cbf to Ascii ***/
    finp = fopen(infile, "r");
    n = 0;
    lCosBlock = 0;
    gpos = 0;
    fwi = 0;
    cosRecordp = (Int16 *) NULL;
    nrec = 0;
    newfile = 1;
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
                                        &cosRecordp)) != -2) {
        if (ind1 == 0) {
            kinst = cedarGetKinst(cedarRecordp);
            ibyr = cedarGetIbyr(cedarRecordp);
            ibdt = cedarGetIbdt(cedarRecordp);
            month = ibdt/100;
            day = ibdt - 100*month;
            sprintf(m, "%2.2d", month);
            sprintf(d, "%2.2d", day);
            sprintf(y, "%2.2d", ibyr - 100*(ibyr/100));
            ibymd = 10000*(ibyr - 100*(ibyr/100)) + ibdt;
            if (newfile == 1) {
                n++;
                sprintf(tag,"%6.6d", ibymd);
                staName[0] = infile[0];
                staName[1] = infile[1];
                staName[2] = infile[2];
                staName[3] = '\0';
                strcpy(outfile, staName);
                strcat(outfile, tag);
                strcat(outfile, "g.001");
                foutp = fopen(outfile, "w");
                newfile = 0;
                nrec = 0;

                sprintf(expDir, "%4.4d", ibyr);
                strcat(expDir, "/");
                strcat(expDir, staName);
                strcat(expDir, "/");
                strcat(expDir, d);
                strcat(expDir, months[month-1]);
                strcat(expDir, y);

/* sts660125g.001 1966/sts/25jan66 41  "St Santin I.S. Radar data" 1  1 */

            }
            ind2 = putNextCedarAsciiRecord(foutp, &cedarRecordp);
            nrec++;
        } else if (ind1 == -1) {
            /* printf("Wrote %d records to %s\n", nrec,outfile); */
            strcpy(expName, "\"St Santin Data Archive\"");
            fileCategory = 1;
            action = 1;
            printf("%s %s %d %s %d %d\n",
                outfile, expDir, kinst, expName, fileCategory, action);
            fprintf(flp, "%s %s %d %s %d %d\n",
                outfile, expDir, kinst, expName, fileCategory, action);
            ibyr = 0;
            ibdt = 0;
            ibymd = 0;
            (void) fclose(foutp);
            newfile = 1;
        } else {
            printf("Fatal error: ind1 = %d\n", ind1);
            return(-1);
        }
     }

    (void) fclose(finp);
    (void) fclose(foutp);

    return(0);
}
