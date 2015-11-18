/*  $Id: madrec.h,v 1.17 2005/06/07 16:51:27 brideout Exp $ */

/* madrigal record structure definition            */
/*				                   */
/* jmh  04/22/96        original                   */
/* jmh  03/24/00        Read/write all file types  */

#ifndef _MADRECH_
#define _MADRECH_

#include "cedarIO.h"

#define MAXFCH 500
#define MAXPARCODES 2000
#define MAXINSTRUMENTS 5000
#define MAXCATEGORIES  100
#define MAX_KINST      500  /* maximum kinst values per file  */
#define MAX_KINDAT     500  /* maximum kindat values per file */
#define DESC_LEN 40
#define DESC16_LEN 12
#define UNIT_LEN 9
#define MNEM_LEN 21
#define CAT_LEN  50
#define INST_LEN 100           /* max length of an instrument name */
#define NO_ERR_STR "No errors"


enum {
    MADRIGALBLOCKSIZE = 6720,
    CBFBLOCKSIZE = 4096
};

typedef struct cedarIndex {
    /* this structure is used for searching an array of records */
    int recNo;
    double startKey;
    double endKey;
    int pos;
    int posPrev;
    Int16 *int16p1;
    int int1;
    int int2;
    int int3;
    int int4;
    int int5;
    int int6;
    int int7;
    int int8;
    long long1;
    int isData;     /* 1 if data record, 0 if catalog or header */
    int isCatalog;  /* i if catalog record, 0 otherwise */
} CedarIndex;

typedef struct MadrigalParms {

    /* read/write */
    int blockSize;

    /* read */
    int sigWords;

    /* write */
    int blockIndex;
    Int16 *blockp;
    int prevRec;
    int thisRec;

} MadrigalParms;

typedef struct BlockedBinaryParms {

    /* read/write */
    int pos;

    /* read */
    Int16 lBlock;

    /* write */
    int maxBlock;
    int lbuf;
    Int16 *blockp;

} BlockedBinaryParms;

typedef struct CbfParms {

    /* read/write */
    int blockSize;
    int pos;
    Int16 *cosRecordp;

    /* read */
    int lCosBlock;
    int fwi;

    /* Random read */
    int initPos8;
    int initFwi;
    int initPos;
    int initLCosBlock;

    /* write */
    int lbuf;
    int blockNumber;
    int previousFileIndex;
    int previousRecordIndex;
    long lastControlWord;
} CbfParms;

typedef struct UnblockedBinaryParms {
    int placeHolder;
} UnblockedBinaryParms;

typedef struct AsciiParms {
    int placeHolder;
} AsciiParms;

typedef struct madrec {

    /* File Parameters */
    char *filnam;
    FILE *fp;
    int fd;
    int file_status;
    int iotype;							   /* file type - see madrecOpen in madrec.c */
    int fileSize;                          /* memory allocated to madrec->filep - size of in-memory file*/
    Int16 *filep;                          /* pointer to hold entire madrigal file in memory */
    int pos;                               /* present position in file - io or memory */
    int nrecords;						   /* number of records in filep */
    int currentRecord;                     /* number of record in filep presently loaded in recordp */
    CedarIndex *indexp;                    /* array of CedarIndex structures, one for each record */

    /* Parameters for the 5 file types */
    MadrigalParms madrigalParms;
    BlockedBinaryParms blockedBinaryParms;
    CbfParms cbfParms;
    UnblockedBinaryParms unblockedBinaryParms;
    AsciiParms asciiParms;    

    /* Record Parameters */
    Int16 *recordp;			    /* an array of Int16 that hold one entire record, prolog included */
    int recordpInMem;                       /* 1 if recordp is is pointing into memory block filep, 0 if pointing */
                                            /* to separate block on the heap; */
    double earliestStartTime;			    /* earliest start time found in file */
    double latestEndTime;					/* latest end time found in file     */

    /* Parameter Information */
    int numParms;                        	/* number of parameters in madrec struct */
    int *parmsListp;						/* list of parameters: [0] 10: year, [1] 11: month, [2] 12: day    */
									/* [3] 13: hour, [4] 14: minute, [5] 15: second, [6] 16: centisecond */
									/* [7] 34: uth, [8] 160: gdlat, [9] 170: glon, [10] 110: gdalt */
									/* Other parameters found in 1-D and 2-D records - note that 8-10 will */
									/* only exist if location in terms of az, el, and range */

    int *parmLocp;							/* indicates whether given parameter is 1-D (1) or 2-D (2) or 3 for 0-7 (time) */
                                            /* undefined for 8-10 */
    double *parmMinp;                       /* minimum value of parameter so far */
    double *parmMaxp;                       /* maximum value of parameter so far */
    int    *parmMissing;                    /* is parameter missing for some record in the file 0=no, 1=yes */
    
    int numKinst;                           /* number of kinst values found in file  */
    int kinstArr[MAX_KINST];                /* array of kinst values found in file   */
    int numKindat;                          /* number of kindat values found in file */
    int kindatArr[MAX_KINDAT];              /* array of kindat values found in file  */
    
    /* Miscellaneous Parameters */
    int pflag;
    char *lastError;
    int startJday0;                              /* holds Julian day number of earliest record in file */
    int *sortedRecnoList;                        /* an array of ints listing the chronological */
                                                 /* order of records in the file. Set to null when */
                                                 /* filed opened; malloc'ed by madrecGetSortedRecnoList */

} Madrec;

/* Method declarations */
Madrec *madrecCreate (void);
Madrec *madrecCreatex (int blockSize);
int madrecDestroy (Madrec *madrecp);
int madrecOpen (Madrec *madrecp, int iotype, char *filnam);
int madrecClose (Madrec *madrecp);
int madrecGetNextRec (Madrec *madrecp);
int madrecPutNextRec (Madrec *madrecp);
int madrecRewind (Madrec *madrecp);
int madrecGetPreviousRec (Madrec *madrecp);
int madrecGetRecByKey (Madrec *madrecp, double key);
int madrecGetRecByRecno (Madrec *madrecp, int recno);
int madrecGenKeys (Madrec *madrecp);
int madrecDeleteKeys (Madrec *madrecp);
int madrecPrintKeys (Madrec *madrecp);
int madrecCheckFile (Madrec *madrecp);
int madrecCopy (Madrec *madrec1p, Madrec *madrec2p);
int madrecSetError (Madrec *madrecp,const char *error);
char *madrecGetError (Madrec *madrecp);
double madrecGetMissing (Madrec *madrecp);
int madrecGetNumParms (Madrec *madrecp);
int *madrecGetParmsList (Madrec *madrecp);
int *madrecGetParmLoc (Madrec *madrecp);
double *madrecGetParmMin (Madrec *madrecp);
double *madrecGetParmMax (Madrec *madrecp);
int madrecGetFileType (Madrec *madrecp);
int madrecHasCatalog(Madrec *madrecp);
int madrecHasHeader(Madrec *madrecp);
int * madrecGetSortedRecnoList (Madrec *madrecp);
int compareCedarIndices(const void * index1, const void * index2);

#endif
