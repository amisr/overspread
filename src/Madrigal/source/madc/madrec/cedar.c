/*  $Id: cedar.c,v 1.54 2008/08/15 14:59:08 brideout Exp $ */

/*
modification history
--------------------
00a,26Apr96         original
*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <geometry.h>
#include <date.h>
#include <pthread.h>

/* global mutex to allow only one thread to load the */
/* following global data                             */
pthread_mutex_t cedar_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Parameter code parameters */
static int nparcodes = 0;
static int code[MAXPARCODES];
static char description[MAXPARCODES][40];
static char Int16Description[MAXPARCODES][12];
static double scaleFactor[MAXPARCODES];
static char units[MAXPARCODES][9];
static char mnemonic[MAXPARCODES][MNEM_LEN];
static char format[MAXPARCODES][9];
static int width[MAXPARCODES];
static int catId[MAXPARCODES];
static int hasDesc[MAXPARCODES];
static int hasErrDesc[MAXPARCODES];
static int cdi[MAXPARCODES]; /* lists parms with additional increments */
static int ipar = 0; /* number of parms with additional increments */
static double faci[MAXPARCODES]; /* scale factors relating main parm to additional increment parameter */

/* Instrument location parameters */
static int nkinst = 0;
static int kinstList[MAXINSTRUMENTS];
static char kinstName[MAXINSTRUMENTS][INST_LEN];
static double kinstLat[MAXINSTRUMENTS];
static double kinstLon[MAXINSTRUMENTS];
static double kinstAlt[MAXINSTRUMENTS];

/* Category parameters */
static int nCategories = 0;
static char catList[MAXCATEGORIES][CAT_LEN];
static int catMin[MAXCATEGORIES];
static int catMax[MAXCATEGORIES];

/* Exp table */
static int nExperiments = 0;
static int * expIdList;
static char ** expPathList;
static char ** expNameList;
static double * expStarttime;
static double * expEndtime;
static int * expKinstList;

/* File table */
static int nFiles = 0;
static char ** fileFileNameList;
static int * fileExpIDList;
static int * fileKindatList;
static int * fileCategoryList;
static int * filePermissionList;


static char *lastError = (char *)NULL;

static int pflag = 0;

/***********************************************************************
*
* cedarGetMadroot   copies Madroot path into user-supplied character
*                   buffer.
*
*   Simply calls getenv, if not found, uses #define __MAD_ROOT__
*
*   Returns  void
*/
void cedarGetMadroot(char * buf)
{ 
    if (getenv("MADROOT") == NULL) 
        strcpy(buf, __MAD_ROOT__);
    else
        strcpy(buf, getenv("MADROOT"));
}



/***********************************************************************
*
* cedarGetLtot   gets length of record
*
*/

int cedarGetLtot (Int16 *cedarp)
{
    return(cedarp[0]);
}


/***********************************************************************
*
* cedarGetKrec   gets Kind of record
*
*/

int cedarGetKrec (Int16 *cedarp)
{
    return(cedarp[1]);
}


/***********************************************************************
*
* isDataRecord   returns 1 if data record, 0 if catalog or header
*
*/

int isDataRecord(Int16 *cedarp)
{
    if (cedarp[1] == CATALOGBIN ||
        cedarp[1] == HEADERBIN ||
        cedarp[1] == CATALOGASCII ||
        cedarp[1] == HEADERASCII)
        return 0;
    else
        return 1;
}


/***********************************************************************
*
* cedarGetKinst   gets instrument code for these data
*
*/

int cedarGetKinst (Int16 *cedarp)
{
    return(cedarp[2]);
}


/***********************************************************************
*
* cedarGetKindat   gets kind-of-data code
*
*/

int cedarGetKindat (Int16 *cedarp)
{
    return(cedarp[3]);
}


/***********************************************************************
*
* cedarGetIbyr   gets beginning year
*
*/

int cedarGetIbyr (Int16 *cedarp)
{
    return(cedarp[4]);
}


/***********************************************************************
*
* cedarGetIbdt   gets beginning date (100*month+day)
*
*/

int cedarGetIbdt (Int16 *cedarp)
{
    return(cedarp[5]);
}


/***********************************************************************
*
* cedarGetIbhm   gets beginning hour and minute (100*hour + minute)
*
*/

int cedarGetIbhm (Int16 *cedarp)
{
    return(cedarp[6]);
}


/***********************************************************************
*
* cedarGetIbcs   gets beginning centisecond
*
*/

int cedarGetIbcs (Int16 *cedarp)
{
    return(cedarp[7]);
}


/***********************************************************************
*
* cedarGetIeyr   gets ending year
*
*/

int cedarGetIeyr (Int16 *cedarp)
{
    return(cedarp[8]);
}


/***********************************************************************
*
* cedarGetIedt   gets ending date (100*month+day)
*
*/

int cedarGetIedt (Int16 *cedarp)
{
    return(cedarp[9]);
}


/***********************************************************************
*
* cedarGetIehm   gets ending hour and minute (100*hour + minute)
*
*/

int cedarGetIehm (Int16 *cedarp)
{
    return(cedarp[10]);
}


/***********************************************************************
*
* cedarGetIecs   gets ending centisecond
*
*/

int cedarGetIecs (Int16 *cedarp)
{
    return(cedarp[11]);
}


/***********************************************************************
*
* cedarGetLprol   gets prolog length
*
*/

int cedarGetLprol (Int16 *cedarp)
{
    return(cedarp[12]);
}


/***********************************************************************
*
* cedarGetJpar   gets number of single-valued parameters
*
*/

int cedarGetJpar (Int16 *cedarp)
{
    return(cedarp[13]);
}


/***********************************************************************
*
* cedarGetMpar  gets number of multiple-valued parameters
*
*/

int cedarGetMpar (Int16 *cedarp)
{
    return(cedarp[14]);
}


/***********************************************************************
*
* cedarGetNrow   gets number of entries for each multiple-valued parameter
*
*/

int cedarGetNrow (Int16 *cedarp)
{
    return(cedarp[15]);
}


/***********************************************************************
*
* cedarGetKpar  gets number of derived parameters
*
*  Deprecated - use Maddata module for all derived data
*
*/

int cedarGetKpar (Int16 *cedarp)
{
    return(8);
}


/***********************************************************************
*
* cedarGetWord   gets specified word from cedar record
*
*/

int cedarGetWord (Int16 *cedarp, int word)
{
    if (word < cedarGetLtot(cedarp))
        return(cedarp[word-1]);
    else
        return(-1);
}


/***********************************************************************
*
* cedarGetStartTime   gets start time of record
*
*/

int cedarGetStartTime (Int16 *cedarp, int *year, int *month, int *day,
                   int *hour, int *minute, int *second, int *centisecond)
{
    *year = cedarp[4];
    *month = cedarp[5]/100;
    *day = cedarp[5] - 100*(*month);
    *hour = cedarp[6]/100;
    *minute = cedarp[6] - 100*(* hour);
    *second = cedarp[7]/100;
    *centisecond = cedarp[7] - 100*(*second);
    return(0);
}


/***********************************************************************
*
* cedarGetEndTime   gets end time of record
*
*/

int cedarGetEndTime (Int16 *cedarp, int *year, int *month, int *day,
                   int *hour, int *minute, int *second, int *centisecond)
{
    *year = cedarp[8];
    *month = cedarp[9]/100;
    *day = cedarp[9] - 100*(*month);
    *hour = cedarp[10]/100;
    *minute = cedarp[10] - 100*(* hour);
    *second = cedarp[11]/100;
    *centisecond = cedarp[11] - 100*(*second);
    return(0);
}


/***********************************************************************
*
* cedarGetStartJday   gets start Julian Day plus fractioanl day
*
*/

double cedarGetStartJday (Int16 *cedarp)
{
    int year=0, month=0, day=0, hour=0, minute=0, second=0;
    double rjday=0.0;

    year = cedarp[4];
    month = cedarp[5]/100;
    day = cedarp[5] - 100*month;
    hour = cedarp[6]/100;
    minute = cedarp[6] - 100*hour;
    second = cedarp[7]/100;
    /* centisecond = cedarp[7] - 100*second; */
    rjday = jday(day, month, year) + hour/24.0 + minute/1440.0 + second/86400.0;
    return(rjday);
}


/***********************************************************************
*
* cedarGetEndJday   gets end Julian Day plus fractioanl day
*
*/

double cedarGetEndJday (Int16 *cedarp)
{
    int year=0, month=0, day=0, hour=0, minute=0, second=0;
    double rjday=0.0;

    year = cedarp[8];
    month = cedarp[9]/100;
    day = cedarp[9] - 100*month;
    hour = cedarp[10]/100;
    minute = cedarp[10] - 100*hour;
    second = cedarp[11]/100;
    /* centisecond = cedarp[11] - 100*second; */
    rjday = jday(day, month, year) + hour/24.0 + minute/1440.0 + second/86400.0;
    return(rjday);
}


/***********************************************************************
*
* cedarGetStartIndex   gets start index time of record
*
*/

double cedarGetStartIndex (Int16 *cedarp)
{
    return(dmadptr((int)cedarp[4], (int)cedarp[5],
                   (int)cedarp[6], (int)cedarp[7]));
}


/***********************************************************************
*
* cedarGetEndIndex   gets end index time of record
*
*/

double cedarGetEndIndex (Int16 *cedarp)
{
    return(dmadptr((int)cedarp[8], (int)cedarp[9],
                   (int)cedarp[10], (int)cedarp[11]));
}


/***********************************************************************
*
* cedarGet1dParcodes  gets 1D parameter codes from a madrigal record
*
*  This methods allocates dynamic memory for the array of ints
*  returned.  The caller of this method is responsible for
*  calling free to release this memory when finished with it.
*
*  If no 1D parameter codes, returns NULL pointer.
*
*/

int * cedarGet1dParcodes(Int16 *cedarp)
{
    int i=0, l=0,lprol=0, jpar=0;
    int *parcodesp = (int *)NULL;

    lprol = cedarp[12];
    jpar = cedarp[13];

    /* check whether any 1D parameter codes exist */
    if (jpar > 0) {
        parcodesp = (int *)malloc(jpar*sizeof(int));
    
        for (i=0; i<jpar; i++) {
            l = lprol + i;
            parcodesp[i] = cedarp[l];
        }
    }
    return(parcodesp);
}


/***********************************************************************
*
* cedarGet2dParcodes  gets 2D parameter codes from a madrigal record
*
*  This methods allocates dynamic memory for the array of ints
*  returned.  The caller of this method is responsible for
*  calling free to release this memory when finished with it.
*
*  If no 2D parameter codes, returns NULL pointer.
*/

int * cedarGet2dParcodes(Int16 *cedarp)
{
    int i=0, l=0,lprol=0, jpar=0, mpar=0;
    int *parcodesp = (int *)NULL;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];

    /* check whether any 2D parameter codes exist */
    if (mpar > 0) {
        parcodesp = (int *)malloc(mpar*sizeof(int));
    
        for (i=0; i<mpar; i++) {
            l = lprol + 2*jpar + i;
            parcodesp[i] = cedarp[l];
        }
    }
    return(parcodesp);    
}



/***********************************************************************
*
* cedarHas1DParcode  returns 1 if cedarp has particular 1D parcode, 0 otherwise.
*
*/
int cedarHas1DParcode(Int16 *cedarp, int parcode)
{
    int i=0, lprol=0, jpar=0;

    lprol = cedarp[12];
    jpar = cedarp[13];

    
    for (i=0; i<jpar; i++) 
    {
        if (cedarp[lprol + i] == parcode)
            return (1);
    }
    return(0);
}


/***********************************************************************
*
* cedarHas2DParcode  returns 1 if cedarp has particular 2D parcode, 0 otherwise.
*
*/
int cedarHas2DParcode(Int16 *cedarp, int parcode)
{
    int i=0, lprol=0, jpar=0, mpar=0;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];

    
    for (i=0; i<mpar; i++) 
    {
        if (cedarp[lprol + 2*jpar +i] == parcode)
            return (1);
    }
    return(0);
}


/***********************************************************************
*
* cedarGet1dParm   gets a scaled 1D parameter from a madrigal record
*
*  If 1D parm does not exist, returns double "missing"
*  If 1D parm = -32767 (missing), returns double "missing"
*  If 1D parm is an error code, and = -32766 (assumed), returns double "assumed"
*  If 1D parm is an error code, and = +32767 (known bad), returns double "knownbad"
*
*  Otherwise, scales value and includes additional increment values 
*  if they exist
*
*  If cedarp is a header or catalog record; warning is printed to std err
*  and returns "missing"
*
*/

double cedarGet1dParm(Int16 *cedarp, int parcode)
{
    int i=0, j=0, l=0,lprol=0, jpar=0, krec=0;
    double scale=0.0;
    double value=0.0;
    int incrValue = 0;
                                 
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    krec = cedarp[1];

    if (krec == CATALOGBIN || krec == HEADERBIN) {
        fprintf(stderr, "Warning: cedarGet1dParm called for non-data record!\n");
        return (missing);
    }

    lprol = cedarp[12];
    jpar = cedarp[13];

    for (i=0; i<jpar; i++) {
        l = lprol + i;
        if (cedarp[l] == parcode) {

            /* check whether special values found */
            if (cedarp[l+jpar] == missingData)
                return(missing);
            if (cedarp[l+jpar] == assumedData && parcode < 0)
                return(assumed);
            if (cedarp[l+jpar] == knownBadData && parcode < 0)
                return(knownbad);

            scale = 1.0;
            for (j=0; j<nparcodes; j++) {
                if (code[j] == abs(cedarp[l])) {
                    scale = scaleFactor[j];
                    break;
                }
            }
            value = scale*(double)cedarp[l+jpar];

            /* now add addition incr if exists */
            for (j=0; j<ipar; j++) {
                if (cdi[j] == abs(parcode)) {
                    /* handle regular code versus (negitive) error code */
                    if (parcode > 0)
                        incrValue = cedarGet1dInt(cedarp, parcode+1);
                    else
                        incrValue = cedarGet1dInt(cedarp, parcode-1);
                    if (incrValue != missingData)
                        value += (double)incrValue*faci[j];
                }
            }

            return (value);
        }
    }
    return(missing);
}


/***********************************************************************
*
* cedarGet2dParm   gets a scaled 2D parameter from a madrigal record
*
*  If 2D parm does not exist, returns array of double "missing"
*  If 2D parm = -32767 (missing), returns double "missing"
*  If 2D parm is an error code, and = -32766 (assumed), returns double "assumed"
*  If 2D parm is an error code, and = +32767 (known bad), returns double "knownbad"
*
*  Otherwise, scales value and includes additional increment values 
*  if they exist
*
*  This methods allocates dynamic memory for the array of doubles
*  returned.  The caller of this method is responsible for
*  calling free to release this memory when finished with it.
*
*  If nrow = 0, returns NULL pointer. 
*
*  If parcode not found, returns array of missing
*
*  If cedarp is a header or catalog record; warning is printed to std err
*  and returns NULL
*/

double * cedarGet2dParm(Int16 *cedarp, int parcode)
{
    int i=0, j=0, k=0, l=0, lprol=0, jpar=0, krec=0,
        mpar=0, nrow=0, l2j=0;
    int incrValue = 0;
    Int16 * intArr;
    int isSpecial = 0;   /* used to flag a special value, no scaling */
    double scale=0.0;
    double *parp = (double *)NULL;
                                    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    krec = cedarp[1];

    if (krec == CATALOGBIN || krec == HEADERBIN) {
        fprintf(stderr, "Warning: cedarGet2dParm called for non-data record!\n");
        return ((double *) NULL);
    }

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];
    l2j =  lprol + 2*jpar;   

    if (nrow == 0) return ((double *) NULL);

    parp = (double *)malloc(nrow*sizeof(double));

    for (i=0; i<mpar; i++) {
        l = l2j + i;
        if (cedarp[l] == parcode) {

            scale = 1.0;
            for (j=0; j<nparcodes; j++) {
                if (code[j] == abs(cedarp[l])) {
                    scale = scaleFactor[j];
                    break;
                }
            }

            /* Convert parameter to double precision */
            for (k=0; k<nrow; k++) {
                l = l2j + (k+1)*mpar + i;
                parp[k] = scale*(double)cedarp[l];

                /* now add addition incr if exists */
                for (j=0; j<ipar; j++) {
                    if (cdi[j] == abs(parcode)) {
                        /* handle regular code versus (negitive) error code */
                        if (parcode > 0) {
                            intArr = cedarGet2dInt(cedarp, parcode+1);
                            incrValue = intArr[k];
                            if (incrValue != missingData)
                                parp[k] += (double)incrValue*faci[j];
                            free(intArr);
                        }
                        else {
                            /* handle error codes */
                            intArr = cedarGet2dInt(cedarp, parcode-1);
                            incrValue = intArr[k];
                            if (incrValue != missingData && incrValue != assumedData &&  incrValue != knownBadData)
                                parp[k] += (double)incrValue*faci[j];
                            free(intArr);
                        }
                    }
                }

                /* Check for special values */
                isSpecial = 0;
                if (parcode > 0) {
                    if (cedarp[l] == missingData) {
                        isSpecial = 1;
                        parp[k] = missing;
                    }
                } else if (parcode < 0) {
                    /* handle special cases of error parameters */
                    /* missing */
                    if (cedarp[l] == missingData) {
                        isSpecial = 1;
                        parp[k] = missing;
                    }
                    /* assumed */
                    else if (cedarp[l] == assumedData) {
                        isSpecial = 1;
                        parp[k] = assumed;
                    }
                    /* known bad */
                    else if (cedarp[l] == knownBadData) {
                        isSpecial = 1;
                        parp[k] = knownbad;
                    }
                }
            }
            return(parp);
        }
    }
    /* parcode not found in record */
    for (k=0; k<nrow; k++) {
        parp[k] = missing;
    }
    return(parp);
}



/***********************************************************************
*
* cedarGet2dParmValue   gets a single scaled 2D parameter from a madrigal record
*
*  If 2D parm does not exist, returns double "missing"
*  If 2D parm = -32767 (missing), returns double "missing"
*  If 2D parm is an error code, and = -32766 (assumed), returns double "assumed"
*  If 2D parm is an error code, and = +32767 (known bad), returns double "knownbad"
*  If row is greater than number of 2d rows, returns double "missing"
*
*  Otherwise, scales value and includes additional increment values 
*  if they exist
*
*  This method differs from cedarGet2dParm in that it only returns a 
*  single double from a single row, so no malloc/free is required.
*
*
*  If cedarp is a header or catalog record; warning is printed to std err
*  and returns missing
*/

double cedarGet2dParmValue(Int16 *cedarp, int parcode, int row)
{
    int i=0, j=0, l=0, lprol=0, jpar=0, krec=0,
        mpar=0, nrow=0, l2j=0;
    Int16 incrValue = 0;
    int isSpecial = 0;   /* used to flag a special value, no scaling */
    double scale=0.0;
    double parp = missing; 
                                    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    krec = cedarp[1];

    if (krec == CATALOGBIN || krec == HEADERBIN) {
        fprintf(stderr, "Warning: cedarGet2dParm called for non-data record!\n");
        return (missing);
    }

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];
    l2j =  lprol + 2*jpar;   

    if (nrow == 0) return (missing);

    for (i=0; i<mpar; i++) {
        l = l2j + i;
        if (cedarp[l] == parcode)
        {
            scale = 1.0;
            for (j=0; j<nparcodes; j++) 
            {
                if (code[j] == abs(cedarp[l])) 
                {
                    scale = scaleFactor[j];
                    break;
                }
            }

            /* Convert parameter to double precision */
            l = l2j + (row+1)*mpar + i;
            parp = scale*(double)cedarp[l];

            /* now add addition incr if exists */
            for (j=0; j<ipar; j++) 
            {
                if (cdi[j] == abs(parcode)) 
                {
                    /* handle regular code versus (negitive) error code */
                    if (parcode > 0) 
                    {
                        incrValue = cedarGet2dIntValue(cedarp, parcode+1, row);
                        if (incrValue != missingData)
                            parp += (double)incrValue*faci[j];
                    }
                    else 
                    {
                        /* handle error codes */
                        incrValue = cedarGet2dIntValue(cedarp, parcode-1, row);
                        if (incrValue != missingData && incrValue != assumedData &&  incrValue != knownBadData)
                            parp += (double)incrValue*faci[j];
                    }
                }
            }

            /* Check for special values */
            isSpecial = 0;
            if (parcode > 0) 
            {
                if (cedarp[l] == missingData) 
                {
                    isSpecial = 1;
                    parp = missing;
                }
            } else if (parcode < 0) 
            {
                /* handle special cases of error parameters */
                /* missing */
                if (cedarp[l] == missingData) 
                {
                    isSpecial = 1;
                    parp = missing;
                }
                /* assumed */
                else if (cedarp[l] == assumedData) 
                {
                    isSpecial = 1;
                    parp = assumed;
                }
                /* known bad */
                else if (cedarp[l] == knownBadData) 
                {
                    isSpecial = 1;
                    parp = knownbad;
                }
            }
            return(parp);
        } /* next 2D parm code */
    }

    /* parcode not found in record */
    return(missing);
}


/***********************************************************************
*
* cedarGetFlatParm   creates a flattened 2D parameter 
*
*  If 1D parmeter exists, copies array of double of length nrow with
*  every value set to the 1D value.  If not, uses cedarGet2dParm.  Note
*  cedarGet2dParm returns all "missing" is parm not found. 
*
*  This methods allocates dynamic memory for the array of doubles
*  returned.  The caller of this method is responsible for
*  calling free to release this memory when finished with it.
*
*  If nrow = 0, returns NULL pointer. 
*
*  If parcode not found, returns array of missing
*/

double * cedarGetFlatParm(Int16 *cedarp, int parcode)
{
    int nrow, i;
    double * result2d;
    double val1d = 0.0;
    double * parmArr;

    nrow = cedarGetNrow(cedarp);

    if (nrow == 0) return ((double *) NULL);

    parmArr = (double *)malloc(nrow*sizeof(double));

    val1d = cedarGet1dParm(cedarp, parcode);
    if (val1d != missing){
        /* copy array of 1d value */
        for (i=0; i<nrow; i++)
            parmArr[i] = val1d;
    } 
    else {
        /* copy array of 2d values */
        result2d = cedarGet2dParm(cedarp, parcode);
        for (i=0; i<nrow; i++)
            parmArr[i] = result2d[i];
        free (result2d);
    }

    return(parmArr);
}



/***********************************************************************
*
* hasData   determines whether any non-missing data in a double array
*
*  Returns 0 if all data in 2d array of length nrow is missing, 1 
*  otherwise.
*/

int hasData(int nrow, double * parp)
{
    int i;

    for (i=0; i < nrow; i++) {
        if (parp[i] != missing)
            return (1);
    }

    return (0);
}


/***********************************************************************
*
* cedarGetParmCodeArray   gets parameter codes of all parameters 
*                         in specp->pparms which are actually available
*                         from the current record.
*
*  User is responsible for calling free to release the returned
*  array of ints when finished with them.
*
*  Deprecated - use Maddata module instead
*/

int * cedarGetParmCodeArray(Int16 *cedarp, Ffspec *specp, int *nlines) {
    int i=0, j=0, k=0, l=0;
    int lprol=0, jpar=0, mpar=0, l2j=0;
    static const int kpar=8, cd1[8] = {10,11,12,13,14,15,16,34}; 
    static const int lpar=3, cd2[3] = {110,160,170};
    int *codep = (int *)NULL;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];

    codep = (int *)malloc((jpar+mpar+kpar)*sizeof(int));

    k = 0;

    /* 1D parameters */
    for (i=0; i<jpar; i++) {
        l = lprol + i;
        for (j=0; j<specp->nparms; j++) {
            if (cedarp[l] == specp->pparms[j]) {
                codep[k++] = cedarp[l];
                break;
            }
        }
    }

    /* 2D parameters */
    l2j =  lprol + 2*jpar;
    for (i=0; i<mpar; i++) {
        l = l2j + i;
        for (j=0; j<specp->nparms; j++) {
            if (cedarp[l] == specp->pparms[j]) {
                codep[k++] = cedarp[l];
                break;
            }
        }
    }

    /* Derived 1D Parameters */
    for (i=0; i<kpar; i++) {
        for (j=0; j<specp->nparms; j++) {
            if (cd1[i] == specp->pparms[j]) {
                codep[k++] = cd1[i];
                break;
            }
        }
    }

    /* Derived 2D Parameters */
    for (i=0; i<lpar; i++) {
        for (j=0; j<specp->nparms; j++) {
            if (cd2[i] == specp->pparms[j]) {
                codep[k++] = cd2[i];
                break;
            }
        }
    }

    *nlines = k;

    return(codep);
}


/***********************************************************************
*
* cedarGetParmArray   flattens a subset of a CEDAR file
*
*  If record is rejected, nlinesp will be set to 0; returned
*     double array will be set to random values.
*
*  User is responsible for calling free to release the returned
*  array of doubles when finished with them.
*
*  Deprecated - use Maddata module
*/

double * cedarGetParmArray(Int16 *cedarp, Ffspec *specp, int *nlinesp) 
{
    int i=0, j=0, k=0, l=0, li=0, real_li=0, m=0, j1=0, j2=0, parcode=0, 
        foundit=0, lprol=0, jpar=0, mpar=0, nrow=0, l2j=0,year=0,
        month=0, day=0, hour=0, minute=0, second=0, centisecond=0;
    double dval[8];

    double scale=0.0, scalei=0.0, parm=0.0;
    double startJday=0.0, ut1=0.0, ut2=0.0, uth=0.0;
    int startJday0=0;

    /* 1d derived parameters */
    static const int kpar=8, cd1[8] = {10,11,12,13,14,15,16,34}; 

    /* 2d derived parameters */
    static int lpar=3, cd2[3] = {110,160,170};
    double *gdlatp, *glonp, *gdaltp;

    /* an array of 2D row reject flags */
    int *qrowp = (int *)NULL;

    /* the array of doubles to be returned */
    static double *parp = (double *)NULL;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    /* Allocate enough memory for flattened array */
    parp = (double *)malloc(nrow*specp->nparms*sizeof(double));

    /* Allocate memory for row reject flag */
    qrowp = (int *)malloc(nrow*sizeof(int));
    for (i=0; i<nrow; i++) {
        qrowp[i] = 1;
    }

    /* Initialize index over output parameters */
    k = 0;

    /* Apply filters. Do the 1d filters first because they reject an entire
       record, whereas 2d filters reject a single row of the 2d array. */

    /* Apply 1d derived parameter filters */
    (void) cedarGetStartTime (cedarp, &year, &month, &day,
                              &hour, &minute, &second, &centisecond);
    dval[0] = year;
    dval[1] = month;
    dval[2] = day;
    dval[3] = hour;
    dval[4] = minute;
    dval[5] = second;
    dval[6] = centisecond;
    startJday = cedarGetStartJday(cedarp);
    startJday0 = (int)startJday;
    ut1 = 24.0*(cedarGetStartJday(cedarp) - startJday0);
    ut2 = 24.0*(cedarGetEndJday(cedarp) - startJday0);
    uth = 0.5*(ut1 + ut2);
    dval[7] = uth;
    for (i=0; i<kpar; i++) {    /* for each 1d derived parameter */
        for (m=0; m<specp->nfilters; m++) {    /* for each filter parameter */
            if (cd1[i] == specp->fparms[m]) {      /* Check if in 1d array */
                parm = dval[i];
                /* Reject this record if parameter does not pass filter */
                if (parm < specp->fmin[m] || parm > specp->fmax[m]) {                     
                    *nlinesp = 0;
                    free(qrowp);
                    return (parp);
                }
            }
        }
    }

    /* Apply 1D parameter filters */
    for (i=0; i<jpar; i++) {    /* for each 1d parameter */
        l = lprol + i;          /* locate it in 1d array */
        for (m=0; m<specp->nfilters; m++) {    /* for each filter parameter */
            if (cedarp[l] == specp->fparms[m]) {   /* check if in 1d array  */
                parcode = cedarp[l];
                scale = cedarGetParScaleFactor(parcode);
                parm = scale*cedarp[l+jpar];    /* Apply the scale factor */
                /* Reject this record if parameter does not pass filter */
                if (parm < specp->fmin[m] || parm > specp->fmax[m]) {
                    *nlinesp = 0;
                    free(qrowp);
                    return (parp);
                }
            }
        }
    }

    /* Apply 2D parameter filters */
    l2j =  lprol + 2*jpar;
    for (i=0; i<mpar; i++) {    /* for each 2d parameter */
        l = l2j + i;            /* locate it in 2d array */
        for (m=0; m<specp->nfilters; m++) {    /* for each filter parameter */
           if (cedarp[l] == specp->fparms[m]) {    /* check if in 2d array  */
                parcode = cedarp[l];
                scale = cedarGetParScaleFactor(parcode);
                for (j=0; j<nrow; j++) {    /* for each row */
                    l = l2j + (j+1)*mpar + i;    /* scale the parameter */
                    if (cedarp[l] <= -32766 || cedarp[l] >= 32767) {
                        parm = missing;
                    } else {
                        parm = scale*cedarp[l];
                    }
                    /* Reject this row if parameter does not pass filter */
                    if (parm < specp->fmin[m] || parm > specp->fmax[m]) {
                        qrowp[j] = 0;
                    }
                 }
                 break;
            }
        }
    }

    /* Apply 2D derived parameters filters */
    for (i=0; i<lpar; i++) {    /* for each 2d derived parameter */
        for (m=0; m<specp->nfilters; m++) {    /* for each filter parameter */
	    if (cd2[i] == specp->fparms[m]) {
	        (void) cedarGetGeodetic(cedarp, &gdlatp, &glonp, &gdaltp);
	        for (j=0; j<nrow; j++) {    /* for each row */
		    if (specp->fparms[m] == 160)
		        parm = gdlatp[j];
		    else if (specp->fparms[m] == 170)
		        parm = glonp[j];
		    else if (specp->fparms[m] == 110)
			parm = gdaltp[j];
                    /* Reject this row if parameter does not pass filter */
                    if (parm < specp->fmin[m] || parm > specp->fmax[m]) {
                        qrowp[j] = 0;
                    }
                }
                free (gdlatp);
                free (glonp);
                free (gdaltp);
	        break;
	    }
	}
    }

    *nlinesp = 0;
    for (j=0; j<nrow; j++) {
        if (qrowp[j] == 1) (*nlinesp)++;
    }

    for (m=0; m<specp->nparms; m++) {    /* for each output parameter */

        /* Add 1d derived parameters to output array */
        foundit = 0;
        for (i=0; i<kpar; i++) {
            if (cd1[i] == specp->pparms[m]) {
                for (j=0; j<nrow; j++) {
                    if (qrowp[j] == 1) {    /* add parameter */
                        parp[k++] = dval[i];
                    }
                }
                foundit = 1;
                break;
            }
        }
        if (foundit == 1)
            continue;

        /* Add 1D parameters to the output array */
        foundit = 0;
        for (i=0; i<jpar; i++) {        /* for each 1d parameter */
            l = lprol + i;              /* locate it in 1d array */
            if (cedarp[l] == specp->pparms[m]) {    /* check if in 1d array  */
                scale = cedarGetParScaleFactor(specp->pparms[m]);
                scalei = 0.0;    /* Check for increment */
                for (j1=0; j1<ipar; j1++) {
                    if (cdi[j1] == specp->pparms[m]) {
                        for (j2=0; j2<jpar; j2++) {
                            li = lprol + j2;
                            if (cedarp[li] == specp->pparms[m]+1) {
                                scalei = faci[j1];
                                real_li = li;
                            }
                        }
                        break;
                    }
                }
                for (j=0; j<nrow; j++) {    /* for each row */
                    if (qrowp[j] == 1) {    /* add parameter */
                        parp[k] = scale*cedarp[l+jpar];
                        if (scalei > 0.0) {
                            parp[k] = parp[k] + scalei*cedarp[real_li+jpar];
                        }
                        k++;
                    }
                }
                foundit = 1;
                break;
            }
        }
        if (foundit == 1)
            continue;

        /* Add 2D parameters to the output array */
        l2j =  lprol + 2*jpar;
        foundit = 0;
        for (i=0; i<mpar; i++) {    /* for each 2d parameter */
            l = l2j + i;            /* locate it in 1d array */
            if (cedarp[l] == specp->pparms[m]) {    /* check if in 2d array  */
                scale = cedarGetParScaleFactor(specp->pparms[m]);
                scalei = 0.0;    /* Check for increment */
                for (j1=0; j1<ipar; j1++) {
                    if (cdi[j1] == specp->pparms[m]) {
                        for (j2=0; j2<mpar; j2++) {
                            li = lprol + 2*jpar + j2;
                            if (cedarp[li] == specp->pparms[m]+1) {
                                scalei = faci[j1];
                                real_li = li;
                            }
                        }
                        break;
                    }
                }
                for (j=0; j<nrow; j++) {    /* for each row */
                    if (qrowp[j] == 1) {    /* add parameter */
                        l = l2j + (j+1)*mpar + i;
                        if (cedarp[l] <= -32766 || cedarp[l] >= 32767) {
                            parp[k] = missing;
                        } else {
                            parp[k] = scale*cedarp[l];
                            if (scalei > 0.0) {
                                if (cedarp[real_li+(j+1)*mpar] > -32766 && cedarp[real_li+(j+1)*mpar] < 32767) {
                                	parp[k] = parp[k] + scalei*cedarp[real_li+(j+1)*mpar];
                                }
                            }
                        }
                        k++;
                    }
                }
                foundit = 1;
                break;
            }
        }
        if (foundit == 1)
            continue;

        /* Add 2D derived parameters to the output array */
        foundit = 0;
        for (i=0; i<lpar; i++) {
            if (cd2[i] == specp->pparms[m]) {
                (void) cedarGetGeodetic(cedarp, &gdlatp, &glonp, &gdaltp);
                for (j=0; j<nrow; j++) {    /* for each row */
                    if (qrowp[j] == 1) {    /* add parameter */
                        if (specp->pparms[m] == 160)
                            parp[k++] = gdlatp[j];
                        else if (specp->pparms[m] == 170)
                            parp[k++] = glonp[j];
                        else if (specp->pparms[m] == 110)
                            parp[k++] = gdaltp[j];
                    }
                }
                free (gdlatp);
                free (glonp);
                free (gdaltp);
                foundit = 1;
                break;   
            }
        }
        if (foundit == 1)
            continue;

    }

    free(qrowp);
    return(parp);
}


/***********************************************************************
*
* cedarGetGeodetic   gets geodetic coordinates from radar coordinates
* 
*  cedarGetGeodetic modifies the three arrays of doubles to return
*  lat, long, and alt.  Length of each array is nrows.  Geodetic
*  coordinates will be calculated in any of the following ways:
*
*     1) az, el, and range - az from azm, az1, or az2, and
*        el from elm, el, or el2
*     2) (altb, alte, or gdalt), gdlat and glon 
*     3) (altb, altav or altb, alte, or gdalt) alone - lat and long assumed
*        to be that of instrument
*     
*     If all three methods fail, all three arrays populated with missing
*
*     All parameters can be either 1d or 2d.   
*      
*  This methods allocates dynamic memory for the array of doubles
*  modified.  The caller of this method is responsible for
*  calling free to release the memory from these 3 arrays when 
*  finished with them.
*
*  If nrow = 0, returns -1. 
*/

int cedarGetGeodetic(Int16 *cedarp, double **gdlatpp, double **glonpp, double **gdaltpp)
{
    int i=0, kinst=0, nrow=0;

    /* station location */
    double slat = 0.0,
           slon = 0.0,
           salt = 0.0;
 
    int noData = 0; /* flag to indicate no data found */

    /* temp arrays of data - freed internally when done */
    double *azm   =(double *)NULL,
           *az1   =(double *)NULL,
           *az2   =(double *)NULL,
           *azd   =(double *)NULL,
           *elm   =(double *)NULL,
           *el1   =(double *)NULL,
           *el2   =(double *)NULL,
           *range =(double *)NULL,
           *altb  =(double *)NULL,
           *altav =(double *)NULL,
           *alte  =(double *)NULL,
           *gdalt =(double *)NULL,
           *gdlat =(double *)NULL,
           *glon  =(double *)NULL;


    /* arrays to be freed by user */
    double *latp = (double *)NULL;
    double *lonp = (double *)NULL;
    double *altp = (double *)NULL;
    

    nrow = cedarGetNrow(cedarp);
    kinst = cedarGetKinst(cedarp);

    if (nrow == 0) return (-1);

    /* from kinst, get station coordinates */
    (void) los2geodetic(kinst, 0.0, 0.0, 0.0, &slat, &slon, &salt);

    /* allocate arrays */
    latp = (double *)malloc(nrow*sizeof(double));
    lonp = (double *)malloc(nrow*sizeof(double));
    altp = (double *)malloc(nrow*sizeof(double));

    azd   = (double *)malloc(nrow*sizeof(double));


    /* Populate each  temp array */
    azm = cedarGetFlatParm(cedarp, 130);
    az1 = cedarGetFlatParm(cedarp, 132);
    az2 = cedarGetFlatParm(cedarp, 133);
    elm = cedarGetFlatParm(cedarp, 140);
    el1 = cedarGetFlatParm(cedarp, 142);
    el2 = cedarGetFlatParm(cedarp, 143);
    range = cedarGetFlatParm(cedarp, 120);    
    altb = cedarGetFlatParm(cedarp, 106);
    altav = cedarGetFlatParm(cedarp, 115);
    alte = cedarGetFlatParm(cedarp, 108);
    gdalt = cedarGetFlatParm(cedarp, 110);
    gdlat = cedarGetFlatParm(cedarp, 160);
    glon = cedarGetFlatParm(cedarp, 170);

    /* Compute mean azimuth - always check for missing data even if some data exists */
    if (!hasData(nrow, azm)) {
        if (hasData(nrow, az1) && hasData(nrow, az2)) {
	    
            for (i=0; i<nrow; i++) {
                if (az1[i] != missing) {
                    while (az1[i] < -180.0) az1[i]=az1[i]+360.0;
                    while (az1[i] > +180.0) az1[i]=az1[i]-360.0;
                }
                if (az2[i] != missing) {
                    while (az2[i] < -180.0) az2[i]=az2[i]+360.0;
                    while (az2[i] > +180.0) az2[i]=az2[i]-360.0;
                }
                if (az1[i] != missing && az2[i] != missing) {
                    azd[i] = az2[i] - az1[i];
                    while (azd[i] < -180.0) azd[i]=azd[i]+360.0;
                    while (azd[i] > +180.0) azd[i]=azd[i]-360.0;
                    azm[i] = (az1[i] + azd[i]/2.0);
                }
                else if (az1[i] == missing && az2[i] != missing)
                    azm[i] = az2[i];
                else if (az1[i] != missing && az2[i] == missing)
                    azm[i] = az1[i];
                else
                    azm[i] = missing;
            }
        } else if (hasData(nrow, az1)) {
            for (i=0; i<nrow; i++)
                azm[i] = az1[i];
        } else if (hasData(nrow, az2)) {
            for (i=0; i<nrow; i++)
                azm[i] = az2[i];
        }   
    }


    /* Compute mean elevation  - always check for missing data even if some data exists */
    if (!hasData(nrow, elm)) {
        for (i=0; i<nrow; i++) {
            if (el1[i] != missing && el2[i] != missing) {
                elm[i] = (el1[i] + el2[i])/2.0;
            } else if (el1[i] != missing) {
                elm[i] = el1[i];
            } else if (el2[i] != missing) {
                elm[i] = el2[i];
            }
        } 
    }

  
    if (hasData(nrow, azm) && hasData(nrow, elm) && hasData(nrow, range)) {
        for (i=0; i<nrow; i++) {
            if (azm[i] != missing && elm[i] != missing && range[i] != missing) {
                (void) los2geodetic(kinst, azm[i], elm[i], range[i],
                                    &latp[i], &lonp[i], &altp[i]);
            } else {
                latp[i] = missing;
                lonp[i] = missing;
                altp[i] = missing;
            }
	}
    }
	    
    else {
        /* try to set alt, lat, and long directly */
        /* if only alt found, set lat and long to be the instruments */
        /* if no alt found, set all to missing */

        /* first alt */
        if (hasData(nrow, gdalt)) {
            for (i=0; i<nrow; i++) {
                altp[i] = gdalt[i];
            }
        }
        else if (hasData(nrow, altb) && hasData(nrow, altav)) {
            for (i=0; i<nrow; i++) 
            {
                if (altb[i] != missing && altav[i] != missing)
                    altp[i] = altb[i] + altav[i]*nrow;
                else
                    altp[i] = missing;
            }
        }
        else if (hasData(nrow, altb) && hasData(nrow, alte)) {
            for (i=0; i<nrow; i++) {
                if (altb[i] != missing && alte[i] != missing)
                    altp[i] = (altb[i] + alte[i])/2;
                else if (altb[i] != missing)
                    altp[i] = altb[i];
                else if (alte[i] != missing)
                    altp[i] = alte[i];
                else
                    altp[i] = missing;
            }
        }
        else if (hasData(nrow, altb)) {
            for (i=0; i<nrow; i++) {
                if (altb[i] != missing)
                	altp[i] = altb[i];
                else
                    altp[i] = missing;
            }
        }    
        else if (hasData(nrow, alte)) {
            for (i=0; i<nrow; i++) {
                if (alte[i] != missing)
                	altp[i] = alte[i];
                else
                    altp[i] = missing;
            }
        }
        else {
            /* no alt data - set everything to missing */
            noData = 1;
            for (i=0; i<nrow; i++) {
                latp[i] = missing;
                lonp[i] = missing;
                altp[i] = missing;
            }
        }

        /* next lat */
        if (hasData(nrow, gdlat)) {
            for (i=0; i<nrow; i++) {
                latp[i] = gdlat[i];
            }
        }
        else {
            /* no lat found, set using kinst if alt found */
            for (i=0; i<nrow; i++) {
                if (altp[i] != missing)
                    latp[i] = slat;
                else
                    latp[i] = missing;
            }
        }

        /* finally lon */
        if (hasData(nrow, glon)) {
            for (i=0; i<nrow; i++) {
                lonp[i] = glon[i];
            }
        }
        else {
            /* no lon found, set using kinst if alt found */
            for (i=0; i<nrow; i++) {
                if (altp[i] != missing)
                    lonp[i] = slon;
                else
                    lonp[i] = missing;
            }
        }
    }

    /* free all temp data */
    free(azm);
    free(az1);
    free(az2);
    free(azd);
    free(elm);
    free(el1);
    free(el2);
    free(range);
    free(altb);
    free(altav);
    free(alte);
    free(gdalt);
    free(gdlat);
    free(glon);


    *gdlatpp = latp;
    *glonpp = lonp;
    *gdaltpp = altp;

    return(0);
}


/***********************************************************************
*
* cedarGet1dInt   gets a 1D parameter (unscaled) from a madrigal record
*
*/

Int16 cedarGet1dInt(Int16 *cedarp, int parcode)
{
    int i=0, l=0, lprol=0, jpar=0;

    lprol = cedarp[12];
    jpar = cedarp[13];
 
    for (i=0; i<jpar; i++) {
        l = lprol + i;
        if (cedarp[l] == parcode) {
            return(cedarp[l+jpar]);
        }
    }
    return(missingData);
}


/***********************************************************************
*
* cedarGet2dInt   gets a 2D parameter (unscaled) from a madrigal record
*
*  This method allocates dynamic memory for the array of ints
*  returned.  The caller of this method is responsible for
*  calling free to release this memory when finished with it.
*
*  If nrow = 0, returns NULL pointer. 
*
*  If parcode not found, returns array of missingData
*/

Int16 * cedarGet2dInt(Int16 *cedarp, int parcode)
{
    int i=0, k=0, l=0, lprol=0, jpar=0, mpar=0, nrow=0;
    Int16 *parp = (Int16 *)NULL;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (nrow == 0) return ((Int16 *) NULL);

    parp = (Int16 *)malloc(nrow*sizeof(Int16));
   
    for (i=0; i<mpar; i++) {
        l = lprol + 2*jpar + i;
        if (cedarp[l] == parcode) {
            for (k=0; k<nrow; k++) {
                l = lprol + 2*jpar + (k+1)*mpar + i;
                parp[k] = cedarp[l];
            }
            return(parp);
        }
    }
    /* parcode not found */
    for (k=0; k<nrow; k++) {
        parp[k] = missingData;
    }
    return(parp);
}



/***********************************************************************
*
* cedarGet2dIntValue   gets a single 2D parameter (unscaled) from a madrigal record
*
*  This method differs from cedarGet2dInt in that it only returns
*  a single unscaled Int16 from a particular row.
*
*  If nrow = 0,  or row > number of 2d rows, returns missingData. 
*
*  If parcode not found, returns missingData
*/

Int16 cedarGet2dIntValue(Int16 *cedarp, int parcode, int row)
{
    int i=0, l=0, lprol=0, jpar=0, mpar=0, nrow=0;
    Int16 parp = missingData;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (nrow == 0) return (missingData);
   
    for (i=0; i<mpar; i++) 
    {
        l = lprol + 2*jpar + i;
        if (cedarp[l] == parcode) 
        {
            l = lprol + 2*jpar + (row+1)*mpar + i;
            parp = cedarp[l];
            return(parp);
        }
    }
    /* parcode not found */
    return(missingData);
}


/***********************************************************************
*
* cedarCreateRecord  creates a new Cedar record   
*
*   User is responsible for freeing dynamically allocated array
*   when finished with it.
*/

Int16 *cedarCreateRecord(int lprol, int jpar, int mpar, int nrow,
                         int krec, int kinst, int kindat,
                         int year1, int month1, int day1,
                         int hour1, int minute1, int second1, int centisecond1,
                         int year2, int month2, int day2,
                         int hour2, int minute2, int second2, int centisecond2)
{
    Int16 *cedarp;
    int ltot=0, i=0;

    ltot = lprol + 2*jpar + mpar*(nrow+1);
    cedarp = (Int16 *) malloc(ltot*sizeof(Int16));
    cedarp[0] = (Int16)ltot;
    cedarp[1] = krec;
    cedarp[2] = kinst;
    cedarp[3] = kindat;
    cedarp[4] = year1;
    cedarp[5] = 100*month1 + day1;
    cedarp[6] = 100*hour1 + minute1;
    cedarp[7] = 100*second1 + centisecond1;
    cedarp[8] = year2;
    cedarp[9] = 100*month2 + day2;
    cedarp[10] = 100*hour2 + minute2;
    cedarp[11] = 100*second2 + centisecond2;
    cedarp[12] = (Int16)lprol;
    cedarp[13] = (Int16)jpar;
    cedarp[14] = (Int16)mpar;
    cedarp[15] = (Int16)nrow;
    for (i=lprol; i<ltot; i++) {
        cedarp[i] = 0;
    }
/*
printf("%6d %6d %6d %6d %6d %6d \n", cedarp[0],cedarp[1],cedarp[2],cedarp[3],cedarp[4],cedarp[5]);
*/
    return(cedarp);
}


/***********************************************************************
*
* cedarCreateCatalogRecord  creates a new Cedar Catalog record
*
*   This method creates a catalog record with or without the actual text.
*   Users can also append text to this record by calling cedarAppendCatalogRecord
*
*   Inputs: 
*
*       kinst - instrument code from instTab.txt
*       modexp - code describing the mode of the experiment
*       year1, month1, day1, hour1, minute1, second1, centisecond1 - starting time 
*           of experiment
*       year2, month2, day2, hour2, minute2, second2, centisecond2 - starting time 
*           of experiment
*       text - text to append.  See Cedar database format for suggested layout.
*              Must be multiple of 80 characters in length - no line feeds.  May
*              be empty, if user is planning to use cedarAppendCatalogRecord.
*
*   Returns - pointer to Int16 holding newly allocated catalog record. User is 
*   responsible for freeing dynamically allocated array
*   when finished with it.
*/

Int16 *cedarCreateCatalogRecord(int kinst, int modexp,
                                int year1, int month1, int day1,
                                int hour1, int minute1, int second1, int centisecond1,
                                int year2, int month2, int day2,
                                int hour2, int minute2, int second2, int centisecond2,
				char * text)
{
    Int16 *cedarp;
    int ltot=0, krec=2001, i=0;
    
    /* check that strlen(text) is divisible by 80 */
    if (strlen(text)%80 != 0)
    {
        fprintf(stderr, "Error in cedarCreateCatalogRecord - length of text must be divisible by 80: %ld is not\n", strlen(text));
        return(NULL);
    }
    
    /* check that no line feeds included */
    for (i=0; i<strlen(text); i++)
    {
        if (text[i] == '\n')
	{
            fprintf(stderr, "Error in cedarCreateCatalogRecord - no line feeds allowed\n");
            return(NULL);
	}
    }

    ltot = 40*(1 + strlen(text)/80);
    cedarp = (Int16 *) malloc(ltot*sizeof(Int16));
    cedarp[0] = (Int16)ltot;
    cedarp[1] = krec;
    cedarp[2] = kinst;
    cedarp[3] = modexp;
    cedarp[4] = year1;
    cedarp[5] = 100*month1 + day1;
    cedarp[6] = 100*hour1 + minute1;
    cedarp[7] = 100*second1 + centisecond1;
    cedarp[8] = year2;
    cedarp[9] = 100*month2 + day2;
    cedarp[10] = 100*hour2 + minute2;
    cedarp[11] = 100*second2 + centisecond2;
    
    /* the rest of the prolog is zeroes */
    for (i=12; i<40; i++)
        cedarp[i] = 0;
    
    /* copy in the text */
    memcpy(cedarp+40, text, strlen(text));
    

    return(cedarp);
}


/***********************************************************************
*
* cedarAppendCatalogRecord  appends text to an existing Catalog Record
*
*   Users should first create a catalog record by calling cedarCreateCatalogRecord
*
*   Inputs: 
*
*       Int16 *cedarp - pointer to existing catalog record
*       char * text - text to append.  See Cedar database format for suggested layout.
*                     Must be multiple of 80 characters in length - no line feeds. 
*
*   Returns: 0 if success, -1 if failure 
*
*/
int cedarAppendCatalogRecord(Int16 **cedarpp, char * text)
{

    int i=0, old_ltot=0, ltot=0;
    
    
    /* check that strlen(text) is divisible by 80 */
    if (strlen(text)%80 != 0)
    {
        fprintf(stderr, "Error in cedarAppendCatalogRecord - length of text must be divisible by 80: %ld is not\n", strlen(text));
        return(-1);
    }
    
    /* check that no line feeds included */
    for (i=0; i<strlen(text); i++)
    {
        if (text[i] == '\n')
	{
            fprintf(stderr, "Error in cedarCreateCatalogRecord - no line feeds allowed\n");
            return(-1);
	}
    }
    
    /* check that this is really a catalog record */
    if ((*cedarpp)[1] != 2001 && (*cedarpp)[1] != 2101)
    {
        fprintf(stderr, "Error in cedarAppendCatalogRecord - cedarp does not point to catalog record\n");
        return(-1);
    }

    old_ltot = (*cedarpp)[0];
    ltot = old_ltot + strlen(text)/2;
    *cedarpp = (Int16 *) realloc((*cedarpp), ltot*sizeof(Int16));
    (*cedarpp)[0] = (Int16)ltot;
    
    
    /* copy in the text */
    memcpy((*cedarpp)+old_ltot, text, strlen(text));
    

    return(0);
}


/***********************************************************************
*
* cedarCreateHeaderRecord  creates a new Cedar Header record
*
*   This method creates a header record with or without the actual text.
*   Users can also append text to this record by calling cedarAppendHeaderRecord
*
*   Inputs: 
*
*       kinst - instrument code from instTab.txt
*       kindat - code describing the kind of data
*       year1, month1, day1, hour1, minute1, second1, centisecond1 - starting time 
*           of experiment
*       year2, month2, day2, hour2, minute2, second2, centisecond2 - starting time 
*           of experiment
*       jpar - number of single-valued parameters in accompanying data records
*       mpar - number of multiple-valued parameters in accompanying data records
*       text - text to append.  See Cedar database format for suggested layout.
*              Must be multiple of 80 characters in length - no line feeds.  May
*              be empty, if user is planning to use cedarAppendHeaderRecord.
*
*   Returns - pointer to Int16 holding newly allocated header record. User is 
*   responsible for freeing dynamically allocated array
*   when finished with it.
*/

Int16 *cedarCreateHeaderRecord(int kinst, int kindat,
                                int year1, int month1, int day1,
                                int hour1, int minute1, int second1, int centisecond1,
                                int year2, int month2, int day2,
                                int hour2, int minute2, int second2, int centisecond2,
				int jpar, int mpar,
				char * text)
{
    Int16 *cedarp;
    int ltot=0, krec=3002, i=0;
    
    /* check that strlen(text) is divisible by 80 */
    if (strlen(text)%80 != 0)
    {
        fprintf(stderr, "Error in cedarCreateHeaderRecord - length of text must be divisible by 80: %ld is not\n", strlen(text));
        return(NULL);
    }
    
    /* check that no line feeds included */
    for (i=0; i<strlen(text); i++)
    {
        if (text[i] == '\n')
	{
            fprintf(stderr, "Error in cedarCreateHeaderRecord - no line feeds allowed\n");
            return(NULL);
	}
    }

    ltot = 40*(1 + strlen(text)/80);
    cedarp = (Int16 *) malloc(ltot*sizeof(Int16));
    cedarp[0] = (Int16)ltot;
    cedarp[1] = krec;
    cedarp[2] = kinst;
    cedarp[3] = kindat;
    cedarp[4] = year1;
    cedarp[5] = 100*month1 + day1;
    cedarp[6] = 100*hour1 + minute1;
    cedarp[7] = 100*second1 + centisecond1;
    cedarp[8] = year2;
    cedarp[9] = 100*month2 + day2;
    cedarp[10] = 100*hour2 + minute2;
    cedarp[11] = 100*second2 + centisecond2;
    cedarp[12] = 40;
    cedarp[13] = jpar;
    cedarp[14] = mpar;
    
    /* the rest of the prolog is zeroes */
    for (i=15; i<40; i++)
        cedarp[i] = 0;
    
    /* copy in the text */
    memcpy(cedarp+40, text, strlen(text));
    

    return(cedarp);
}


/***********************************************************************
*
* cedarAppendHeaderRecord  appends text to an existing Header Record
*
*   Users should first create a header record by calling cedarCreateHeaderRecord
*
*   Inputs: 
*
*       Int16 *cedarp - pointer to existing header record
*       char * text - text to append.  See Cedar database format for suggested layout.
*                     Must be multiple of 80 characters in length - no line feeds. 
*
*   Returns: 0 if success, -1 if failure 
*
*/
int cedarAppendHeaderRecord(Int16 **cedarpp, char * text)
{

    int i=0, old_ltot=0, ltot=0;
    
    /* check that strlen(text) is divisible by 80 */
    if (strlen(text)%80 != 0)
    {
        fprintf(stderr, "Error in cedarAppendHeaderRecord - length of text must be divisible by 80: %ld is not\n", strlen(text));
        return(-1);
    }
    
    /* check that no line feeds included */
    for (i=0; i<strlen(text); i++)
    {
        if (text[i] == '\n')
	{
            fprintf(stderr, "Error in cedarCreateHeaderRecord - no line feeds allowed\n");
            return(-1);
	}
    }
    
    /* check that this is really a header record */
    if ((*cedarpp)[1] != 3002 && (*cedarpp)[1] != 3101)
    {
        fprintf(stderr, "Error in cedarAppendHeaderRecord - cedarp does not point to header record\n");
        return(-1);
    }

    old_ltot = (*cedarpp)[0];
    ltot = old_ltot + strlen(text)/2;
    (*cedarpp) = (Int16 *) realloc((*cedarpp), ltot*sizeof(Int16));
    (*cedarpp)[0] = (Int16)ltot;
    
    
    /* copy in the text */
    memcpy((*cedarpp)+old_ltot, text, strlen(text));
    

    return(0);
}


/***********************************************************************
*
* cedarSetKrec   sets Kind of record
*
*/

int cedarSetKrec (Int16 *cedarp, int krec)
{
    cedarp[1] = krec;
    return(0);
}


/***********************************************************************
*
* cedarSetKinst   sets instrument code for these data
*
*/

int cedarSetKinst (Int16 *cedarp, int kinst)
{
    cedarp[2] = kinst;
    return(0);
}


/***********************************************************************
*
* cedarSetKindat   sets kind-of-data code
*
*/

int cedarSetKindat (Int16 *cedarp, int kindat)
{
    cedarp[3] = kindat;
    return(0);
}


/***********************************************************************
*
* cedarSetStartTime   sets start time of record
*
*/

int cedarSetStartTime (Int16 *cedarp, int year, int month, int day,
                       int hour, int minute, int second, int centisecond)
{
    cedarp[4] = year;
    cedarp[5] = 100*month + day;
    cedarp[6] = 100*hour + minute;
    cedarp[7] = 100*second + centisecond;
    return(0);
}


/***********************************************************************
*
* cedarSetEndTime   sets end time of record
*
*/

int cedarSetEndTime (Int16 *cedarp, int year, int month, int day,
                     int hour, int minute, int second, int centisecond)
{
    cedarp[8] = year;
    cedarp[9] = 100*month + day;
    cedarp[10] = 100*hour + minute;
    cedarp[11] = 100*second + centisecond;
    return(0);
}


/***********************************************************************
*
* cedarSet1dParm   sets a 1D parameter in a Cedar record
*
*   Inputs:
*       Int16 *cedarp - pointer to existing Cedar record
*       int parcode   - Cedar parmater code
*       double parm   - doubles containing value to set.Special values
*                       may be set by setting values to #defines
*                       missing, assumed, or knownbad
*       int index     - index of which 2d parameter to set
*
*   Returns 1 if failure, 0 if success.  If value out of Int16 range, will set
*   value to missing and return failure.
*/

int cedarSet1dParm(Int16 *cedarp, int parcode, double parm, int index)
{
    int lprol=0, jpar=0, j=0, l=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    lprol = cedarp[12];
    jpar = cedarp[13];
    if (index > jpar-1) {
        return(1);
    }
    l = lprol + index;
    cedarp[l] = parcode;
    for (j=0; j<nparcodes; j++) {
        if (code[j] == abs(cedarp[l])) {
            if (parm == missing)
                cedarp[l+jpar] = (Int16)missingData;
            else if (parm == assumed && parcode < 0)
                cedarp[l+jpar] = (Int16)assumedData;
            else if (parm == knownbad && parcode < 0)
                cedarp[l+jpar] = (Int16)knownBadData;
            else if (parm >= 0.0)
	    {
	        /* check if out of range */
		if ((parm/scaleFactor[j] - 1) > knownBadData)
		{
		    cedarp[l+jpar] = (Int16)missingData;
		    return(1);
		}
		else
                    cedarp[l+jpar] = (Int16)(parm/scaleFactor[j] + 0.5);
	    }
            else
	    {
	        /* check if out of range */
		if ((parm/scaleFactor[j] + 1) < missingData)
		{
		    cedarp[l+jpar] = (Int16)missingData;
		    return(1);
		}
		else
                    cedarp[l+jpar] = (Int16)(parm/scaleFactor[j] - 0.5);
	    }
            break;
        }
    }

    return(0);
}


/***********************************************************************
*
* cedarSetNorm1dParm   sets a 1D parameter in a Cedar record with the units
*                      of the standard parameter even if additional increment parameter
*
*   Inputs:
*       Int16 *cedarp - pointer to existing Cedar record
*       int parcode   - Cedar parmater code
*       double parm   - doubles containing value to set.Special values
*                       may be set by setting values to #defines
*                       missing, assumed, or knownbad
*       int index     - index of which 2d parameter to set
*
*   Returns 1 if failure, 0 if success.  If value out of Int16 range, will set
*   value to missing and return failure.
*/

int cedarSetNorm1dParm(Int16 *cedarp, int parcode, double parm, int index)
{
    int lprol=0, jpar=0, j=0, l=0;
    double scale = 0.0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();
	
    scale = cedarGetNormScaleFactor (parcode);

    lprol = cedarp[12];
    jpar = cedarp[13];
    if (index > jpar-1) {
        return(1);
    }
    l = lprol + index;
    cedarp[l] = parcode;
    for (j=0; j<nparcodes; j++) {
        if (code[j] == abs(cedarp[l])) {
            if (parm == missing)
                cedarp[l+jpar] = (Int16)missingData;
            else if (parm == assumed && parcode < 0)
                cedarp[l+jpar] = (Int16)assumedData;
            else if (parm == knownbad && parcode < 0)
                cedarp[l+jpar] = (Int16)knownBadData;
            else if (parm >= 0.0)
	    {
	        /* check if out of range */
		if ((parm/scale - 1) > knownBadData)
		{
		    cedarp[l+jpar] = (Int16)missingData;
		    return(1);
		}
		else
                    cedarp[l+jpar] = (Int16)(parm/scale + 0.5);
	    }
            else
	    {
	        /* check if out of range */
		if ((parm/scale + 1) < missingData)
		{
		    cedarp[l+jpar] = (Int16)missingData;
		    return(1);
		}
		else
                    cedarp[l+jpar] = (Int16)(parm/scale - 0.5);
	    }
            break;
        }
    }

    return(0);
}


/***********************************************************************
*
* cedarSet2dParm   sets all values for a 2D parameter in a cedar record
*
*   Inputs:
*       Int16 *cedarp - pointer to existing Cedar record
*       int parcode   - Cedar parmater code
*       double *parmp - array of doubles containing values to set.  Length
*                       must be nrow.  Special values may be set by setting
*                       values to #defines missing, assumed, or knownbad
*       int index     - index of which 2d parameter to set
*
*   Returns 1 if failure, 0 if success. If any value out of Int16 range, will set
*   value to missing, but all valid values will still be set.  Returns 1 if any
*   out of range data found.
*
*/

int cedarSet2dParm(Int16 *cedarp, int parcode, double *parmp, int index)
{
    int k=0, j=0, l=0,lprol=0, jpar=0, mpar=0, nrow=0;
    int retCode = 0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (index > mpar-1) {
        return(1);
    } 
   
    l = lprol + 2*jpar + index;
    cedarp[l] = parcode;
    for (j=0; j<nparcodes; j++) {
        if (code[j] == abs(cedarp[l])) {
            for (k=0; k<nrow; k++) {
                if (parmp[k] == missing)
                    cedarp[l+(k+1)*mpar] = (Int16)missingData;
                else if (parmp[k] == assumed && parcode < 0)
                        cedarp[l+(k+1)*mpar] = (Int16)assumedData;
                else if (parmp[k] == knownbad && parcode < 0)
                        cedarp[l+(k+1)*mpar] = (Int16)knownBadData;
                else if (parmp[k] >= 0.0)
		{
		    /* check if out of range */
		    if ((parmp[k]/scaleFactor[j] + 1) > knownBadData)
		    {
			cedarp[l+(k+1)*mpar] = (Int16)missingData;
			retCode = 1;
		    }
		    else
                        cedarp[l+(k+1)*mpar] = (Int16)(parmp[k]/scaleFactor[j] + 0.5);
		}
                else
		{
		    /* check if out of range */
		    if ((parmp[k]/scaleFactor[j] - 1) < missingData)
		    {
			cedarp[l+(k+1)*mpar] = (Int16)missingData;
			retCode = 1;
		    }
		    else
                        cedarp[l+(k+1)*mpar] = (Int16)(parmp[k]/scaleFactor[j] - 0.5);
		}
            }
            break;
        }
    }

    return(retCode);
}


/***********************************************************************
*
* cedarSetNorm2dParm   sets all values for a 2D parameter in a cedar record
*                      with the units as standard parameter even if 
*                      additional increment parameter
*
*   Inputs:
*       Int16 *cedarp - pointer to existing Cedar record
*       int parcode   - Cedar parmater code
*       double *parmp - array of doubles containing values to set.  Length
*                       must be nrow.  Special values may be set by setting
*                       values to #defines missing, assumed, or knownbad
*       int index     - index of which 2d parameter to set
*
*   Returns 1 if failure, 0 if success. If any value out of Int16 range, will set
*   value to missing, but all valid values will still be set.  Returns 1 if any
*   out of range data found.
*
*/

int cedarSetNorm2dParm(Int16 *cedarp, int parcode, double *parmp, int index)
{
    int k=0, j=0, l=0,lprol=0, jpar=0, mpar=0, nrow=0;
    int retCode = 0;
    double scale = 0.0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();
	
    scale = cedarGetNormScaleFactor (parcode);

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (index > mpar-1) {
        return(1);
    } 
   
    l = lprol + 2*jpar + index;
    cedarp[l] = parcode;
    for (j=0; j<nparcodes; j++) {
        if (code[j] == abs(cedarp[l])) {
            for (k=0; k<nrow; k++) {
                if (parmp[k] == missing)
                    cedarp[l+(k+1)*mpar] = (Int16)missingData;
                else if (parmp[k] == assumed && parcode < 0)
                        cedarp[l+(k+1)*mpar] = (Int16)assumedData;
                else if (parmp[k] == knownbad && parcode < 0)
                        cedarp[l+(k+1)*mpar] = (Int16)knownBadData;
                else if (parmp[k] >= 0.0)
		{
		    /* check if out of range */
		    if ((parmp[k]/scale - 1) > knownBadData)
		    {
			cedarp[l+(k+1)*mpar] = (Int16)missingData;
			retCode = 1;
		    }
		    else
                        cedarp[l+(k+1)*mpar] = (Int16)(parmp[k]/scale + 0.5);
		}
                else
		{
		    /* check if out of range */
		    if ((parmp[k]/scale + 1) < missingData)
		    {
			cedarp[l+(k+1)*mpar] = (Int16)missingData;
			retCode = 1;
		    }
		    else
                        cedarp[l+(k+1)*mpar] = (Int16)(parmp[k]/scale - 0.5);
		}
            }
            break;
        }
    }

    return(retCode);
}


/***********************************************************************
*
* cedarSet1dInt   puts a 1D parameter (unscaled) into a madrigal record
*
*/

int cedarSet1dInt(Int16 *cedarp, int parcode, Int16 int1d, int index)
{
    int lprol=0, jpar=0, l=0;

    lprol = cedarp[12];
    jpar = cedarp[13];
    l = lprol + index;

    if (index > jpar-1) {
        return(1);
    }

    cedarp[l] = parcode;
    cedarp[l+jpar] = int1d;

    return(0);
}


/***********************************************************************
*
* cedarSet2dInt   puts a 2D parameter (unscaled) into a madrigal record
*
*/

int cedarSet2dInt(Int16 *cedarp, int parcode, Int16 *int2dp, int index)
{
    int k=0, l=0,lprol=0, jpar=0, mpar=0, nrow=0;

    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (index > mpar-1) {
        return(1);
    } 
    
    l = lprol + 2*jpar + index;
    cedarp[l] = parcode;
    for (k=0; k<nrow; k++) {
        cedarp[l+(k+1)*mpar] = int2dp[k];
    }
    return(0);
}


/***********************************************************************
*
* cedarPrintRecord   prints cedar record
*
*/

int cedarPrintRecord(Int16 *cedarp)
{
    int i=0, j=0, k=0, l=0, iblk=0, nblk=0, ltot=0, krec=0, lprol=0,
        jpar=0, mpar=0, nrow=0, nlines=0;
    double scale[100];
    int isError[100];
    char *s;
    int codeFound = 0;
    char prolParName[17][7] = {"  LTOT\0", "  KREC\0", " KINST\0",
                               "KINDAT\0", "  IBYR\0", "  IBDT\0",
                               "  IBHM\0", "  IBCS\0", "  IEYR\0",
                               "  IEDT\0", "  IEHM\0", "  IECS\0",
                               " LPROL\0", "  JPAR\0", "  MPAR\0",
                               "  NROW\0", "      \0"};

    ltot = cedarp[0];
    krec = cedarp[1];
    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];
    
    /* init isError to 0 */
    for (i=0; i<100; i++)
        isError[i] = 0;
    
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    /* reset prolog length if catalog record */
    if (krec == CATALOGBIN || krec == CATALOGASCII)
        lprol = 16;

    /* Print Prolog */
    (void) printf("Prolog\n");
    for (i=0; i<lprol; i++) {
        if (i < 16) {
            (void) printf("%s = %d\n", &prolParName[i][0], cedarp[i]);
        }
        /*
        else if (i==17) {
            startTime = (unsigned int)((unsigned short)cedarp[17]<<16) +
                                       (unsigned short)cedarp[18];
            (void) printf("PR[%d] = %d (start sec = %d)\n",
                i, cedarp[i], startTime); 
        }           
        else if (i==19) {
            endTime = (unsigned int)((unsigned short)cedarp[19]<<16) +
                                     (unsigned short)cedarp[20];
            (void) printf("PR[%d] = %d (  end sec = %d)\n",
                i, cedarp[i], endTime);  
        }
        else {
            (void) printf("PR[%d] = %d\n", i, cedarp[i]);
        }
        */
    }

    if (krec == DATABIN) {

    /* Print 1D Parameter Codes, Values and Descriptions */
    (void) printf("\n1D parameter codes, values and descriptions\n");
    for (i=lprol; i<lprol+jpar; i++) {
        codeFound = 0;
        for (j=0; j<nparcodes; j++) {
            if (code[j] == abs(cedarp[i]))
            {
                 codeFound = 1;
                 break;
            }
        }
        if (codeFound)
        {   
	    /* check for special values */
            if (cedarp[i] > 0) 
	        if (cedarp[i+jpar] != -32767)
                    (void) printf("%6d %12.5e %40s\n", cedarp[i], 
                        scaleFactor[j]*cedarp[i+jpar], description[j]);
	        else
		    (void) printf("%6d %12.5e %40s\n", cedarp[i], 
                        (double)cedarp[i+jpar], description[j]);
            else
	        if (cedarp[i+jpar] != -32767 && cedarp[i+jpar] != -32766 && cedarp[i+jpar] != 32767)
                    (void) printf("%6d %12.5e  Uncertainty in %40s\n", cedarp[i],
                        scaleFactor[j]*cedarp[i+jpar], description[j]);
		else
		    (void) printf("%6d %12.5e  Uncertainty in %40s\n", cedarp[i],
                        (double)cedarp[i+jpar], description[j]);
        }
        else /* unknown code */
        {
            if (cedarp[i] > 0) 
                (void) printf("%6d %12.5e %40s\n", cedarp[i], 
                    1.0*cedarp[i+jpar], "Unknown parameter");
            else
                (void) printf("%6d %12.5e  Uncertainty in %40s\n", cedarp[i],
                    1.0*cedarp[i+jpar], "Unknown parameter");
        }
    }

    /* Print 2D Parameter Codes and Descriptions */
    (void) printf("\n2D parameter codes and descriptions\n");
    for (i=0; i<mpar; i++) {
        codeFound = 0;
        l = lprol + 2*jpar + i;
        for (j=0; j<nparcodes; j++) {
            if (code[j] == abs(cedarp[l])) 
            {
                codeFound = 1;
                break;
            }
        }
        if (codeFound)
        {
            if (cedarp[l] > 0) 
                (void) printf("%6d %s\n", cedarp[l], description[j]);
            else
                (void) printf("%6d Uncertainty in %s\n",
                    cedarp[l], description[j]);
        }
        else /* unknown code */
        {
            if (cedarp[l] > 0) 
                (void) printf("%6d %s\n", cedarp[l], "Unknown parameter");
            else
                (void) printf("%6d Uncertainty in %s\n",
                    cedarp[l], "Unknown parameter");
        }
        
    }

    /* Print 2D parameters */
    (void) printf("\n2D parameters\n");
    nblk = mpar/12;
    if (nblk*12 != mpar) nblk++;
    for (iblk=0; iblk<nblk; iblk++) {
	l = lprol + 2*jpar;
        for (i=0; i<=nrow; i++) {
	    for (j=0; j<mpar; j++) {
		if (j>=12*iblk && j<12*(iblk+1)) {
		    if (i == 0) {
		        if (cedarp[l] < 0)
		            isError[j] = 1;
		        codeFound = 0;
		        for (k=0; k<nparcodes; k++) 
		        {
			        if (code[k] == abs(cedarp[l])) 
			        {
			            codeFound = 1;
			            break;
			        }
		        }
		        if (codeFound)
		            scale[j] = scaleFactor[k];
		        else
		            scale[j] = 1.0;
		        (void) printf("%11d", cedarp[l]);
		    } else {
                        if ((cedarp[l] == -32767) || (isError[j] == 1 && (cedarp[l] == -32766 || cedarp[l] == 32767)))
			    (void) printf("%11.4e", (double)cedarp[l]);
                        else
		            (void) printf("%11.4e", scale[j]*cedarp[l]);
		    }
	        }
                l++;
	    }
	    (void) printf("\n");
	}
    }

    } else if (krec == CATALOGBIN || krec == HEADERBIN) {
        nlines = ltot/40;
        /* Write remainder of record */
        for (i=1; i<nlines; i++) {
            s = (char *) (&((cedarp)[40*i]));
            for (k=79; k>=0; k--) {
                if (isgraph((int)s[k])) break;
            }
            for (j=0; j<=k; j++) {
                (void) fputc(s[j], stdout);
            }   
            (void) fputc('\n', stdout);            
        }     
    }
    (void) fflush(stdout);
    return(0);
}



/***********************************************************************
*
* cedarGetInformation   gets Ascii Information from Catalog or Header record
*
*   inputs:  Int16 * cedarp (pointer to cedar record)
*
*   outputs:  char * (pointer to dynamically allocated string holding
*             ASCII text in catalog or header record, or empty string
*             if no text available.  Will return empty string if called
*             with a data record instead of a header or catalog record).
*             The string will have 81 characters for each line of
*             information - the first 80 characters will be the 80
*             characters in the file with unprintable characters converted
*             to spaces, and the 81st character a newline. After the last
*             line a null character is added to make a valid c string.
*
*  The user is resposible for freeing the returned string when finished
*  with it.
*
*/

char * cedarGetInformation(Int16 *cedarp)
{
    int i=0, j=0,ltot=0, krec=0, nlines=0;
    char *s;
    char * information;


    ltot = cedarp[0];
    krec = cedarp[1];
    

    if (krec != CATALOGBIN && krec != HEADERBIN)
    {
        /* not a catalog or header record */
	information = (char *)malloc(1);
	information[0] = '\0';
    } 
    
    else 
    {
        nlines = ltot/40;
	if (nlines < 1) nlines = 1;
	/* allocate 81 bytes for each line plus final terminator */
	information = (char *)malloc(((81 * (nlines - 1)) + 1) * sizeof(char));
	
        /* Handle each line */
        for (i=1; i<nlines; i++) {
            s = (char *) (&((cedarp)[40*i]));
	    /* Handle each character - if non-printable, convert to space */
            for (j=0; j<80; j++) {
                if (!isgraph((int)s[j])) s[j] = ' ';
		information[(81 * (i - 1)) + j] = s[j];
            }
	    /* add newline at end of each line */
	    information[(81*i) - 1] = '\n';
        }
	/* add null to end of string */
	information[81*(nlines - 1)] = '\0';  
    }


    return(information);
}

/***********************************************************************
*
* cedarPrintProlog   prints cedar record prolog
*
*/

int cedarPrintProlog(Int16 *cedarp)
{
    int i=0, lprol=0;
    static const char *err1="cedarGetNextRec error - bad prolog length";

    /* Print Prolog */
    lprol = cedarp[12];
    if (lprol < 16 || lprol > 32) {
	(void) cedarSetError(err1);
        for (i=0; i<16; i++) {
            (void) printf("%6d", cedarp[i]);
        }
        return(1);
    }
    for (i=0; i<lprol; i++) {
        (void) printf("%6d", cedarp[i]);
    }
    (void) printf("\n");
    (void) fflush(stdout);
    return(0);
}


/***********************************************************************
*
* cedarReadParCodes   reads the following metadata tables:
*
*  1. parcods.tab  (parameter information)
*  2. instTab.txt   (instrument location)
*  3. madCatTab.txt  (parameter category information)
*
*   For the moment hard-coded to the column layout of parcods.tab
*      0-7     Code
*      10-48   Description   Note: DESC_LEN   = 40
*      50-60   Int16Desc     Note: DESC16_LEN = 12
*      62-68   ScaleFactor
*      70-77   Units         Note: UNIT_LEN   =  9
*      81-100  Mnemonic      Note: MNEM_LEN   = 21
*      105-112 Format
*      114-115 Width
*      118-120 CatId
*      122-122 hasDesc  - does this mnemonic have an html description?
*      124-124 hasErrDesc - does this error mnemonic have an html description?
*
*   Returns 0 if successful, non-zero and error set if not successful
*/

int cedarReadParCodes ()
{

    /* Data file declarations */
    FILE *fp;

    /* Miscellaneous declarations */
    int i=0, j=0, l=0, lineCount=0;
    int isAdditionIncr = 0; /* set to 1 if this parameter is an additional increment */
    char infile[128], s[1200], t[40];
    char * pToken;
    char tmpMnem[MNEM_LEN] = "";
    char stdMnem[MNEM_LEN] = "";
    static const char *err1="cedarReadParCodes error - parameter Code file does not exist";
    static const char *err3="cedarReadParCodes error - instTab.txt file does not exist";
    static const char *err4="cedarReadParCodes error - madCatTab.txt file does not exist";
    static const char *err5="cedarReadParCodes error - format error in parcods.tab";
    static const char *err6="cedarReadParCodes error - format error in instTab.txt";
    static const char *err7="cedarReadParCodes error - format error in madCatTab.txt";
    
    /* the global mutex cedar_mutex must be acquired before this method is called */
    pthread_mutex_lock(&cedar_mutex);
    /* if already loaded, just release mutex and return */
    if (nparcodes != 0)
    {
        pthread_mutex_unlock(&cedar_mutex);
        return(0);
    }
    
    /* This is the first thread to get the lock  */
    /* Load data and release mutex at the end    */

    /* Open  parameter table file for reading */
    cedarGetMadroot(infile);
    (void) strcat(infile, "/metadata/parcods.tab");
    if ((fp = fopen (infile, "r")) == NULL) {
        (void) cedarSetError(err1);
        pthread_mutex_unlock(&cedar_mutex);
        return(1);
    }

    /* Read Parameter Code File */
    lineCount = 0;
    for (l=0; l<MAXPARCODES; l++) {
        if (fgets(s, 1200, fp) == NULL) break;

        /* skip any lines less than min len of 118 characters */
        if (strlen(s) < 118)
            continue;

        /* Code */
        j=0;
        for (i=0; i<=7; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%d", &code[l]);

        /* description */
        j=0;
        for (i=10; i<=48; i++) {
            description[l][j++] = s[i];
        }
        description[l][j] = '\0';
	/* check whether this is an additional increment parameter */
	if (strstr(description[l], "Additional increment") != NULL)
	{
	    isAdditionIncr = 1;
	    /* add new additional increment parm with code - 1 */
	    cdi[ipar] = code[l] - 1;
	    ipar++;
	}
	else
	    isAdditionIncr = 0;

        /* Int16_Description */
        j=0;
        for (i=50; i<=60; i++) {
            Int16Description[l][j++] = s[i];
        }
        Int16Description[l][j] = '\0';

        /* scaleFactor */
        j=0;
        for (i=62; i<=68; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%le", &scaleFactor[l]);
	
	/* get additional increment scale factor if needed */
	if (isAdditionIncr)
	    faci[ipar-1] = scaleFactor[l];

        /* units */
        j=0;
        for (i=70; i<=77; i++) {
            units[l][j++] = s[i];
        }
        units[l][j] = '\0';

        /* mnemonic - convert to standard form */
        j=0;
        for (i=81; i<=100; i++) {
            tmpMnem[j++] = s[i];
        }
        tmpMnem[j] = '\0';
        getStdMnem(tmpMnem, stdMnem);
        strcpy(mnemonic[l], stdMnem);

        /* format */
        j=0;
        for (i=105; i<=112; i++) {
            format[l][j++] = s[i];
        }
        format[l][j] = '\0';

        /* width */
        j=0;
        for (i=114; i<=115; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%d", &width[l]);

        /* catId */
        j=0;
        for (i=118; i<=120; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%d", &catId[l]);
        
        /* hasDesc */
        j=0;
        for (i=122; i<=122; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%d", &hasDesc[l]);
        
        /* hasErrDesc */
        j=0;
        for (i=124; i<=124; i++) {
            t[j++] = s[i];
        }
        t[j] = '\0';
        (void) sscanf(t, "%d", &hasErrDesc[l]);

        lineCount++;

    }
    nparcodes = lineCount;
    (void) fclose(fp);

    /* raise error if nparcodes == 0 */
    if (nparcodes == 0)
    {
        (void) cedarSetError(err5);
        return(1);
    }

    /* now parse the instTab.txt file */

    cedarGetMadroot(infile);
    (void) strcat(infile, "/metadata/instTab.txt");
    if ((fp = fopen (infile, "r")) == NULL) {
        (void) cedarSetError(err3);
        pthread_mutex_unlock(&cedar_mutex);
        return(1);
    }

    /* Read instTab.txt File */
    lineCount = 0;
    for (l=0; l<MAXINSTRUMENTS; l++) {
        if (fgets(s, 1200, fp) == NULL) break;

        /* tokenize the line using comma as tokenizer */
        pToken = strtok(s, ",");
        if (pToken == NULL)
            continue;
        kinstList[l] = atoi(pToken);

        /* skip next token, then get instname */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        strncpy(kinstName[l], pToken, INST_LEN-1);
        kinstName[l][INST_LEN-1] = '\0';
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        /* if not a number, set to missing */
        if (strpbrk(pToken, "1234567890") == NULL)
            kinstLat[l] = missing;
        else
            kinstLat[l] = atof(pToken);

        /* next get longitude */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        /* if not a number, set to missing */
        if (strpbrk(pToken, "1234567890") == NULL)
            kinstLon[l] = missing;
        else
            kinstLon[l] = atof(pToken);

        /* finally get altitude */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        /* if not a number, set to 0.0 (default alt) */
        if (strpbrk(pToken, "1234567890") == NULL)
            kinstAlt[l] = 0.0;
        else
            kinstAlt[l] = atof(pToken);
	    
	/* if kinstLat, kinstLon, and kinstAlt are all 0 or missing, set all to missing */
	if ((kinstLat[l] == 0.0 || kinstLat[l] == missing) &&
	    (kinstLon[l] == 0.0 || kinstLon[l] == missing) &&
	    (kinstAlt[l] == 0.0 || kinstLon[l] == missing))
	{
	    kinstLat[l] = missing;
	    kinstLon[l] = missing;
	    kinstAlt[l] = missing;
	}

        lineCount++;
    }

    nkinst = lineCount;
    (void) fclose(fp);

    /* raise error if nkinst == 0 */
    if (nkinst == 0)
    {
        (void) cedarSetError(err6);
        return(1);
    }


    /* now parse the madCatTab.txt.txt file */

    cedarGetMadroot(infile);
    (void) strcat(infile, "/metadata/madCatTab.txt");
    if ((fp = fopen (infile, "r")) == NULL) {
        pthread_mutex_unlock(&cedar_mutex);
        (void) cedarSetError(err4);
        return(1);
    }

    /* Read madCatTab.txt File */
    lineCount = 0;
    for (l=0; l<MAXCATEGORIES; l++) {
        if (fgets(s, 1200, fp) == NULL) break;

        /* tokenize the line using comma as tokenizer */
        pToken = strtok(s, ",");
        if (pToken == NULL)
            continue;
        /* set j = catId */
        j = atoi(pToken);

        /* next, get category description - truncate if too long */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        strncpy(catList[j], pToken, CAT_LEN - 1);
        catList[j][CAT_LEN - 1] = '\0';

        /* read category minimum */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        catMin[j] = atoi(pToken);

        /* read category maximum */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
            continue;
        catMax[j] = atoi(pToken);

        lineCount++;
    }

    nCategories = lineCount;
    (void) fclose(fp);

    /* raise error if nCategories == 0 */
    if (nCategories == 0)
    {
        pthread_mutex_unlock(&cedar_mutex);
        (void) cedarSetError(err7);
        return(1);
    }
    
    pthread_mutex_unlock(&cedar_mutex);

    return(0);
}

 
/***********************************************************************
*
* cedarGetNumParCodes   Gets number of Cedar parameter codes in parcods.tab
*/

int cedarGetNumParCodes ()
{
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    return(nparcodes);
}


/***********************************************************************
*
* cedarGetParCode   Gets Cedar parameter code from table given its position
*                   in file parcods.tab
*
*
*/

int 
cedarGetParCode (int position)
{
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    return(code[position]);
}


/***********************************************************************
*
* cedarGetParCodeIndex   Gets index of Cedar parameter code in table, given its code
*
*   For a pure Madrigal parameter with code 0, will return missing.  Use
*   madGetParMnemIndex instead.  For a negative parcode, will return negitive
*   of index found.  If not found, returns missingData.
*
*   No longer requires that parcods.tab be in order.
*/

int cedarGetParCodeIndex (int parcode)
{
    int i = 0;

    if (parcode == 0)
        return(missingData);

    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    for (i = 0; i < nparcodes; i++)
    {
        if (abs(parcode) == code[i])
        {
            /* found match */
            if (parcode >= 0)
                return i;
            else
                return -i;
        }
    }

    return(missingData);
}



/***********************************************************************
*
*  madGetParMnemIndex   Gets index of Madrigal parameter code in table, given its mnemonic
*
*   Returns the index of the specified mnemonic.  Matching is case-insensitive, and
*   ignores whitespace. If not found and begins with "D", will next search with "D"
*   removed, and return the negitive of the index found.  If still not found, 
*   returns missingData.
*
*/
int madGetParMnemIndex (char * mnem)
{
    int i = 0;
    char stdMnem[MNEM_LEN] = "";

    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    getStdMnem(mnem, stdMnem);

    /* try to find non-error index */
    for (i = 0; i < nparcodes; i++)
    {
        if (strcmp(stdMnem, mnemonic[i]) == 0)
            return i;
    }

    /* now try to find error index */
    if (stdMnem[0] != 'D')
        return (missingData);

    for (i = 0; i < nparcodes; i++)
    {
        if (strcmp(stdMnem + 1, mnemonic[i]) == 0)
            return (-i);
    }

    return(missingData);
}



/***********************************************************************
*
*  isMadparmError   returns 1 if this is an error parm, 0 if standard, 
*                   -1 if neither
*
*
*/
int isMadparmError(const char * mnem)
{
    int i = 0;
    char stdMnem[MNEM_LEN] = "";

    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    getStdMnem(mnem, stdMnem);

    /* try to find non-error index */
    for (i = 0; i < nparcodes; i++)
    {
        if (strcmp(stdMnem, mnemonic[i]) == 0)
            return (0);
    }

    /* now try to find error index */
    if (stdMnem[0] == 'D')
    {
        for (i = 0; i < nparcodes; i++)
        {
            if (strcmp(stdMnem + 1, mnemonic[i]) == 0)
                return (1);
        }
    }
    
    /* check if stdMnem is an integer */
    for (i = 0; i < strlen(mnem); i++)
    {
        if (!isdigit(stdMnem[i]) && stdMnem[i] != '-')
            return (-1); /* not an integer */
    }
    
    /* stdMnem is an integer */
    i = atoi(stdMnem);
    
    if (i < 0)
        return (1);
    else
        return (0);
}


/***********************************************************************
*
*  getStdMnem   converts a str to standard mnemonic form
*
*   Inputs: const char * mnem    - the string containing the mnemonic to be converted
*           char * stdMnem - a string to copy the standard form of the 
*                            mnemonic to.  Allocated by the user.  At most
*                            MNEM_LEN - 1 characters will be copied.  Std form
*                            strips all whitespace and is upper case.
*
*/

void getStdMnem (const char * mnem, char * stdMnem)
{
    int i = 0, j = 0;

    for (i=0; i < strlen(mnem); i++)
    {
        if (i == MNEM_LEN - 1)
        {
            stdMnem[i] = '\0';
            return;
        }
        if (mnem[i] == ' ' || mnem[i] == '\t' || mnem[i] == '\n')
            continue;
        /* valid character found */
        stdMnem[j++] = toupper(mnem[i]);
    }
    stdMnem[j] = '\0';
}



/***********************************************************************
*
* cedarGetParCodeType   Gets type of Cedar parameter code from table, given its code.
*
*   For a pure Madrigal parameter with code 0, will return first found.  Use
*   madGetParMnemType instead.  If not in parcods.tab but in a standard range,
*   as defined by madCatTab.txt will return Cedar values.  If not in parcodes
*   and not in any standard range, will return "Unknown Parameter Type"
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*/

char * cedarGetParCodeType (int parcode)
{
    int index=0;
    char * retStr = NULL;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*CAT_LEN);
    
    /* first find index, if any */
    index = cedarGetParCodeIndex(parcode);

    if (index != missingData)
    {
        strcpy(retStr, catList[catId[abs(index)]]);
        return (retStr);
    }

    /* else parcode was not in parcods.tab, */
    /* try to return type based on min and max in madCatTab.txt */

    for (index = 0; index < nCategories; index++)
    {
        if (abs(parcode) >= catMin[index] && abs(parcode) <= catMax[index])
        {
            strcpy(retStr, catList[index]);
            return (retStr);
        }
    }

    /* outside of standard ranges */
    strcpy(retStr, "Unknown Parameter Type");
    return(retStr);
}


/***********************************************************************
*
* madGetParMnemType   Gets type of Madrigal parameter from table, given its mnemonic.
*
*   If mnemonic not in parcods.tab, will return "Unknown Parameter Type"
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*/

char * madGetParMnemType (char * mnem)
{
    int index=0, i=0;
    char * retStr = NULL;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*CAT_LEN);

    /* first find index, if any */
    index = madGetParMnemIndex(mnem);

    if (index != missingData)
        strcpy(retStr, catList[catId[abs(index)]]);
    else
    {
        /* check if mnem is an integer */
        for (i = 0; i < strlen(mnem); i++)
        {
            if (!isdigit(mnem[i]) && mnem[i] != '-')
            {
                strcpy(retStr, "Unknown Parameter Type");
                return(retStr);
            }
        }
        /* mnemonic is an integer, let cedarGetParCodeType deal with it */
        free(retStr);
        retStr = cedarGetParCodeType(atoi(mnem));
    }

    return(retStr);
}



/***********************************************************************
*
* madGetCategoryIndex   Gets the index of a given Category name.
*
*   If category string not found, returns missingData
*   Matching is case and whitespace sensitive
*
*/

int madGetCategoryIndex (char * category)
{
    int index=0;

    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    for(index = 0; index < nCategories; index++)
    {
        if(strcmp(category, catList[index]) == 0)
            return(index);
    }
    return(missingData);
}



/***********************************************************************
*
* cedarGetParDescription   Gets Cedar parameter code description from
* table
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*
*
*/

char * cedarGetParDescription (int parcode)
{
    int i=0;
     
     /* Note that allocated string is longer than DESC_LEN to allow */
     /* "Error in " to be prepended                                 */
    char * desc = malloc(sizeof(char) * (DESC_LEN + 10));
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    if (i == missingData) {
        strcpy(desc, "Unknown Parameter Code");
        return(desc);
    }

    if (parcode > 0 ) {
        strcpy(desc, description[i]);
        return(desc);
    } else {
        (void) strcpy(desc, "Error in ");
        (void) strcat(desc, description[abs(i)]);
        return(desc);
    }
}



/***********************************************************************
*
* madGetParDescription   Gets Madrigal parameter code description from
* table, given mnemonic
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*
*/

char * madGetParDescription (char * mnem)
{
    int i=0;

     /* Note that allocated string is longer than DESC_LEN to allow */
     /* "Error in " to be prepended                                 */
    char * desc = malloc(sizeof(char) * (DESC_LEN + 10));
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = madGetParMnemIndex(mnem);
    if (i == missingData) {
        strcpy(desc, "Unknown Parameter Code");
        return(desc);
    }

    if (i > 0 ) {
        strcpy(desc, description[i]);
        return(desc);
    } else {
        (void) strcpy(desc, "Error in ");
        (void) strcat(desc, description[abs(i)]);
        return(desc);
    }
}


/***********************************************************************
*
* cedarGetParInt16Description   Gets Cedar parameter code Int16
*                               description from table
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*/

char * cedarGetParInt16Description (int parcode)
{
    int i=0;
    char * retStr = NULL;

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*DESC16_LEN);
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        strcpy(retStr, "Unknown");
    }
    else
        strcpy(retStr, Int16Description[abs(i)]);

    return(retStr);
}



/***********************************************************************
*
* madGetParInt16Description   Gets Madrigal parameter code Int16
*                               description from table, given mnemonic
*
*/

char * madGetParInt16Description (char * mnem)
{
    int i=0;
    char * retStr = NULL;

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*DESC16_LEN);
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        strcpy(retStr, "Unknown");
    }
    else
        strcpy(retStr, Int16Description[abs(i)]);

    return(retStr);
}


/***********************************************************************
*
* cedarGetParScaleFactor   Gets Cedar parameter scale factor from table
*
*/

double cedarGetParScaleFactor (int parcode)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        return(1.0);
    }
    
    return(scaleFactor[abs(i)]);
}


/***********************************************************************
*
* madGetParScaleFactor   Gets Madrigal parameter scale factor from table,
*                        given mnemonic
*
*/

double madGetParScaleFactor (char * mnem)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        return(1.0);
    }
    
    return(scaleFactor[abs(i)]);
}


/***********************************************************************
*
* cedarGetNormScaleFactor   Gets Cedar parameter scale factor, where additional
*                           increment parameters use the same units as main
*                           parameter.  Differs from cedarGetParScaleFactor, which
*                           returns scale factors for additional increment parameters
*                           that may have different units than the main parameter.
*
*/

double cedarGetNormScaleFactor (int parcode)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();
	
    /* see if this is an additional increment parameter */
    for (i=0; i<ipar; i++)
    {
    	if (abs(parcode) == cdi[i]+1)
	    return(faci[i]);
    }

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        return(1.0);
    }
    
    return(scaleFactor[abs(i)]);
}


/***********************************************************************
*
* madGetNormScaleFactor   Gets Madrigal parameter scale factor, where additional
*                           increment parameters use the same units as main
*                           parameter.  Differs from cedarGetParScaleFactor, which
*                           returns scale factors for additional increment parameters
*                           that may have different units than the main parameter.
*
*/

double madGetNormScaleFactor (char * mnem)
{
    int i=0;

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        return(1.0);
    }
    
    return(cedarGetNormScaleFactor(i));
}


/***********************************************************************
*
* cedarGetParUnits   Gets Cedar parameter code units from table
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*/

char * cedarGetParUnits (int parcode)
{
    int i=0;
    char * retStr = NULL;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*UNIT_LEN);

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        strcpy(retStr, "Unknown");
    }
    else
        strcpy(retStr, units[abs(i)]);

    return(retStr);
}


/***********************************************************************
*
* madGetParUnits   Gets Madrigal parameter code units from table,
*                  given mnemonic
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.
*/

char * madGetParUnits (char * mnem)
{
    int i=0;
    char * retStr = NULL;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    /* allocate string to return */
    retStr = (char *)malloc(sizeof(char)*UNIT_LEN);

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        strcpy(retStr, "Unknown");
    }
    else
        strcpy(retStr, units[abs(i)]);

    return(retStr);
}


/***********************************************************************
*
* cedarGetParMnemonic   Gets Cedar parameter code mnemonic from table
*
*   User is responsible for freeing dynamically allocated string
*   when finished with it.  If parcode 0 passed in, first Madrigal
*   parameter found will be returned.
*
*   If unknown parcode passed in, mnemonic is atoi(parcode)
*/

char * cedarGetParMnemonic (int parcode)
{
    int i=0;

    /* allocated string 1 more than MNEM_LEN to allow */
    /* "D" to be prepended                            */
    char * mnem = malloc(sizeof(char) * (MNEM_LEN + 1));
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    if (i == missingData) {
        sprintf(mnem, "%i", parcode);
        return(mnem);
    }
    
    if (parcode > 0 ) {
        strcpy(mnem, mnemonic[i]);
        return(mnem);
    } else {
        (void) strcpy(mnem, "D");
        (void) strcat(mnem, mnemonic[abs(i)]);
        return(mnem);
    }
}

/***********************************************************************
*
* cedarGetParCodeFromMnemonic   Gets Cedar parameter code given mnemonic 
*
*   If mnemonic is integer in form of string, returns that integer
*   If not found, returns missingData
*
*/

int cedarGetParCodeFromMnemonic (char * mnem)
{
    int index = 0;
    int i = 0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    index = madGetParMnemIndex(mnem);

    if (index == missingData) {
        /* check if the mnemonic is already an integer */
        for (i=0; i<strlen(mnem); i++)
            if (!isdigit(mnem[i]) && mnem[i] != '-')
                return(missingData);
        /* is an integer */
        return(atoi(mnem));
    }
    
    if (index >= 0)
        return(code[index]);
    else
        return(-1 * code[abs(index)]); 
}


/***********************************************************************
*
* cedarGetParFormat   Gets Cedar parameter code format from table.
*
*   If not found, returns NULL
*/

char * cedarGetParFormat (int parcode)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);

    if (i == missingData)
    {
        return (NULL);
    }
    else
        return(format[abs(i)]);
}



/***********************************************************************
*
* madGetParFormat   Gets Madrigal parameter format from table (given mnemonic)
*
*   If not found, returns NULL
*/

char * madGetParFormat (char * mnem)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();


    i = madGetParMnemIndex(mnem);

    if (i == missingData)
    {
        return (NULL);
    }
    else
        return(format[abs(i)]);
}


/***********************************************************************
*
* cedarGetParWidth   Gets Cedar parameter field width from table
*
*    If unknown, returns default value of 11
*
*/

int cedarGetParWidth (int parcode)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        return(11);
    }
    
    return(width[abs(i)]);
}



/***********************************************************************
*
* madGetParWidth   Gets Madrigal parameter field width from table,
*                  given mnemonic
*
*    If unknown, returns default value of 11
*/

int madGetParWidth (char * mnem)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        return(11);
    }
    
    return(width[abs(i)]);
}


/***********************************************************************
*
* cedarHasHtmlDesc   Returns 1 if parameter has entry in Html description
*                    page, 0 if not.  Works also for error codes (< 0)
*
*    If unknown, returns default value of 0
*
*/

int cedarHasHtmlDesc(int parcode)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = cedarGetParCodeIndex(parcode);
    
    if (i == missingData) {
        return(0);
    }
    
    if (parcode > 0)
        return(hasDesc[abs(i)]);
    else
        return(hasErrDesc[abs(i)]);
}



/***********************************************************************
*
* madHasHtmlDesc   Returns 1 if mnemonic has entry in Html description
*                    page, 0 if not.  Works also for error mnemonics.
*
*    If unknown, returns default value of 0
*/

int madHasHtmlDesc (char * mnem)
{
    int i=0;
    
    /* be sure cedarReadParCodes has been called */
    if (nparcodes == 0)
        cedarReadParCodes();

    i = madGetParMnemIndex(mnem);
    
    if (i == missingData) {
        return(0);
    }
    
    if (isMadparmError(mnem))
        return(hasErrDesc[abs(i)]);
    else
        return(hasDesc[abs(i)]);
}



/***********************************************************************
*
* cedarCheckRecord   checks cedar record for consistency
*
*/

int cedarCheckRecord (Int16 *cedarp)
{
    int ltot=0, lprol=0, jpar=0, mpar=0, nrow=0;

    ltot = cedarp[0];
    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (ltot != lprol + 2*jpar + mpar*(nrow+1)) {
        return(1);
    }
    return(0);
}


/***********************************************************************
*
* cedarHexPrintRecord   prints hex version of record
*
*/

int cedarHexPrintRecord (Int16 *cedarp)
{
    int ltot, i, j;
    char cbuf1[9], cbuf2[5];

    ltot = cedarp[0];

    cbuf2[4] = '\0';
    for (i=0; i<ltot; i++) {
        (void) sprintf(cbuf1, "%8x", cedarp[i]);
        for (j=4; j<8; j++)
            if (cbuf1[j] == ' ')
                cbuf2[j-4] = '0';
            else
                cbuf2[j-4] = cbuf1[j];
        (void) printf("%4s ", cbuf2);
        if (16*((i+1)/16) == i+1)
            (void) printf("\n");
    }
    (void) printf("\n");

    (void) fflush(stdout);
    return(0);
}


/***********************************************************************
*
* cedarDecimalPrintRecord   prints hex version of record
*
*/

int cedarDecimalPrintRecord (Int16 *cedarp)
{
    int ltot=0, i=0;

    ltot = cedarp[0];

    for (i=0; i<ltot; i++) {
        (void) printf("%6d ", cedarp[i]);
        if (10*((i+1)/10) == i+1)
            (void) printf("\n");
    }
    (void) printf("\n");

    (void) fflush(stdout);
    return(0);
}


/***********************************************************************
*
* cedarSetError   sets cedar error
*
*/

int cedarSetError (const char *error) 
{
    if (lastError != (char *)NULL)
        free(lastError); 
    lastError = (char *)malloc(strlen(error)+1);
    (void) strcpy(lastError, error);
    if (pflag != 0) (void) printf("%s\n", error);
    return(0);
}


/***********************************************************************
*
* cedarGetError   gets last cedar error
*
*/

char * cedarGetError ()
{
    return(lastError);
}


/***********************************************************************
*
* cedarTabInt   linear interpolation routine
*
* tabint interpolates linearly to calculate y(x) from a table
* containing nt independent variable values xt and dependent
* variable values yt. the xt are assumed to be in non-decreasing
* order.
*
*/

double cedarTabInt (int nt, double *xt, double *yt, double x, double badval)
{

    int n=0;
    double y=0.0;

    /* check input values */
    if(nt < 1 || x < xt[0] || x > xt[nt-1]) {
       y = badval;
       return (y);
    }

    /* find the tabular points on either side of x */
    for (n=1; n<nt; n++) {
       if(x <= xt[n]) break;
    }


    /* calculate y */
    if (yt[n] == badval || yt[n-1] == badval || xt[n] <= xt[n-1]) {
        y = badval;
    } else {
        y = yt[n-1] + (x-xt[n-1])*(yt[n]-yt[n-1])/(xt[n]-xt[n-1]);
    }

    return (y);
}


/***********************************************************************
*
* cedarUpdateParmsList   Updates list of parameters and their minimum
*                        and maximum values
*
*   The first eleven parameters are effectively derived:
*   [0] 10: year, [1] 11: month,  [2] 12: day
*   [3] 13: hour, [4] 14: minute, [5] 15: second, 
*   [6] 16: centisecond [7] 34: uth, [8] 160: gdlat, 
*   [9] 170: glon, [10] 110: gdalt 
*
*   All parameters actually in the file will be listed starting with
*   the 12th.  If any of the above parameters are actually in the file
*   itself, they will appear again.
*
*   Does not include data from 2D rows if all error parameters are
*   missing of knownbad
*
*   Also updates earliestStartTime, latestEndTime, and lists of
*   all kinsts and kindats found in file.
*
*   If header or catalog record, returns 0 immediately without making
*   any changes.
*
*/

int cedarUpdateParmsList(Int16 *cedarp, int *numParmsp,
                         int *parmsListpp[], int *parmLocpp[],
                         double *parmMinpp[], double *parmMaxpp[], int *parmMissing[],
                         int *startJday0, 
                         double * earliestStartTime, double * latestEndTime,
                         int * numKinst, int * kinstArr,
                         int * numKindat, int * kindatArr)
{
    int i=0,j=0, k=0, foundit=0, code=0,lprol=0, jpar=0, mpar=0, 
        nrow=0,year=0, month=0, day=0, hour=0, minute=0, second=0,
        centisecond=0, krec=0, kinst=0, kindat=0, retVal=0;
    double startJday=0.0, ut1=0.0, ut2=0.0, uth=0.0, *val;
    double startTime=0.0, endTime=0.0;
    double *gdlatp, *glonp, *gdaltp;
    int kinstFound=0, kindatFound=0;
    int isFirstRecord = 0;  /* keeps track of whether this is the first record */
    int * hasGoodData = NULL; /* dynamically allocated array that determines whether each row has good data */

    krec = cedarp[1];
    kinst = cedarp[2];
    kindat = cedarp[3];
    lprol = cedarp[12];
    jpar = cedarp[13];
    mpar = cedarp[14];
    nrow = cedarp[15];

    if (krec == CATALOGBIN || krec == HEADERBIN)
        return(0);
        
    /* allocate hasGoodData and set values */
    hasGoodData = (int *)malloc(sizeof(int) * nrow);
    for (k=0; k<nrow; k++) 
        hasGoodData[k] = goodDataExists(cedarp, k);

    /* Get time parameters from prolog */
    if (*numParmsp == 0) {
        /* this is the first record */
        isFirstRecord = 1;
        *numParmsp = 8;
        *parmsListpp = (int *)malloc(*numParmsp*sizeof(int));
        *parmLocpp = (int *)malloc(*numParmsp*sizeof(int));
        *parmMinpp = (double *)malloc(*numParmsp*sizeof(double));
        *parmMaxpp = (double *)malloc(*numParmsp*sizeof(double));
        *parmMissing = (int *)malloc(*numParmsp*sizeof(int));
        for (i=0; i<*numParmsp; i++) {
            (*parmLocpp)[i] = 3;
        }
        (*parmsListpp)[0] = 10;    /* year        */
        (*parmsListpp)[1] = 11;    /* month       */
        (*parmsListpp)[2] = 12;    /* day         */
        (*parmsListpp)[3] = 13;    /* hour        */
        (*parmsListpp)[4] = 14;    /* minute      */
        (*parmsListpp)[5] = 15;    /* second      */
        (*parmsListpp)[6] = 16;    /* centisecond */
        (void) cedarGetStartTime (cedarp, &year, &month, &day,
                           &hour, &minute, &second, &centisecond);
        startTime = getKey(year, month, day, hour, minute, second);
        *earliestStartTime = startTime;
        (*parmMinpp)[0] = year;
        (*parmMinpp)[1] = month;
        (*parmMinpp)[2] = day;
        (*parmMinpp)[3] = hour;
        (*parmMinpp)[4] = minute;
        (*parmMinpp)[5] = second;
        (*parmMinpp)[6] = centisecond;
        (void) cedarGetEndTime (cedarp, &year, &month, &day,
                         &hour, &minute, &second, &centisecond);
        endTime = getKey(year, month, day, hour, minute, second);
        *latestEndTime = endTime;
        (*parmMaxpp)[0] = year;
        (*parmMaxpp)[1] = month;
        (*parmMaxpp)[2] = day;
        (*parmMaxpp)[3] = hour;
        (*parmMaxpp)[4] = minute;
        (*parmMaxpp)[5] = second;
        (*parmMaxpp)[6] = centisecond;
        startJday = cedarGetStartJday(cedarp);
        *startJday0 = (int)startJday;
        ut1 = 24.0*(cedarGetStartJday(cedarp) - *startJday0);
        ut2 = 24.0*(cedarGetEndJday(cedarp) - *startJday0);
        uth = 0.5*(ut1 + ut2);
        (*parmsListpp)[7] = 34;    /* hours past 0000 UT */
        (*parmMinpp)[7] = uth;
        (*parmMaxpp)[7] = uth;
        /* time is never missing */
        (*parmMissing)[0] = 0;
        (*parmMissing)[1] = 0;
        (*parmMissing)[2] = 0;
        (*parmMissing)[3] = 0;
        (*parmMissing)[4] = 0;
        (*parmMissing)[5] = 0;
        (*parmMissing)[6] = 0;
        (*parmMissing)[7] = 0;

    } else {
        /* Note: the following code makes the assumption that records are arranged in
           chronological order - this should be verified - temp only 
           Use getKey method to check both start and end time*/

        (void) cedarGetEndTime (cedarp, &year, &month, &day,
                         &hour, &minute, &second, &centisecond);
                         
        endTime = getKey(year, month, day, hour, minute, second);
        if (endTime > *latestEndTime)
            *latestEndTime = endTime;
            
        (*parmMaxpp)[0] = year;
        (*parmMaxpp)[1] = month;
        (*parmMaxpp)[2] = day;
        (*parmMaxpp)[3] = hour;
        (*parmMaxpp)[4] = minute;
        (*parmMaxpp)[5] = second;
        (*parmMaxpp)[6] = centisecond;
        ut1 = 24.0*(cedarGetStartJday(cedarp) - *startJday0);
        ut2 = 24.0*(cedarGetEndJday(cedarp) - *startJday0);
        uth = 0.5*(ut1 + ut2);
        (*parmMaxpp)[7] = uth;
    }

    /* Get geodetic coordinates */
    if (*numParmsp == 8) {
        *numParmsp = 11;
        *parmsListpp = (int *)realloc(*parmsListpp, (*numParmsp)*sizeof(int));
        *parmLocpp = (int *)realloc(*parmLocpp, (*numParmsp)*sizeof(int));
        *parmMinpp = (double *)realloc(*parmMinpp, (*numParmsp)*sizeof(double));
        *parmMaxpp = (double *)realloc(*parmMaxpp, (*numParmsp)*sizeof(double));
        *parmMissing = (int *)realloc(*parmMissing, (*numParmsp)*sizeof(int));
        (*parmsListpp)[8] = 160;    /* latitude  */
        (*parmMinpp)[8] = 1.e38;
        (*parmMaxpp)[8] = -1.e38;
        (*parmsListpp)[9] = 170;    /* longitude */
        (*parmMinpp)[9] = 1.e38;
        (*parmMaxpp)[9] = -1.e38;
        (*parmsListpp)[10] = 110;   /* altitude  */
        (*parmMinpp)[10] = 1.e38;
        (*parmMaxpp)[10] = -1.e38;
        /* assume not missing */
        (*parmMissing)[8] = 0;
        (*parmMissing)[9] = 0;
        (*parmMissing)[10] = 0;

    }
    retVal = cedarGetGeodetic(cedarp, &gdlatp, &glonp, &gdaltp);
    if (retVal != -1)
    {
        for (k=0; k<nrow; k++) {
        
            /* skip rows without valid error measurements */
            if (!hasGoodData[k])
                continue;
                
            if ((gdlatp[k] < (*parmMinpp)[8]) && (gdlatp[k] != missing))
                (*parmMinpp)[8] = gdlatp[k];
            if ((gdlatp[k] > (*parmMaxpp)[8]) && (gdlatp[k] != missing)) 
                (*parmMaxpp)[8] = gdlatp[k];

            if ((glonp[k] > (*parmMaxpp)[9]) && (glonp[k] != missing)) 
                (*parmMaxpp)[9] = glonp[k];
            if ((glonp[k] < (*parmMinpp)[9]) && (glonp[k] != missing))
                (*parmMinpp)[9] = glonp[k];

            if ((gdaltp[k] > (*parmMaxpp)[10]) && (gdaltp[k] != missing)) 
                (*parmMaxpp)[10] = gdaltp[k];
            if ((gdaltp[k] < (*parmMinpp)[10]) && (gdaltp[k] != missing))
                (*parmMinpp)[10] = gdaltp[k];
        }

        free (gdlatp);
        free (glonp);
        free (gdaltp);
    }
    
    /* check if any existing parameters are missing from this record */
    /* start with 11 to ignore derived parameters */
    for (j=11; j<*numParmsp; j++) 
    {
        if (!cedarHas1DParcode(cedarp, (*parmsListpp)[j]) && !cedarHas2DParcode(cedarp, (*parmsListpp)[j])) 
        {
            /* this parameter is missing */
            (*parmMissing)[j] = 1;
        }
    }

    /* Check for new 1D Parameter Codes */
    for (i=lprol; i<lprol+jpar; i++) {
        foundit = 0;
        /* start with 11 to ignore derived parameters */
        for (j=11; j<*numParmsp; j++) {
            if ((*parmsListpp)[j] == cedarp[i]) {
                foundit = 1;
                break;
            }
        }
        if (foundit == 0) {
            (*numParmsp)++;
            *parmsListpp = (int *)realloc(*parmsListpp, *numParmsp*sizeof(int));
            *parmLocpp = (int *)realloc(*parmLocpp, *numParmsp*sizeof(int));
            *parmMinpp = (double *)realloc(*parmMinpp, *numParmsp*sizeof(double));
            *parmMaxpp = (double *)realloc(*parmMaxpp, *numParmsp*sizeof(double));
            *parmMissing = (int *)realloc(*parmMissing, *numParmsp*sizeof(int));
            (*parmsListpp)[*numParmsp-1] = cedarp[i];
            (*parmLocpp)[*numParmsp-1] = 1;
            (*parmMinpp)[*numParmsp-1] = cedarGet1dParm(cedarp, cedarp[i]);
            (*parmMaxpp)[*numParmsp-1] = cedarGet1dParm(cedarp, cedarp[i]);
            /* if this is the first record, mark as not missing, otherwise it was missing from earlier records */
            if (isFirstRecord)
                (*parmMissing)[*numParmsp-1] = 0;
            else
                (*parmMissing)[*numParmsp-1] = 1;
        } else {
            if (cedarGet1dParm(cedarp, cedarp[i]) < (*parmMinpp)[j] && cedarGet1dParm(cedarp, cedarp[i]) != missing) 
                (*parmMinpp)[j] = cedarGet1dParm(cedarp, cedarp[i]);
            if (cedarGet1dParm(cedarp, cedarp[i]) > (*parmMaxpp)[j] && cedarGet1dParm(cedarp, cedarp[i]) != missing) 
                (*parmMaxpp)[j] = cedarGet1dParm(cedarp, cedarp[i]);
        }
        /* now check if any "derived" parameters need to be derived by 1D data */
        /* since cedarGetGeodetic only handles 2D data                         */
        if (nrow == 0) 
        {
	    if (cedarp[i] == 160)/* latitude */
            {
	        if ((cedarGet1dParm(cedarp, 160) != missing) && (cedarGet1dParm(cedarp, 160) < (*parmMinpp)[8]))
                    (*parmMinpp)[8] = cedarGet1dParm(cedarp, 160);
                if ((cedarGet1dParm(cedarp, 160) != missing) && (cedarGet1dParm(cedarp, 160) > (*parmMaxpp)[8]))
                    (*parmMaxpp)[8] = cedarGet1dParm(cedarp, 160);
            }
	    if (cedarp[i] == 170)/* longitude */
            {
	        if ((cedarGet1dParm(cedarp, 170) != missing) && (cedarGet1dParm(cedarp, 170) < (*parmMinpp)[9]))
                    (*parmMinpp)[9] = cedarGet1dParm(cedarp, 170);
                if ((cedarGet1dParm(cedarp, 170) != missing) && (cedarGet1dParm(cedarp, 170) > (*parmMaxpp)[9]))
                    (*parmMaxpp)[9] = cedarGet1dParm(cedarp, 170);
            }
            if (cedarp[i] == 110)/* altitude */
            {
	        if ((cedarGet1dParm(cedarp, 110) != missing) && (cedarGet1dParm(cedarp, 110) < (*parmMinpp)[10]))
                    (*parmMinpp)[10] = cedarGet1dParm(cedarp, 110);
                if ((cedarGet1dParm(cedarp, 110) != missing) && (cedarGet1dParm(cedarp, 110) > (*parmMaxpp)[10]))
                    (*parmMaxpp)[10] = cedarGet1dParm(cedarp, 110);
            }
        }
    }


    /* Check for new 2D Parameter Codes*/
    for (i=0; i<mpar; i++) {
        foundit = 0;
        code = cedarp[lprol+2*jpar+i];
        /* start with 11 to ignore derived parameters */
        for (j=11; j<*numParmsp; j++) {
            if ((*parmsListpp)[j] == code) {
                foundit = 1;
                break;
            }
        }
        val = cedarGet2dParm(cedarp, code);

        if (foundit == 0) {
            (*numParmsp)++;
            *parmsListpp = (int *)realloc(*parmsListpp, (*numParmsp)*sizeof(int));
            *parmLocpp = (int *)realloc(*parmLocpp, (*numParmsp)*sizeof(int));
            *parmMinpp = (double *)realloc(*parmMinpp, (*numParmsp)*sizeof(double));
            *parmMaxpp = (double *)realloc(*parmMaxpp, (*numParmsp)*sizeof(double));
            *parmMissing = (int *)realloc(*parmMissing, (*numParmsp)*sizeof(int));
            (*parmsListpp)[*numParmsp-1] = code;
            (*parmLocpp)[*numParmsp-1] = 2;
            (*parmMinpp)[*numParmsp-1] = 1.e38;
            (*parmMaxpp)[*numParmsp-1] = -1.e38;
            /* if this is the first record, mark as not missing, otherwise it was missing from earlier records */
            if (isFirstRecord)
                (*parmMissing)[*numParmsp-1] = 0;
            else
                (*parmMissing)[*numParmsp-1] = 1;
            for (k=0; k<nrow; k++) {
            
                /* skip rows without valid error measurements */
                if (!hasGoodData[k])
                    continue;

                    
                if ((val[k] < (*parmMinpp)[j]) & (val[k] != missing))
                    (*parmMinpp)[j] = val[k];
                if ((val[k] > (*parmMaxpp)[j]) & (val[k] != missing)) 
                    (*parmMaxpp)[j] = val[k];
            }
        } else {
            for (k=0; k<nrow; k++) {
            
                /* skip rows without valid error measurements */
                if (!hasGoodData[k])
                    continue;
                    
                if ((val[k] < (*parmMinpp)[j]) & (val[k] != missing))
                    (*parmMinpp)[j] = val[k];
                if ((val[k] > (*parmMaxpp)[j]) & (val[k] != missing)) 
                    (*parmMaxpp)[j] = val[k];
            }
        }
        /* free val for next call to cedarGet2dParm */
        free(val);
    }
    
    /* check for new values of kinst */
    kinstFound = 0;
    for (i=0; i<*numKinst; i++)
    {
        if (kinst == kinstArr[i])
        {
            kinstFound = 1;
            break;
        }
    }
    if (kinstFound == 0)
    {
       kinstArr[*numKinst] = kinst;
       *numKinst = *numKinst + 1;
    }
    
    /* check for new values of kindat */
    kindatFound = 0;
    for (i=0; i<*numKindat; i++)
    {
        if (kindat == kindatArr[i])
        {
            kindatFound = 1;
            break;
        }
    }
    if (kindatFound == 0)
    {
       kindatArr[*numKindat] = kindat;
       *numKindat = *numKindat + 1;
    }
    
    free(hasGoodData);
    
    return(0);
}


/***********************************************************************
*
*  cedarGetStationPos  Gets instrument coordinates for a given kinst
*
*      Uses data from metadata/instTab.txt
*
*/

void cedarGetStationPos(int kinst, double * lat, double * lon, double * alt)
{
    int i = 0;

    /* make sure instTab.txt has been loaded */
    if (nkinst == 0)
        cedarReadParCodes();

    for (i=0; i<nkinst; i++) {
        if (kinst == kinstList[i]) {
            *lat = kinstLat[i];
            *lon = kinstLon[i];
            *alt = kinstAlt[i];
            return;
        }
    }

    /* unknown instrument, set all to missing */
    *lat = missing;
    *lon = missing;
    *alt = missing;

}


/***********************************************************************
*
*  cedarGetStationName  Gets instrument name for a given kinst
*
*      Uses data from metadata/instTab.txt
*
*/

char * cedarGetStationName(int kinst)
{
    int i = 0;

    /* make sure instTab.txt has been loaded */
    if (nkinst == 0)
        cedarReadParCodes();

    for (i=0; i<nkinst; i++) {
        if (kinst == kinstList[i]) {
            return(kinstName[i]);
        }
    }

    /* unknown instrument, return NULL */
    return(NULL);
}


/***********************************************************************
*
* searchFilesByDate    searches the metadata for all files between
*                      starttime and endtime. 
*
*   arguments: 
*       double starttime: seconds since 1/1/1950.
*       double endtime: seconds since 1/1/1950.
*       int * numFilesFound: pointer to int, set to number of files found
*       char ** fileList: pointer to char pointer to be allocated and
*                         populated with a comma-delimited list of full
*                         paths to files found
*       double ** fileStarttime: pointer to double array to be allocated and
*                                populated with start times of each file found
*                                (number of seconds since 1/1/1950)
*       double ** fileEndtime: pointer to double array to be allocated and
*                              populated with end times of each file found
*                              (number of seconds since 1/1/1950)
*
*       To be found, the file must start after starttime and end before endtime.
*       File must also be a default file.
*
*       User must free fileList, fileStarttime, fileEndtime if numFilesFound > 0
*
*   returns: 0 if success, non-zero and error set if not successful
*
*/
int searchFilesByDate(double starttime,
                      double endtime,
                      int * numFilesFound,
                      char ** fileList,
                      double ** fileStarttime,
                      double ** fileEndtime)
{
    int i=0, j=0;
    int expID = 0;
    int fileListStrlen = 0;  /* keeps track of present len of fileList string */

    /* be sure loadExpFileTable called first */
    if(loadExpFileTable())
        return(-1);

    /* init */
    *numFilesFound = 0;
    *fileList = NULL;
    *fileStarttime = NULL;
    *fileEndtime = NULL;

    /* loop through each experiment to see if data matches */
    for (i=0; i<nExperiments; i++)
    {
        if (expStarttime[i] < starttime)
            continue;
        if (expEndtime[i] > endtime)
            continue;
        /* exp okay, find all its default files */
        expID = expIdList[i];
        for (j=0; j<nFiles; j++)
        {
            if (fileExpIDList[j] != expID)
                continue;
            if (fileCategoryList[j] != 1)
                continue;
            /* add this file */
            (*numFilesFound)++;
            fileListStrlen += strlen(fileFileNameList[j]) + strlen(expPathList[i]) + 3;
            if (*numFilesFound == 1)
            {
                *fileList = (char *)malloc(sizeof(char)*fileListStrlen);
                *fileStarttime = (double *)malloc(sizeof(double)*(*numFilesFound));
                *fileEndtime = (double *)malloc(sizeof(double)*(*numFilesFound));
            }
            else
            {
                *fileList = (char *)realloc(*fileList, sizeof(char)*fileListStrlen);
                *fileStarttime = (double *)realloc(*fileStarttime, sizeof(double)*(*numFilesFound));
                *fileEndtime = (double *)realloc(*fileEndtime, sizeof(double)*(*numFilesFound));
            }

            /* populate fileList */
            if (*numFilesFound > 1)
                strcat(*fileList, ",");
            else
                strcpy(*fileList, "");
            
            strcat(*fileList, expPathList[i]);
            strcat(*fileList, "/");
            strcat(*fileList, fileFileNameList[j]);

            (*fileStarttime)[(*numFilesFound)-1] = expStarttime[i];
            (*fileEndtime)[(*numFilesFound)-1] = expEndtime[i];
            
        }

    }

    /* success */
    return (0);
}



/***********************************************************************
*
* loadExpFileTable    Loads data from expTab.txt and fileTab.txt into
*                     global data. Private method - do not call directly.
*
*   arguments: None
*
*   returns: 0 if success, non-zero and error set if not successful
*
*   Affects: loads global data that deals with expTab and fileTab
*
*/
int loadExpFileTable()
{
    /* Data file declarations */
    FILE *fp;

    /* Miscellaneous declarations */
    char infile[128], s[1200], tmpStr[1200];
    int i=0;
    int year=0, month=0, day=0, hour=0, min=0, sec=0;
    int date=0, time=0;
    char * pToken;
    char * subStr;
    static const char *err1="loadExpFileTable error - expTab.txt does not exist";
    static const char *err2="loadExpFileTable error - fileTab.txt does not exist";
    static const char *err3="loadExpFileTable error - format error in expTab.tab";
    static const char *err4="loadExpFileTable error - format error in fileTab.tab";

    
    /* the global mutex cedar_mutex must be acquired before this method is called */
    pthread_mutex_lock(&cedar_mutex);
    /* if already loaded, just release mutex and return */
    if (nExperiments != 0)
    {
        pthread_mutex_unlock(&cedar_mutex);
        return(0);
    }
    
    /* This is the first thread to get the lock  */
    /* Load data and release mutex at the end    */

    /* now parse the expTab.txt file */

    cedarGetMadroot(infile);
    (void) strcat(infile, "/metadata/expTab.txt");
    if ((fp = fopen (infile, "r")) == NULL) 
    {
        (void) cedarSetError(err1);
        pthread_mutex_unlock(&cedar_mutex);
        return(-1);
    }

    /* Read expTab.txt File */
    while (fgets(s, 1200, fp) != NULL)
    {
        /* tokenize the line using comma as tokenizer */
        pToken = strtok(s, ",");
        if (pToken == NULL)
            continue;
        /* add new experiment */
        nExperiments++;

        /* allocate expIdList */
        if (nExperiments == 1)
            expIdList = (int *)malloc(sizeof(int)*1);
        else
            expIdList = (int *)realloc(expIdList, sizeof(int)*nExperiments);

        expIdList[nExperiments - 1] = atoi(pToken);

        /* get path from next token */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }
        
        /* alloc expPathList */
        if (nExperiments == 1)
            expPathList = (char **)malloc(sizeof(char*)*1);
        else
            expPathList = (char **)realloc(expPathList, sizeof(char*)*nExperiments);

        /* find string madtoc/ (7 chars long) */
        strcpy(tmpStr, pToken);
        subStr = strstr(tmpStr, "madtoc/");
        if (subStr == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);       
        }

        /* malloc expPathList, less 7 unneeded chars */
        expPathList[nExperiments-1] = malloc(sizeof(char)*(strlen(subStr)-5));
        strcpy(expPathList[nExperiments-1], subStr + 7);

        /* get name from next token */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* alloc expNameList */
        if (nExperiments == 1)
            expNameList = (char **)malloc(sizeof(char*)*1);
        else
            expNameList = (char **)realloc(expNameList, sizeof(char*)*nExperiments);

        /* malloc expNameList */
        expNameList[nExperiments-1] = malloc(sizeof(char)*(1+strlen(pToken)));
        strcpy(expNameList[nExperiments-1], pToken);

        /* skip next field - site id */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* get start date */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        date = atoi(pToken);

        /* get start time */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }
 
        time = atoi(pToken);

        /* convert to year, month, day */
        year = date/10000;
        date = date - year*10000;
        month = date/100;
        day = date - month*100;

        /* convert to hour, min, sec */
        hour = time/10000;
        time = time - hour*10000;
        min = time/100;
        sec = time - min*100;
        
        /* allocate expStarttime */
        if (nExperiments == 1)
            expStarttime = (double *)malloc(sizeof(double)*1);
        else
            expStarttime = (double *)realloc(expStarttime, sizeof(double)*nExperiments);

        /* set expStarttime */
        expStarttime[nExperiments - 1] = getKey(year, month, day, hour, min, sec);

        /* get end date */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        date = atoi(pToken);

        /* get end time */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }
 
        time = atoi(pToken);

        /* convert to year, month, day */
        year = date/10000;
        date = date - year*10000;
        month = date/100;
        day = date - month*100;

        /* convert to hour, min, sec */
        hour = time/10000;
        time = time - hour*10000;
        min = time/100;
        sec = time - min*100;
        
        /* allocate expEndtime */
        if (nExperiments == 1)
            expEndtime = (double *)malloc(sizeof(double)*1);
        else
            expEndtime = (double *)realloc(expEndtime, sizeof(double)*nExperiments);

        /* set expEndtime */
        expEndtime[nExperiments - 1] = getKey(year, month, day, hour, min, sec);

        /* get kinst */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err3);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* allocate expKinstList */
        if (nExperiments == 1)
            expKinstList = (int *)malloc(sizeof(int)*1);
        else
            expKinstList = (int *)realloc(expKinstList, sizeof(int)*nExperiments);

        expKinstList[nExperiments - 1] = atoi(pToken);


    }

    (void) fclose(fp);

    /* raise error if nExperiments == 0 */
    if (nExperiments == 0)
    {
        (void) cedarSetError(err3);
        pthread_mutex_unlock(&cedar_mutex);
        return(-1);
    }

    /* now parse the fileTab.txt file */

    cedarGetMadroot(infile);
    (void) strcat(infile, "/metadata/fileTab.txt");
    if ((fp = fopen (infile, "r")) == NULL) 
    {
        (void) cedarSetError(err2);
        pthread_mutex_unlock(&cedar_mutex);
        return(-1);
    }

    /* Read fileTab.txt File */
    while (fgets(s, 1200, fp) != NULL)
    {
        /* tokenize the line using comma as tokenizer */
        pToken = strtok(s, ",");
        if (pToken == NULL)
            continue;
        /* add new file */
        nFiles++;

        /* alloc fileFileNameList */
        if (nFiles == 1)
            fileFileNameList = (char **)malloc(sizeof(char*)*1);
        else
            fileFileNameList = (char **)realloc(fileFileNameList, sizeof(char*)*nFiles);

        /* malloc fileFileNameList */
        fileFileNameList[nFiles-1] = malloc(sizeof(char)*(1+strlen(pToken)));
        strcpy(fileFileNameList[nFiles-1], pToken);

        /* get expId */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err4);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* allocate fileExpIDList */
        if (nFiles == 1)
            fileExpIDList = (int *)malloc(sizeof(int)*1);
        else
            fileExpIDList = (int *)realloc(fileExpIDList, sizeof(int)*nFiles);

        fileExpIDList[nFiles - 1] = atoi(pToken);

        /* get kindat */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err4);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* allocate fileKindatList */
        if (nFiles == 1)
            fileKindatList = (int *)malloc(sizeof(int)*1);
        else
            fileKindatList = (int *)realloc(fileKindatList, sizeof(int)*nFiles);

        fileKindatList[nFiles - 1] = atoi(pToken);

        /* get category */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err4);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* allocate fileCategoryList */
        if (nFiles == 1)
            fileCategoryList = (int *)malloc(sizeof(int)*1);
        else
            fileCategoryList = (int *)realloc(fileCategoryList, sizeof(int)*nFiles);

        fileCategoryList[nFiles - 1] = atoi(pToken);

        /* skip next six tokens */
        for (i=0; i<6; i++)
        {
            pToken = strtok('\0', ",");
            if (pToken == NULL)
            {
                (void) cedarSetError(err4);
                pthread_mutex_unlock(&cedar_mutex);
                return(-1);
            }
        }

        /* get permission */
        pToken = strtok('\0', ",");
        if (pToken == NULL)
        {
            (void) cedarSetError(err4);
            pthread_mutex_unlock(&cedar_mutex);
            return(-1);
        }

        /* allocate filePermissionList */
        if (nFiles == 1)
            filePermissionList = (int *)malloc(sizeof(int)*1);
        else
            filePermissionList = (int *)realloc(filePermissionList, sizeof(int)*nFiles);

        filePermissionList[nFiles - 1] = atoi(pToken);

    }

    (void) fclose(fp);

    /* raise error if nFiles == 0 */
    if (nFiles == 0)
    {
        (void) cedarSetError(err4);
        pthread_mutex_unlock(&cedar_mutex);
        return(-1);
    }

    pthread_mutex_unlock(&cedar_mutex);

    return(0);
}


/***********************************************************************
*
* goodDataExists    Returns 1 if record contains valid data at 2d parameter
*                   index index2D.  Valid data is when the absolute value
*                   of any error parameter is not missing or knownbad.  
*                   If no error parameters, always returns 1.
*                   0 otherwise. 
*
*   arguments:
*       record pointer to Madrigal record
*       index into 2D parameter values
*
*   returns:
*       1 if 2d index contains valid data,
*       0 if not
*
*/ 
int goodDataExists(Int16 * recordp, int index2D)
{
    int i;
    int num2DParms;
    int * list2DParams;
    double errorValue;
    int errParmFound = 0;
    
    /* get number of 2D parameters */
    num2DParms = cedarGetMpar(recordp);
    
    /* get list of 2D parameters */
    list2DParams = cedarGet2dParcodes(recordp);
    
    /* loop through each 2D parameter to find a real error measurement */
    for (i = 0; i < num2DParms; i++)
    {
	
        /* check if a negitive (error) parameter, otherwise continue */
	if (list2DParams[i] >= 0)
	    continue;
	    
        errParmFound = 1;

	/* check if a valid error value */
	/* valid if not missing or knownbad */
	
        errorValue = cedarGet2dParmValue(recordp, list2DParams[i], index2D);
	
	if (errorValue == missing || errorValue == knownbad)
	    continue;
	else 
	{
	    /* good row */
            free(list2DParams);
	    return 1;
        }
    }
    free(list2DParams);

    /* if no error parameters, return 1 */
    if (errParmFound == 0) return 1;
    
    /* no valid error code found */
    return 0;
}
