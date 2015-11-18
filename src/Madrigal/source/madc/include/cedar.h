/*  $Id: cedar.h,v 1.20 2007/01/18 14:21:32 brideout Exp $ */

/* cedar definitions  */
/*				         */
/* jmh  04/26/96        original         */

#ifndef _CEDARH_
#define _CEDARH_

/* the following line is meant to be editted during installation */
#define __MAD_ROOT__ "/Users/mnicolls/Documents/Work/Madrigal"

#define missingData  -32767
#define assumedData  -32766
#define knownBadData  32767


#define missing  1.e-38
#define assumed  2.e-38
#define knownbad 3.e-38

typedef struct ffspec {
    /* Ffspec defines parameters of interest and simple range filters to apply */
    int nparms;
    int *pparms;
    int nfilters;
    int *fparms;
    double *fmin;
    double *fmax;
} Ffspec;

/* Function declarations */
void cedarGetMadroot(char * buf);
int cedarGetLtot (Int16 *cedarp);
int cedarGetKrec (Int16 *cedarp);
int isDataRecord (Int16 *cedarp);
int cedarGetKinst (Int16 *cedarp);
int cedarGetKindat (Int16 *cedarp);
int cedarGetIbyr (Int16 *cedarp); 
int cedarGetIbdt (Int16 *cedarp);
int cedarGetIbhm (Int16 *cedarp);
int cedarGetIbcs (Int16 *cedarp);
int cedarGetIeyr (Int16 *cedarp);
int cedarGetIedt (Int16 *cedarp);
int cedarGetIehm (Int16 *cedarp);
int cedarGetIecs (Int16 *cedarp);
int cedarGetLprol (Int16 *cedarp);
int cedarGetJpar (Int16 *cedarp);
int cedarGetMpar (Int16 *cedarp);
int cedarGetNrow (Int16 *cedarp);
int cedarGetKpar (Int16 *cedarp);
int cedarGetStartTime (Int16 *cedarp, int *year, int *month, int *day,
                   int *hour, int *minute, int *second, int *centisecond);
int cedarGetEndTime (Int16 *cedarp, int *year, int *month, int *day,
                 int *hour, int *minute, int *second, int *centisecond);
double cedarGetStartJday (Int16 *cedarp);
double cedarGetEndJday (Int16 *cedarp);
double cedarGetStartIndex (Int16 *cedarp);
double cedarGetEndIndex (Int16 *cedarp);
int *cedarGet1dParcodes (Int16 *cedarp);
int *cedarGet2dParcodes (Int16 *cedarp);
int cedarHas1DParcode(Int16 *cedarp, int parcode);
int cedarHas2DParcode(Int16 *cedarp, int parcode);
double cedarGet1dParm (Int16 *cedarp, int parcode);
double *cedarGet2dParm (Int16 *cedarp, int parcode);
double cedarGet2dParmValue(Int16 *cedarp, int parcode, int row);
double * cedarGetFlatParm(Int16 *cedarp, int parcode);
int hasData(int nrow, double * parp);
int *cedarGetParmCodeArray(Int16 *cedarp, Ffspec *specp, int *nlines);
double *cedarGetParmArray(Int16 *cedarp, Ffspec *specp, int *nlines);
int cedarGetGeodetic(Int16 *cedarp, double **gdlatpp, double **glonpp, double **gdaltpp);
Int16 cedarGet1dInt (Int16 *cedarp, int parcode);
Int16 *cedarGet2dInt (Int16 *cedarp, int parcode);
Int16 cedarGet2dIntValue(Int16 *cedarp, int parcode, int row);

Int16 *cedarCreateRecord(int lprol, int jpar, int mpar, int nrow,
                         int krec, int kinst, int kindat,
                         int year1, int month1, int day1,
                         int hour1, int minute1, int second1, int centisecond1,
                         int year2, int month2, int day2,
                         int hour2, int minute2, int second2, int centisecond2);
			 
Int16 *cedarCreateCatalogRecord(int kinst, int modexp,
                                int year1, int month1, int day1,
                                int hour1, int minute1, int second1, int centisecond1,
                                int year2, int month2, int day2,
                                int hour2, int minute2, int second2, int centisecond2,
				char * text);
				
Int16 *cedarCreateHeaderRecord(int kinst, int kindat,
                                int year1, int month1, int day1,
                                int hour1, int minute1, int second1, int centisecond1,
                                int year2, int month2, int day2,
                                int hour2, int minute2, int second2, int centisecond2,
				int jpar, int mpar,
				char * text);

int cedarAppendCatalogRecord(Int16 **cedarpp, char * text);
int cedarAppendHeaderRecord(Int16 **cedarpp, char * text);
int cedarSetKrec (Int16 *cedarp, int krec);
int cedarSetKinst (Int16 *cedarp, int kinst);
int cedarSetKindat (Int16 *cedarp, int kindat);
int cedarSetStartTime (Int16 *cedarp, 
                       int year, int month, int day,
                       int hour, int minute, int second, int centisecond);
int cedarSetEndTime (Int16 *cedarp, 
                     int year, int month, int day,
                     int hour, int minute, int second, int centisecond);
int cedarSet1dParm (Int16 *cedarp, 
                    int parcode, double parm, int index);
int cedarSet2dParm (Int16 *cedarp, 
                    int parcode, double *parm, int index);
int cedarSetNorm1dParm (Int16 *cedarp, 
                    int parcode, double parm, int index);
int cedarSetNorm2dParm (Int16 *cedarp, 
                    int parcode, double *parm, int index);

int cedarSet1dInt (Int16 *cedarp, int parcode, Int16 int1d, int index);
int cedarSet2dInt (Int16 *cedarp, int parcode, Int16 *int1d, int index);

int cedarPrintRecord (Int16 *cedarp);
int cedarPrintProlog (Int16 *cedarp);
char * cedarGetInformation(Int16 *cedarp);

int cedarReadParCodes (void);
int cedarGetNumParCodes (void);
int  cedarGetParCode (int position);
char *cedarGetParCodeType (int);
char * madGetParMnemType (char * mnem);
int madGetCategoryIndex (char * category);
int cedarGetParCodeIndex (int);
int madGetParMnemIndex (char * mnem);
int isMadparmError(const char * mnem);
void getStdMnem (const char * mnem, char * stdMnem);
char *cedarGetParDescription (int);
char * madGetParDescription (char * mnem);
char *cedarGetParInt16Description (int);
char * madGetParInt16Description (char * mnem);
double cedarGetParScaleFactor (int);
double madGetParScaleFactor (char * mnem);
double cedarGetNormScaleFactor (int);
double madGetNormScaleFactor (char * mnem);
char *cedarGetParUnits (int);
char * madGetParUnits (char * mnem);
char *cedarGetParMnemonic (int);
int cedarGetParCodeFromMnemonic(char * mnem);
char *cedarGetParFormat(int);
char * madGetParFormat (char * mnem);
int cedarGetParWidth(int);
int madGetParWidth (char * mnem);
int cedarHasHtmlDesc(int parcode);
int madHasHtmlDesc (char * mnem);
int cedarUpdateParmsList(Int16 *cedarp, int *numParmsp,
                         int *parmsListpp[], int *parmLocpp[],
                         double *parmMinpp[], double *parmMaxpp[], int *parmMissing[],
                         int *startJday0, 
                         double * earliestStartTime, double * latestEndTime,
                         int * numKinst, int * kinstArr,
                         int * numKindat, int * kindatArr);
int cedarCheckRecord (Int16 *cedarp);
int cedarHexPrintRecord (Int16 *cedarp);
int cedarDecimalPrintRecord (Int16 *cedarp);
int cedarSetError (const char *error);
char *cedarGetError (void);
int cedarGetWord (Int16 *cedarp, int word);
double cedarTabInt (int nt, double *xt, double *yt,
                    double x, double badval);

void cedarGetStationPos(int kinst, double * lat, double * lon, double * alt);
char * cedarGetStationName(int kinst);
int searchFilesByDate(double starttime,
                      double endtime,
                      int * numFilesFound,
                      char ** fileList,
                      double ** fileStarttime,
                      double ** fileEndtime);
int loadExpFileTable(void);
int goodDataExists(Int16 * recordp, int index2D);

#endif
