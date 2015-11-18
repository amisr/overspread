/******************************************************
*       maddata data structure definition            
*
*  This header file exposes the data structure maddata.
*  This file is the entire public interface to maddata.
*  The struct maddata is intented to provide easy
*  access to madrigal data from a single cedar file that
*  has been combined with derived parameters and has been
*  filtered.  The data is organized as follows:
*
*           Maddata           MadparmList
*         |         |
*         v         v
*      Madcycle   Madfilter
*         |
*         v
*      Madrecord
*
*  All data in this structure corresponds to Madrigal parameters,
*  and so is referenced by its unique mnemonic, not by Cedar
*  parameter codes.  All data is stored as doubles, with special
*  values as defined in cedar.h.
*
*  While this module is written in C and not C++; its methods
*  and design are as close as I could get to object-oriented.
*  Every data structure should be instantiated via a create*
*  method and released via destroy*.  All other methods take
*  the respective data structure pointer as the first argument.
*  See usage in simpleMaddata.c.
*
*  The Madrecord structure defined in this file differs from
*  the Madrec structure defined in madrec.h in that the Madrecord
*  struct does not care about the Cedar file format, or indeed
*  in what way the data is stored.  It's basic unit of data is
*  a double, not the 16 bit Int as in the Cedar format.
*
*  The derivation engine behind this interface is defined in the
*  private modules madDeriveEngine and madDeriveMethods.  Extending
*  maddata simply involves adding new methods (and possibly
*  parameters), as fully explained in madDeriveMethods.h.
*
*  See the file simpleMaddata.c for example usage.
*				                
* B. Rideout   10/11/2002        original
*
*/


#ifndef _MADDATAH_
#define _MADDATAH_


#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <cedarIO.h>
#include <madrec.h>
#include <cedar.h>
#include <date.h>
#include <madDeriveMethods.h>
#include <assert.h>

/* constants */
#define ISPRINT_MIN_CHARS_PER_LINE 50   /* the minimum value of isprint output char/line */
#define BIG_BUF  5000                   /* the size of a large buffer for string manipulation */

/* used to identify parameter types */
typedef enum {
    UNDETERMINED_PARM,
    MEASURED_PARM,
    DERIVED_PARM
} Parm_type;


/* used to identify whether parameter is standard or error */
typedef enum {
    STANDARD_MNEM,
    ERROR_MNEM
}Error_type;


/* used to identify filter types */
typedef enum {
    SINGLE_FILT,
    MULT_FILT,
    DIV_FILT,
    ADD_FILT,
    SUB_FILT
} Filter_type;


/* used to identify record types */
typedef enum {
    HEADER_REC,
    CATALOG_REC,
    DATA_REC
} Rec_type;


typedef struct madparmList {

    /* this structure holds a list of Madrigal parameters */

    int          numParm;          /* Number of parameters in this list                                               */
    Parm_type  * typeParmList;     /* array of Parm_type enum values MEASURED_PARM, DERIVED_PARM or UNDETERMINED_PARM */
    Error_type * errParmList;      /* array of Error_type enum values STANDARD_MNEM or ERROR_MNEM                     */
    char      ** mnemList;         /* Array (len = numParm) of mnemonic strings in this list                          */
    double     * minList;          /* Array (len = numParm) of minimum values of parameters - if unknown, = missing   */
    double     * maxList;          /* Array (len = numParm) of maximum values of parameters - if unknown, = missing   */

} MadparmList;


typedef struct madrecParmType {

    /* this structure holds 2 parm lists, 1D and 2D, and defines a record type */

    MadparmList * parm1DList;     /* List of 1D parameters */
    MadparmList * parm2DList;     /* List of 2D parameters */

} MadrecParmType;


typedef struct madrecord {

    /* this structure holds all data for a single madrecord  */


    Rec_type  rectype;           /* Record type: HEADER_REC, CATALOG_REC, or DATA_REC.  If DATA_REC,         */
                                 /* text will be empty string. If not, all 1D parms will be missing and      */
                                 /* num2Drows will be zero.                                                  */
    char   *  text;              /* Text of header or catalog record. Empty string if data rec.              */
    int       numType;           /* index into maddata.madrecParmTypeList that defines the parm type of      */
                                 /* record (that is, its list of 1D and 2D parameters)                       */
    double *  data1Dparms;       /* pointer to array of doubles containing 1D data,                          */
                                 /*  len = maddata.madrecParmTypeList[numType].parm1DList.numParm            */
    int       num2Drows;         /* number of 2D rows in this record (i.e., number of values per 2D parm)    */
    double ** data2Dparms;       /* pointer to array (len = num2Drows) of arrays of doubles                  */
                                 /* containing 2D data                                                       */
                                 /*  len each array = maddata.madrecParmTypeList[numType].parm2DList.numParm */
    int       kinst;             /* kind of instrument id                                                    */
    double    starttime;         /* start time of record in seconds since 1/1/1950                           */
    double    endtime;           /* end time of record in seconds since 1/1/1950                             */

} Madrecord;


typedef struct madcycle {

    /* this structure holds all data for a single madcycle */

    int          cyclenum;          /* cycle number (starts at 0)                    */
    int          cycleId;           /* cycle id                                      */
    char *       cycleDesc;         /* Additional cycle description                  */
    int          numMadrecords;     /* Number of Madrecords in cycle after filtering */
    Madrecord ** madRecordList;     /* Array of Madrecord *s, len = numMadrecords    */

} Madcycle;



typedef struct madfilter {

    /* this structure holds all data for a single madfilter          */
    /* Filters are based on one of the following                     */
    /* being within a set of one or more  ranges:                    */
    /*     1. MadParm1              (SINGLE_FILT)                    */
    /*     2. MadParm1 * Madparm2   (MULT_FILT)                      */
    /*     3. MadParm1 / Madparm2   (DIV_FILT)                       */
    /*     4. MadParm1 + Madparm2   (ADD_FILT)                       */
    /*     5. MadParm1 - Madparm2   (SUB_FILT)                       */
    /*                                                               */
    /*  Filters can use any Madrigal parameter, measured or derived  */
    /*                                                               */
    /*  If MadParm1 or MadParm2 = missing, filter always rejects     */
    /*  result.                                                      */
    /*                                                               */
    /*  If all parameters are 1D in filter, filter rejection rejects */
    /*  the entire record.  Otherwise, rejects only 2D data row.  If */
    /*  a record has 2D data, and all 2D rows are rejected by        */
    /*  filters, entire record is rejected.                          */

    Filter_type    filtType;    /* identifies the type of filter listed above          */
    int            numRange;    /* number of ranges - must be at least one             */
    double *       lower;       /* array of length numRange of lower limit of range    */
                                /*   if "missing", no lower limit for that range       */
    double *       upper;       /* array of length numRange of upper limit of range    */
                                /*   if "missing", no upper limit for that range       */
    char *         madParm1;    /* Mnemonic of first parameter - cannot be 0 length    */
    char *         madParm2;    /* Mnemonic of second parameter - can be 0 length      */

} Madfilter;


typedef struct madfilterlist {

    /* holds a list of madfilters */

    int         numFilters;     /* number of Madfilters in list          */
    Madfilter * madfilt_list;   /* array of Madfilters, len = numFilters */

} MadfilterList;


typedef struct maddata {

    /* this structure holds all data for exposed via maddata */
    /* This is the main data structure, and is meant to be   */
    /* the main way to expose Madrigal data from a single    */
    /* cedar file that contains filtering and derived data   */

    char *          filename;           /* full path the file which was basis of data                   */
    char *          infoStr;            /* Information string (may be used in outputing formatted data) */
    MadparmList   * requestParmList;    /* List of parameters requested                                 */
    MadfilterList * madFiltList;        /* list of Madfilters to apply                                  */
    int             numCycles;          /* number of Madcycles in Maddata                               */
    Madcycle **     madCycleList;       /* Array of Madcycle pointers, len = numCycles                  */
    int             numTypes;           /* number of record types in file                               */
    MadrecParmType* madrecParmTypeList; /* Array of MadrecParmType that lists all record types in this  */
                                        /* file.  A record type is a unique combination of 1D and 2D    */
                                        /* parameters.  Len = numTypes                                  */

} Maddata;


/* Method declarations */

    /* Parameter handling methods */

MadparmList * createMadparmList(void);

MadparmList * copyMadparmList(MadparmList * madparmList);

void destroyMadparmList(MadparmList * madparmList);

int appendMadparm(MadparmList * madparmList, const char * mnem);

int hasParm(MadparmList * madparmList, const char * mnem);

int isErrorParm(MadparmList * madparmList, int index);

int getIndex(MadparmList * madparmList, const char * mnem);

double getMinParm(MadparmList * madparmList, char * mnem);

double getMaxParm(MadparmList * madparmList, char * mnem);

int analyzeFileParms(char * filename, 
                     MadparmList ** list1DMeasParms,
                     MadparmList ** list2DMeasParms,
                     MadparmList ** list1DDervParms,
                     MadparmList ** list2DDervParms,
                     FILE * errFile);
                     
MadparmList * getDerivedParms(MadparmList * listMeasParms);

    /* Filter handling methods */

MadfilterList * createMadfilterList(void);

void destroyMadfilterList(MadfilterList * madfilt_list);

int appendMadfilter(MadfilterList * madfilt_list,
                    Filter_type filtType,
                    int numRange, 
                    double * lower, 
                    double * upper, 
                    char * madParm1,
                    char * madParm2);

MadfilterList * copyMadfilterList(MadfilterList * madfilterList);

    /* Maddata methods */

Maddata * createMaddata(char * filename,
                        char * infoStr,
                        MadparmList * requestParmList,
                        MadfilterList * madfilterList,
                        FILE * errFile);
                        

Maddata * createNonfileMaddata(MadparmList * requestedParms,
                               double ut1,
                               double ut2,
                               int kinst,
                               MadparmList * oneDParms,
                               MadparmList * twoDParms,
                               int num2Drows,
                               double * oneDdata,
                               double ** twoDdata,
                               FILE * errFile);

void destroyMaddata(Maddata * maddata);

int appendMadrecParmType(Maddata * maddata,
                         MadparmList * parm1DList,
                         MadparmList * parm2DList);

int appendMadcycle(Maddata * maddata,
                   int cycleId,
                   char * cycleDesc);


    /* Madcycle methods */

Madcycle * createMadcycle(int cyclenum,
                          int cycleId,
                          char * cycleDesc);
void destroyMadcycle(Madcycle * madcycle);


    /* Madrecord methods */

Madrecord * createMadrecord(Rec_type rectype,
                            int numType,
                            char * text,
                            int num1DParms,
                            double * data1Dparms,
                            int kinst,
                            double starttime,
                            double endtime);
                            
void destroyMadrecord(Madrecord * madrecord);

    /* Formatting methods */
    
void simpleMadrecordPrint(Maddata * maddata,
                          int cycId,
                          int recId,
                          FILE * fp);
                          
void simpleMadfilterPrint(Madfilter * madfilter, int filterNum, FILE * fp);

MadfilterList * getMadfilterListFromStr(char * str);

void simpleMaddataPrint(Maddata * maddata, FILE * fp);

void classicIsprint(Maddata * maddata,
                    int displayHeaders,
                    int displaySummary,
                    int maxCharsPerLine,
                    char * missingStr,
                    char * assumedStr,
                    char * knownBadStr,
                    FILE * fp);
                    
void printIsprintHeader(Maddata * maddata, 
                        int cycleNum,
                        int recNum, 
                        FILE * fp);

char * getIsprintHeader(Maddata * maddata, 
                        int cycleNum,
                        int recNum);
                    
void printIsprintLabel(Maddata * maddata, 
                        int maxCharsPerLine, 
                        FILE * fp);

void getIsprintLabel(Maddata * maddata, 
                     int maxCharsPerLine,
                     char ** mnemStr,
                     char ** labelStr);
                        
void classicMadrecordPrint(Maddata * maddata, 
                           int cycleNum, 
                           int recNum, 
                           int displayHeaders,
                           int maxCharsPerLine,
                           char * missingStr,
                           char * assumedStr,
                           char * knownBadStr,
                           FILE * fp);
                           
 void getClassicMadrecordStrings(Maddata * maddata, 
                                int cycleNum, 
                                int recNum, 
                                int maxCharsPerLine,
                                char * missingStr,
                                char * assumedStr,
                                char * knownBadStr,
                                char ** headerStr,
                                char ** mnemStr,
                                char ** labelStr,
                                char ** dataStr);
                                
void lookerMadrecordPrint(Maddata * maddata, 
                          char * missingStr,
                          char * assumedStr,
                          char * knownBadStr,
			  int printHeaderFlag,
                          FILE * fp);
                          
/* parsing methods */

int populate1DDataFromStr(char * onedString, 
                          MadparmList ** oneDParms, 
                          double ** oneDdata);

int populate2DDataFromStr(char * twodString, 
                          MadparmList ** twoDParms, 
                          double *** twoDdata, 
                          int * num2Drows);

/* more to be added - xml, etc */

    /* Extention methods */

int registerCalcUrl(char * url,
                    char * methodName,
                    MadparmList * inputParms,
                    MadparmList * outputParms);


#endif
