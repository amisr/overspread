/* $Id: maddataF77.c,v 1.13 2009/01/06 21:47:55 brideout Exp $ */

/************************************************************************************************
      
      Fortran callable C routines for Fortran access to the C-language
      Madrigal API - Maddata module.
      
      These comments show how to call these methods from Fortran.
 
      To build, use -L$MADROOT/lib -lmadrec -lgeo
       See example program source/madc/testF77/tmaddataF77.f
      ________________________________________________________________________
      
      SUBROUTINE CRMADD(FILEC,
     *                  PARMS,
     *                  FLTSTR,
     *                  RFMADD,
     *                  NUMREC,
     *                  STATUS)
      CHARACTER FILEC*(*)
      CHARACTER PARMS*(*)
      CHARACTER FLTSTR*(*) 
      INTEGER RFMADD, NUMREC, STATUS
      (for 64 bit machines, use INTEGER*8 RFMADD)
C
      CRMADD  Creates Maddata given a file, a list of desired parameters,
      and filters.   
      CRMADD is meant to stand for "CReate MADData"
C            
      Input parameters: 
      FILEC - file name 
      PARMS - comma delimited list of desired parameters
              (not case-sensitive)  Example PARMS: "gdalt,Azm,F10.7,PH+,Fof2"
      FLTSTR - The filter string is the same string that is used in the new isprint
       command line.  Filters are separated by spaces.  The allowed filters
       are:  
C
          date1=mm/dd/yyyy  (starting date to be examined. If time1 not given, defaults to 0 UT.)
             Example: date1=01/20/1998 
C
          time1=hh:mm:ss (starting UT time to be examined. If date1 given, is applied to date1.
                          If not, applies on the first day of the experiment.)
             Example: time1=13:30:00
C
          date2=mm/dd/yyyy (ending date to be examined.  If time2 not given, defaults to 0 UT.)
             Example: date2=01/21/1998
C
          time2=hh:mm:ss (ending UT time to be examined - If date2 not given, ignored.)
             Example: time2=15:45:00
C
          In the follow arguments ranges are used.  If any range value is not given, it may be used to 
          indicate no lower or upper limit (but the comma is always required). Ranges are inclusive
          of the end points:
C
          z=lower alt limit1, upper alt limit1 [or lower alt limit2 , upper alt limit2 ...] (km)
             Example 1: z=100,500  (This would limit the geodetic altitude to 100 to 500 km.)
             Example 2: z=100,200or300,400  (This would limit the geodetic altitude to 100 to 200 km
                                             or 300 to 400 km.)
             Example 3: z=,200or300,400   (Since the lower limit of the first range is missing, this 
                                           would limit the geodetic altitude to anything below 200 km 
                                           or from 300 to 400 km.)
C
          az=lower az limit1, upper az limit1 [or lower az limit2 , upper az limit2 ...] (from -180 to 180 degrees)
             Example 1: az=100,120  (This would limit the azimuth to 100 to 120 degrees.)
             Example 2: az=-180,-90or90,180  (This would limit the azimuth to between -180 and -90 degrees or
                                             to between 90 and 180 degrees.  Note this allows a filter to go
                                             through 180 degrees.)
C
          el=lower el limit1, upper el limit1 [or lower el limit2 , upper el limit2 ...] (from 0 to 90) 
             Example 1: el=0,45  (This would limit the elevation from 0 to 45 degrees.) 
  
          plen=lower pl limit1, upper pl limit1 [or lower pl limit2 , upper pl limit2 ...] (pulse len in sec)
             Example 1: el=,5e-4  (This would limit the pulse length to 5e-4 seconds or less.)
C
C
          Free form filters using any mnemonic, or two mnemonics added, subtracted, multiplied, or divided.
          Any number of filters may be added: 
C
          filter=[mnemonic] or [mnemonic1,[+*-/]mnemonic2] , lower limit1 , upper limit1 [or lower limit2 , upper limit2 ...] 
             Example 1: filter=ti,500,1000or2000,3000   (Limits the data to points where Ti is between 500 and 1000 degrees
                                                       or between 2000 and 3000 degrees.  Note that the units are always
                                                       those of the Cedar standard.)
             Example 2: filter=gdalt,-,sdwht,0,    (This filter implies "gdalt - sdwht" must be greater than 0.0.  Since
                                                    sdwht is shadow height - the distance above any point on the earth 
                                                    where the sun is first visible - this filter implies that only data 
                                                    in direct sunlight will be displayed.)
             Example 3: filter=ti,/,Dti,100,   (Limits the data to points where the ratio Ti/dTi is more than 100.)
C
          So an full FLTSTR argument might be:
C 
             "date1=01/20/1998 time1=13:30:00 z=,200or300,400 filter=gdalt,-,sdwht,0, filter=ti,/,Dti,100," 
C   
      Output parameters:  
      RFMADD - a long integer reference to the Maddata created, 
      used by all other methods.  Will return 0 if error  
      occurs.  (for 64 bit machines, use INTEGER*8 RFMADD) 
      NUMREC - number of records in Maddata
      STATUS - 0 if success, -1 if failure  

      ***************************************************************
      
      SUBROUTINE CRNFMD(PARMS,
     *                  UT,
     *                  KINST,
     *                  ONED,
     *                  TWOD,
     *                  RFMADD,
     *                  NROW)
      CHARACTER PARMS*(*)
      DOUBLE PRECISION UT
      CHARACTER ONED*(*) 
      CHARACTER TWOD*(*)
      INTEGER KINST, RFMADD, NROW
C     (for 64 bit machines, use INTEGER*8 RFMADD)
C
      CRNFMD  Creates a Maddata record given some input data (no file needed) for a set time.   
      CRNFMD is meant to stand for "CReate NonFile MadData"
C            
      Input parameters: 
C
      PARMS - comma delimited list of desired parameters
              (not case-sensitive)  Example PARMS: "gdalt,Azm,F10.7,PH+,Fof2"
C
      UT - seconds since 1/1/1950 to calculate data at
C
      KINST - instrument id.  If not important, use 0    
C
      ONED - A string that sets one dimension data (That is, one value per parameter).  For example, 
             "gdalt=100.0 glon=45.0 gdlat=-20.0"   
C
      TWOD - A string that sets two dimension data (That is, NROW values per parameter, where every
             parameter must have the same number of values, if more than one).  For example, the
             follow TWOD string would produce 8 rows with every combination of gdlat = 45 or 50,
             glon = 20 or 30, and gdalt = 500 or 600:
C
             "gdlat=45,45,45,45,50,50,50,50 glon=20,20,30,30,20,20,30,30 gdalt=500,600,500,600,500,600,500,600"
C
      Output parameters:
C   
      RFMADD - a long integer reference to the Maddata created, 
      used by all other methods.  Will return 0 if error  
      occurs.  (for 64 bit machines, use INTEGER*8 RFMADD)
C
      NROW - number of rows in Record returned.  If TWOD string used, should be equal to the number of data points
             for each parameter.  If no TWOD data, should be 1.  If error, will be 0.
C
      ***************************************************************
 
 
      SUBROUTINE GTNROW(RFMADD, RECNUM, NROW)
      INTEGER RFMADD
      INTEGER RECNUM, NROW
      (for 64 bit machines, use INTEGER*8 RFMADD)
C 
      GTNROW  Gets the number of rows of data in record RECNUM.   
      GTNROW is meant to stand for "GeT Number of ROWs"
C              
      Input parameters: 
      RFMADD - reference to data created by CRMADD 
      RECNUM - record number of interest (starts at 1)
C 
      Output parameter:
      NROW - number of rows of data.  If only 1D data requested, will
      always be 1.

 
      ***************************************************************
 
      SUBROUTINE GTMADD(RFMADD, RECNUM, NROW, DATROW, STATUS)
      INTEGER RFMADD
      (for 64 bit machines, use INTEGER*8 RFMADD)
      INTEGER RECNUM
      INTEGER NROW, STATUS
      DOUBLE PRECISION DATROW(*)
C 
      GTMADD  Gets one row of data via the DATROW array.   
      GTMADD is meant to stand for "GeT MADData"
C 
      Input parameters: 
      RFMADD - reference to data created by CRMADD 
      RECNUM - record number of interest (starts at 1)
      NROW - row number of interest (starts at 1)
C 
      Output parameters:
      DATROW - Array of doubles to be filled with data.  Order of
      data is the same as the order of parameters in parms string
      passed into CRMADD.  User must be sure array size is equal to
      (or larger than) number of parameters requested.
C
      STATUS - 0 if success, -1 if failure

 
      ***************************************************************
 
 
      SUBROUTINE FRMADD(RFMADD)
      INTEGER RFMADD
      (for 64 bit machines, use INTEGER*8 RFMADD)
C 
      FRMADD  Frees the memory allocated by CRMADD.   
      FRMADD is meant to stand for "FRee MADData"
C 
      Input parameters: 
      RFMADD - reference to data created by CRMADD 
C 
      This method should be called if your program wants to call
      CRMADD more than once.  Call FRMADD just before CRMADD to
      avoid a memory leak.  Will set RFMADD = 0. (for 64 bit machines, use INTEGER*8 RFMADD)
      
      ***************************************************************
      
      
      SUBROUTINE STALOC(KINST,SLATGD,SLON,SALTGD,SLATGC,SR,ISTAT)
      INTEGER KINST,ISTAT
      DOUBLE PRECISION SLATGD,SLON,SALTGD,SLATGC,SR
      
C 
      STALOC  Returns location of a given instrument (kinst)   
      STALOC is meant to stand for "STAtion LOCation"
C 
      Input parameters: 
      KINST - instrument id 
C 
      Output parameters:
      SLATGD - instrument geodetic latitude
      SLON - instrument longitude (0 to 360)
      SALTGD - instrument geodetic altitude (km)
      SLATGC- instrument geocentric latitude
      SR - distance from center of earth (km)
      ISTAT - 0 if successful, -1 if not

*****************************************************************************/



#include <stdlib.h>
#include <stdio.h>
#define _REENTRANT
#include <string.h>
#include <maddata.h>


/* prototypes */
int copyF77String(char * str, int len, char * buf, int bufsize);

int crmadd_(char *filec, char *parms, char *fltstr, long int *rfmadd, 
	int *numrec, int *status, short filec_len, short parms_len, 
	short fltstr_len);
	
int crnfmd_(char *parms, double *ut, int *kinst, 
        char *oned, char *twod, long int *rfmadd, int *nrow, 
        short parmc_len, short oned_len, short twod_len);
	
int gtnrow_(long int *rfmadd, int *recnum, int *nrow);

int gtmadd_(long int *rfmadd, int *recnum, int *nrow, double *
	datrow, int *status);

int frmadd_(long int *rfmadd);

int staloc_(int * kinst, 
            double * slatgd,
	    double * slon,
	    double * saltgd,
	    double * slatgc,
	    double * sr,
	    int * istat);

/* implementations */

int crmadd_(char *filec, char *parms, char *fltstr, long int *rfmadd, 
	int *numrec, int *status, short filec_len, short parms_len, 
	short fltstr_len)
{
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    
    /* real C Strings to hold arguments from Fortran */
    char * fileString = NULL;
    char * parmsString = NULL;
    char * fltString = NULL;
    
    MadparmList * desiredParms = NULL;
    MadfilterList * madFiltList = NULL;
    Maddata * maddata = NULL;
    char * pToken = NULL;
    char * restrict1 = NULL;
    int i = 0;
    
    /* malloc three strings to copy from fortran strings */
    fileString = (char *)malloc(sizeof(char) * (filec_len + 1));
    parmsString = (char *)malloc(sizeof(char) * (parms_len + 1));
    fltString = (char *)malloc(sizeof(char) * (fltstr_len + 1));

    /* create 3 real C strings from three input strings */
    copyF77String(filec, (int)filec_len, fileString, (int)(filec_len+1));
    copyF77String(parms, (int)parms_len, parmsString, (int)(parms_len+1));
    copyF77String(fltstr, (int)fltstr_len, fltString, (int)(fltstr_len+1));
    
    
    /* try to create list of desired parameters from parmsString */
    desiredParms = createMadparmList();
    
    /* parse parmsString */
    pToken = (char *)strtok_r(parmsString, ",", &restrict1);
    
    while (pToken)
    {      
        if (appendMadparm(desiredParms, pToken) == -1)
        {
            fprintf(stderr, "Illegal parameter %s in PARMS\n", pToken);
            destroyMadparmList(desiredParms);
            free(fileString);
            free(parmsString);
            free(fltString);
            *status = -1;
            *rfmadd = 0;
            return(0);
        }
        
        pToken = (char *)strtok_r('\0', ",", &restrict1);
    }
        
    madFiltList = getMadfilterListFromStr(fltString);
    
    if (madFiltList == NULL)
    {
       fprintf(stderr, "\nIllegal FLTSTR argument\n");
       destroyMadparmList(desiredParms);
       free(fileString);
       free(parmsString);
       free(fltString);
       *rfmadd = 0;
       *status = -1;
       return(0);
    }
    
    maddata = createMaddata(fileString,
                            "",
                            desiredParms,
                            madFiltList,
                            stderr);
                            
    if (maddata == NULL)
    {
       fprintf(stderr, "\nProblem creating maddata with %s\n", fileString);
       destroyMadparmList(desiredParms);
       destroyMadfilterList(madFiltList);
       free(fileString);
       free(parmsString);
       free(fltString);
       *status = -1;
       *rfmadd = 0;
       return(0);
    }
                            
    /* set rfmadd to address of maddata */
    *rfmadd = (long int)maddata;
    
    /* get number of records in maddata */
    *numrec = 0;
    for (i=0; i<maddata->numCycles; i++)
        *numrec += maddata->madCycleList[i]->numMadrecords;
        
    /* free unneeded data structures */
    destroyMadparmList(desiredParms);
    destroyMadfilterList(madFiltList);
    free(fileString);
    free(parmsString);
    free(fltString);

    *status = 0;
    return 0;
} /* crmadd_ */




int crnfmd_(char *parms, double *ut, int *kinst, 
        char *oned, char *twod, long int *rfmadd, int *nrow, 
        short parms_len, short oned_len, short twod_len)
{
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    
    /* real C Strings to hold arguments from Fortran */
    char * parmsString = NULL;
    char * onedString = NULL;
    char * twodString = NULL;
    
    MadparmList * desiredParms = NULL;
    Maddata * maddata = NULL;
    char * pToken = NULL;
    char * restrict1 = NULL;
    int i = 0;
    
    /* data structures to hold 1 and 2 D data */
    MadparmList * oneDParms = NULL;
    MadparmList * twoDParms = NULL;
    double * oneDdata = NULL;
    double ** twoDdata = NULL;
    int num2Drows = 0;
    
    /* malloc three strings to copy from fortran strings */
    parmsString = (char *)malloc(sizeof(char) * (int)(parms_len + 1));
    onedString = (char *)malloc(sizeof(char) * (int)(oned_len + 1));
    twodString = (char *)malloc(sizeof(char) * (int)(twod_len + 1));
    
    /* create 3 real C strings from three input strings */
    copyF77String(parms, (int)parms_len, parmsString, (int)(parms_len+1));

    copyF77String(oned, (int)oned_len, onedString, (int)(oned_len+1));

    copyF77String(twod, (int)twod_len, twodString, (int)(twod_len+1));
    
    /* try to create list of desired parameters from parmsString */
    desiredParms = createMadparmList();
    
    /* parse parmsString */
    pToken = (char *)strtok_r(parmsString, ",", &restrict1);
    
    while (pToken)
    {      
        if (appendMadparm(desiredParms, pToken) == -1)
        {
            fprintf(stderr, "Illegal parameter %s in PARMS\n", pToken);
            destroyMadparmList(desiredParms);
            free(parmsString);
            free(onedString);
            free(twodString);
            *nrow = 0;
            *rfmadd = 0;
            return(0);
        }
        
        pToken = (char *)strtok_r('\0', ",", &restrict1);
    }
    
    /* populate 1D parameters and data from oned string */
    if (populate1DDataFromStr(onedString, &oneDParms, &oneDdata) == -1)
    {
        fprintf(stderr, "Error found in 1D string: %s\n", onedString);
        destroyMadparmList(desiredParms);
        free(parmsString);
        free(onedString);
        free(twodString);
        *nrow = 0;
        *rfmadd = 0;
        return(0);
    }
    
    /* populate 2D parameters and data from twod string */
    if (populate2DDataFromStr(twodString, &twoDParms, &twoDdata, &num2Drows) == -1)
    {
        fprintf(stderr, "Error found in 2D string: %s\n", twodString);
        destroyMadparmList(oneDParms);
        destroyMadparmList(desiredParms);
        free(parmsString);
        free(onedString);
        free(twodString);
        *nrow = 0;
        *rfmadd = 0;
        return(0);
    }
    
    /* create the data you want */
    maddata = createNonfileMaddata(desiredParms,
                                   *ut,
                                   *ut,
                                   *kinst,
                                   oneDParms,
                                   twoDParms,
                                   num2Drows,
                                   oneDdata,
                                   twoDdata,
                                   stderr);
                                                                 
    if (maddata == NULL)
    {
       fprintf(stderr, "\nProblem creating maddata\n");
       destroyMadparmList(desiredParms);
       destroyMadparmList(oneDParms);
       destroyMadparmList(twoDParms);
       free(parmsString);
       free(onedString);
       free(twodString);
       free(oneDdata);
       for (i=0; i<twoDParms->numParm; i++)
           free(twoDdata[i]);
       free (twoDdata);
       *nrow = 0;
       *rfmadd = 0;
       return(0);
    }
                            
    /* set rfmadd to address of maddata, and set nrow */
    *rfmadd = (long int)maddata;
    if (num2Drows == 0)
        *nrow = 1;
    else
        *nrow = num2Drows;
    
    /* free unneeded data structures */
    if (oneDdata != NULL)
        free(oneDdata);
        
    for (i=0; i<twoDParms->numParm; i++)
        free(twoDdata[i]);
    if (twoDdata != NULL)
        free (twoDdata);
    destroyMadparmList(desiredParms);
    destroyMadparmList(oneDParms);
    destroyMadparmList(twoDParms);
    free(parmsString);
    free(onedString);
    free(twodString);
    
    return 0;
}



int gtnrow_(long int *rfmadd, int *recnum, int *nrow)
{
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    
    int i = 0;
    int cArrRecNum = *recnum - 1;  /* C arrays start at 0, not 1 */
    int recSoFar = 0;
    int recThisCycle = 0;
    int cycNum = -1;
    int recNumInCyc = -1;
    Maddata * maddata = NULL;
    
    maddata = (Maddata *)*rfmadd;
    
    /* check that recnum > 0 */
    if (*recnum < 1)
    {
        fprintf(stderr, "Record number %i not legal\n", (int)*recnum);
        *nrow = -1;
        return (0);
    }
    
    /* first, find the right cycle */
    recSoFar = 0;
    for (i=0; i<maddata->numCycles; i++)
    {
        recThisCycle = maddata->madCycleList[i]->numMadrecords;
        if (cArrRecNum >= recSoFar && cArrRecNum < (recSoFar + recThisCycle))
        {
            cycNum = i;
            recNumInCyc = cArrRecNum - recSoFar;
            break;
        }
        recSoFar += recThisCycle;
    }
    
    if (cycNum == -1)
    {
        /* bad record number */
        fprintf(stderr, "Record number %i not found\n", (int)*recnum);
        *nrow = -1;
        return (0);
    }
    
    *nrow = (int)(maddata->madCycleList[cycNum]->madRecordList[recNumInCyc]->num2Drows);
    
    /* If there'e only 1D data, return 1 for 1 row */
    if (*nrow == 0)
        *nrow = 1;

    return 0;
} /* gtnrow_ */



int gtmadd_(long int *rfmadd, int *recnum, int *nrow, double *
	datrow, int *status)
{    
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    
    int i = 0;
    int cArrRecNum = *recnum - 1;  /* C arrays start at 0, not 1 */
    int cArrNrow = *nrow - 1;  /* C arrays start at 0, not 1 */
    int recSoFar = 0;
    int recThisCycle = 0;
    int cycNum = -1;
    int recNumInCyc = -1;
    int numParms = 0;
    int recTypeIndex = 0;
    Maddata * maddata = NULL;
    Madrecord * thisRecord = NULL;
    int * locationArr = NULL;  /* an array of int, len = 2*numParm, where each parm has  */
                               /* 2 ints, first is dimension (1 or 2), 2nd is index into */
                               /* data array for that parameter                          */
      
    /* Parameter adjustments */
    --datrow;
        
    /* check that recnum > 0 */
    if (*recnum < 1)
    {
        fprintf(stderr, "Record number %i not legal\n", (int)*recnum);
        *status = -1;
        return (0);
    }
    
    /* check that nrow > 0 */
    if (*nrow < 1)
    {
        fprintf(stderr, "Row number %i not legal\n", (int)*nrow);
        *status = -1;
        return (0);
    }
    
    maddata = (Maddata *)*rfmadd;
    
    /* initialize all data to missing */
    for (i=1; i<=maddata->requestParmList->numParm; i++)
        datrow[i] = missing;
    
    /* if no parameters selected, return */
    numParms = maddata->requestParmList->numParm;
    if (numParms == 0)
        return 0;
    
    /* first, find the right cycle */
    recSoFar = 0;
    for (i=0; i<maddata->numCycles; i++)
    {
        recThisCycle = maddata->madCycleList[i]->numMadrecords;
        if (cArrRecNum >= recSoFar && cArrRecNum < (recSoFar + recThisCycle))
        {
            cycNum = i;
            recNumInCyc = cArrRecNum - recSoFar;
            break;
        }
        recSoFar += recThisCycle;
    }
    
    if (cycNum == -1)
    {
        /* bad record number */
        fprintf(stderr, "Record number %i not found\n", (int)*recnum);
        *status = -1;
        return (0);
    }
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[cycNum]->madRecordList[recNumInCyc]; 
    
    /* malloc locationArr */
    locationArr = (int   *)malloc(sizeof(int)*2*numParms);
    if (locationArr == NULL)
    {
        perror("malloc");
        exit(-1);
    }
        
    /* get record type of this Madrecord */
    recTypeIndex =  thisRecord->numType;
    
    /* populate locationArr */
    for (i=0; i<numParms; i++)
    {
       
       if (hasParm(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                   maddata->requestParmList->mnemList[i]))
       {
           /* its a 1D parameter */
           locationArr[2*i] = 1;
           locationArr[2*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[2*i + 1] != -1);
       }
       else
       {
           /* its a 2D parameter */
           locationArr[2*i] = 2;
           locationArr[2*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm2DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[2*i + 1] != -1);
       }
    }
    
    /* loop through each parameter and set datrow */
    for (i=0; i < numParms; i++)
    {
        if (locationArr[2*i] == 1) /* 1D data */
            datrow[i+1] = thisRecord->data1Dparms[locationArr[2*i + 1]];
        else   /* 2D data */
            datrow[i+1] = thisRecord->data2Dparms[cArrNrow][locationArr[2*i + 1]];
    }

    /* free temp memory */
    free(locationArr);

    *status = 0;
    return 0;
} /* gtmadd_ */


int frmadd_(long int *rfmadd)
{
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    
    Maddata * maddata = NULL;
    
    if (*rfmadd == 0)
        return 0;
    
    maddata = (Maddata *)*rfmadd;
    destroyMaddata(maddata);
    
    *rfmadd = 0;
    
    return 0;
} /* frmadd_ */


int staloc_(int * kinst, 
            double * slatgd,
	    double * slon,
	    double * saltgd,
	    double * slatgc,
	    double * sr,
	    int * istat)
{
    /* meant to be called from Fortran - see Fortran comments above for documentation */
    int direction = 1; /* geodetic to geocentric */
    int intKinst = 0;
    
    intKinst = (int)(*kinst);
    
    cedarGetStationPos(intKinst, slatgd, slon, saltgd);
    if (*slatgd == missing)
    {
        *istat = -1;
        return(-1);
    }
	
    CONVRT_F77(&direction,slatgd,saltgd,slatgc,sr);
    *istat = 0;
    
    return 0;
} /* staloc_ */


/*  HELPER METHODS */



/***********************************************************************
*
* copyF77String - copies Fortran style string into buffer.  Puts NULL
*       after last space in Fortran string if it ends with a space,
*       otherwise append NULL to the end of the string.  Returns 0
*       if success, -1 if buffer too small
*
*   arguments: 
*      char * str - a Fortran string, terminated with spaces 
*      int len - length of Fortran string (charater array)
*      char * buf - buffer to copy string into
*      int bufsize - size of buffer
*
*
*   returns - 0 if success, -1 BufSize too small.
*/
int copyF77String(char * str, int len, char * buf, int bufsize)
{
    int i=0, j=0;
    
    /* find last non-space char */    
    for (i=len-1; i>-1; i--)
    {
        if (str[i] != ' ')
            break;
    }
    
    /* check that str not too long */
    if (i > bufsize - 2)
        return (-1);
        
    /* copy str */
    for (j=0; j<i+1; j++)
        buf[j] = str[j];
        
    /* terminate */
    buf[i+1] = '\0';
    
    return (0);
}

