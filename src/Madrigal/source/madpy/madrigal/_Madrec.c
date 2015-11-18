/*
modification history
--------------------
11/2/2001	Written by Bill Rideout
*/

/*
*    This code exists to expose the functionality in the Madrigal
*    C API Madrec library as the Python module _Madrec.  These functions should
*    never be called directly by users of the Python Madrigal Madrec
*    library.  All calls to the Python Madrec library should be through
*    the public class MadrigalFile.py.
*
*    6/4/2002 - B. Rideout added cedarGetInformation, which returns a string
*    containing all the information in a header or catalog record.
*
*     $Id: _Madrec.c,v 1.57 2009/04/28 19:35:50 brideout Exp $
*/


#include "_Madrec.h"
#define MADREC_PY_STR_LEN 1000



/******** Wrapper functions for Madrigal C API madrec library ***********
*  
*  This file exports the following functions to Python:
*     getSummary
*     getDerivableParms
*     getIsprintReport
*     getFileType
*     looker
*     getParmsAtTime
*     radarToGeodetic
*     geodeticToRadar
*     createMaddata
*     destroyMaddata
*     getMadrecord
*     madrecOpen
*     madrecClose
*     madrecGetNextRec
*     madrecDumpDataRecord
*     madrecDumpCatalogRecord
*     madrecDumpHeaderRecord
*     madrecCreateDataRecord
*     cedarSet1dParm
*     cedarSet2dParm
*     madrecPutNextRec
*     madrecCreateCatalogRecord
*     madrecCreateHeaderRecord
*     destroyMadrecord
*     cedarGetParCodeType
*     madGetParMnemType
*     madGetParType
*     madGetCategoryIndex
*     cedarGetParMnemonic
*     cedarGetParDescription
*     cedarGetParScaleFactor
*     cedarGetParCodeFromMnemonic
*     cedarGetInformation
*     cedarCatalogHeaderList
*     madGetParDescription
*     madGetSimpleParDescription
*     madGetParUnits
*     madGetParFormat
*     madGetParWidth
*     madHasHtmlDesc
*     getUtFromDate
*     getDateFromUt
*     traceMagneticField
*     convertGeodeticGeomagnetic
*
*/


/***********************************************************************
*
* getSummary     gets list summary information in madrigal file
*
*   arguments:
*       1. Python string representing full path to file
*
*   returns:
*       Python list of the following Python objects:
*           1. Python list of Python integers of all KINST values found
*           2. Python list of Python integers of all KINDAT values found
*           3. Python list of Python integers of all parameters in file
*           4. Python list of Python integers, one for each parameter above,
*              =1 if that parameter was found to be missing from some records,
*              =-1 if never missing
*           5. Python double of maximum pulse length (microsec) found
*           6. Python double of minimum pulse length (microsec) found
*           7. Python double of maximum valid altitude found (km above sea level)
*              (Valid means a real error value exists at that altitude)
*           8. Python double of minimum valid altitude found (km above sea level)
*              (Valid means a real error value exists at that altitude)
*           9. Python double of maximum latitude (deg) found
*          10. Python double of minimum latitude (deg) found
*          11. Python double of maximum longitude (deg) found 
*          12. Python double of minimum longitude (deg) found
*          13. Python list of 6 Python integers representing earliest
*              date found in file [year, month, day, hour, min, sec]
*          14. Python list of 6 Python integers representing latest
*              date found in file [year, month, day, hour, min, sec]
*          15. Python list of Python integers of all 1-D parameters in file
*          16. Python list of Python integers of all 2-D parameters in file
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*       PyExc_IOError if unable to open madrigal file
*
*/
static PyObject * _Madrec_getSummary(PyObject * self, PyObject * args)
{
    double  maxPulseLen = 0.0; // maximum pulse length (microsec)
    double  minPulseLen = 0.0; // maximum pulse length (microsec)
    double  maxAltitude = 0.0; // maximum altitude of measurement (km)
    double  minAltitude = 0.0; // maximum altitude of measurement (km)
    double  maxLatitude = 0.0; // maximum latitude (deg)
    double  minLatitude = 0.0; // minimum latitude (deg)
    double  maxLongitude = 0.0; // maximum longitude (deg)
    double  minLongitude = 0.0; // minimum longitude (deg)
    char * filename;
    char errString[ERR_STR_LEN];
    int  errInt;
    Madrec * madrecp;
    double earliestTime = 0.0;
    double latestTime = 0.0;
    int i;
    int count = 0;
    int kinstArray[MAX_KINST];
    int kindatArray[MAX_KINDAT];
    int parmArray[MAX_NUM_PARMS];
    int parm1dArray[MAX_NUM_PARMS];
    int parm2dArray[MAX_NUM_PARMS];
    int missingArray[MAX_NUM_PARMS];
    
    // create an empty python list to hold kinst values
    PyObject * retKinstList;
    
    //create an empty python list to hold kindat values
    PyObject * retKindatList;
    
    //create an empty python list to hold parameters values
    PyObject * retParamList;

    //create an empty python list to hold whether parameter were ever missing
    PyObject * retMissingList;
    
    //create a python float to return max pulse length
    PyObject * retMaxPulseLen;

   //create a python float to return min pulse length
    PyObject * retMinPulseLen; 
    
    //create a python float to return max altitude
    PyObject * retMaxAltitude;

    //create a python float to return min altitude
    PyObject * retMinAltitude;

    //create a python float to return max latitude
    PyObject * retMaxLatitude;

    //create a python float to return min latitude
    PyObject * retMinLatitude;

    //create a python float to return max longitude
    PyObject * retMaxLongitude;

    //create a python float to return min longitude
    PyObject * retMinLongitude;
    
    //create a python list to hold earliest date/time
    // Format 6 ints: year, month, day, hour, min, sec
    PyObject * retEarliestDateTime;
    
    //create a python list to hold latest date/time
    // Format 6 ints: year, month, day, hour, min, sec
    PyObject * retLatestDateTime;
    
    //create an empty python list to hold 1d parameters values
    PyObject * retParam1dList;
    
    //create an empty python list to hold 2d parameters values
    PyObject * retParam2dList;
    
    // initialize python lists
    retKinstList = PyList_New(0);
    retKindatList = PyList_New(0);
    retParamList = PyList_New(0);
    retMissingList = PyList_New(0);
    retEarliestDateTime = PyList_New(0);
    retLatestDateTime = PyList_New(0);
    retParam1dList = PyList_New(0);
    retParam2dList = PyList_New(0);

    // initialize kinstArray to 0
    for (i=0; i < MAX_KINST; i++)
        kinstArray[i] = 0;

    // initialize kindatArray to 0
    for (i=0; i < MAX_KINDAT; i++)
        kindatArray[i] = 0;

    // initialize parmArray to 0
    for (i=0; i < MAX_NUM_PARMS; i++)
        parmArray[i] = 0;

    // initialize missingArray to 0
    for (i=0; i < MAX_NUM_PARMS; i++)
        missingArray[i] = 0;
    
    // get file name
    if (!PyArg_ParseTuple(args, "s", &filename))
        return NULL;
    
    // initialize parm1dArray to 0
    for (i=0; i < MAX_NUM_PARMS; i++)
        parm1dArray[i] = 0;
	
    // initialize parm2dArray to 0
    for (i=0; i < MAX_NUM_PARMS; i++)
        parm2dArray[i] = 0;
        
    /* Create a madrec object */
    madrecp = madrecCreate();

    /* open into memory, get Summary data */
    errInt = madrecOpen(madrecp, 30, filename);
    if (errInt)
    {
        sprintf(errString, "Unable to open %s, due to error %i", filename, errInt);
        PyErr_SetString(PyExc_IOError, errString);
        return NULL;
    }
    
    // get data from madrec
    
    /* get kinst array */
    for (i=0; i<madrecp->numKinst; i++)
        kinstArray[i] = madrecp->kinstArr[i];

    /* get kindat array */
    for (i=0; i<madrecp->numKindat; i++)
        kindatArray[i] = madrecp->kindatArr[i];

    /* get parm array - skip 1st 11 since they're derived */
    for (i=11; i<madrecp->numParms; i++)
        parmArray[i-11] = madrecp->parmsListp[i];

    /* get missing array - skip 1st 11 since they're derived */
    for (i=11; i<madrecp->numParms; i++)
    {
	if (madrecp->parmMissing[i] == 0)
            missingArray[i-11] = -1;
	else
	    missingArray[i-11] = 1;
    }
    
    /* get 1dparm array - skip 1st 11 since they're derived */
    count = 0;
    for (i=11; i<madrecp->numParms; i++)
    {
	if (madrecp->parmLocp[i] == 1)
	{
            parm1dArray[count] = madrecp->parmsListp[i];
	    count++;
	}
    }
    
    /* get 2dparm array - skip 1st 11 since they're derived */
    count = 0;
    for (i=11; i<madrecp->numParms; i++)
    {
	if (madrecp->parmLocp[i] == 2)
	{
            parm2dArray[count] = madrecp->parmsListp[i];
	    count++;
	}
    }
    
    
    /* get max and min pulse len */
    maxPulseLen = missing;
    for (i=11; i<madrecp->numParms; i++)
    {
        if (madrecp->parmsListp[i] == PULSE_LEN_CODE)
	{
	    maxPulseLen = madrecp->parmMaxp[i];
            minPulseLen = madrecp->parmMinp[i];
	    break;
	}
    }

    /* get max and min altitute - always index 10 */
    /* Note that if not found will be = -1.e38 */
    if (madrecp->parmMaxp[10] != -1.e38)
        maxAltitude = madrecp->parmMaxp[10];
    else
        maxAltitude = missing;
    
    if (madrecp->parmMinp[10] != -1.e38)
        minAltitude = madrecp->parmMinp[10];
    else
        minAltitude = missing;

    /* get max and min latitude - always index 8 */
    /* Note that if not found will be = -1.e38 */
    if (madrecp->parmMaxp[8] != -1.e38)
        maxLatitude = madrecp->parmMaxp[8];
    else
        maxLatitude = missing;
    
    if (madrecp->parmMinp[8] != -1.e38)
        minLatitude = madrecp->parmMinp[8];
    else
        minLatitude = missing;

    /* get max and min longitude - always index 9 */
    /* Note that if not found will be = -1.e38 */
    if (madrecp->parmMaxp[9] != -1.e38)
        maxLongitude = madrecp->parmMaxp[9];
    else
        maxLongitude = missing;
    
    if (madrecp->parmMinp[9] != -1.e38)
        minLongitude = madrecp->parmMinp[9];
    else
        minLongitude = missing;

    /* get earliest time */
    earliestTime = madrecp->earliestStartTime;
    
    /* get latest time */
    latestTime = madrecp->latestEndTime;
    
    // close file
    madrecClose(madrecp);
    
    // destroy madrec
    madrecDestroy(madrecp);

    // add kinst array to retKinstList
    addIntListToPythonList(kinstArray, retKinstList);

    // add kindat array to retKindatList
    addIntListToPythonList(kindatArray, retKindatList);

    // add parm array to retParamList
    addIntListToPythonList(parmArray, retParamList);

    // add missing array to retMissingList
    addIntListToPythonList(missingArray, retMissingList);
    
    // create max pulse length
    retMaxPulseLen = PyFloat_FromDouble(maxPulseLen);

    // create min pulse length
    retMinPulseLen = PyFloat_FromDouble(minPulseLen);
    
    // create max altitude
    retMaxAltitude = PyFloat_FromDouble(maxAltitude);

    // create min altitude
    retMinAltitude = PyFloat_FromDouble(minAltitude);

    // create max latitude
    retMaxLatitude = PyFloat_FromDouble(maxLatitude);

    // create min latitude
    retMinLatitude = PyFloat_FromDouble(minLatitude);

    // create max longitude
    retMaxLongitude = PyFloat_FromDouble(maxLongitude);

    // create min longitude
    retMinLongitude = PyFloat_FromDouble(minLongitude);

    // create earliest time python list
    setPyTime(retEarliestDateTime, earliestTime);

    // create latest time python list
    setPyTime(retLatestDateTime, latestTime);
    
    // add 1dparm array to retParam1dList
    addIntListToPythonList(parm1dArray, retParam1dList);
    
    // add 2dparm array to retParam2dList
    addIntListToPythonList(parm2dArray, retParam2dList);
    
    //return all info; 
    return Py_BuildValue("[OOOOOOOOOOOOOOOO]", retKinstList, retKindatList, retParamList,
                         retMissingList, retMaxPulseLen, retMinPulseLen, retMaxAltitude,
                         retMinAltitude, retMaxLatitude, retMinLatitude, retMaxLongitude,
                         retMinLongitude, retEarliestDateTime, retLatestDateTime,
			 retParam1dList, retParam2dList);
}


/***********************************************************************
*
* getDerivableParms     returns list of derivable parameters as 
*                       mnemonics given list of measured parameter 
*                       parcodes (integers) or mnemonics
*
*   arguments:
*       1. Python list with parcodes (integers or strings) or mnemonics of measured
*          parameters
*
*   returns:
*       Python list of mnemonics (strings) of derivable parameters
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_getDerivableParms(PyObject * self, PyObject * args)
{
    PyObject * MnemList;      /* input list */
    PyObject * PyItem;      /* list item */
    PyObject * outputList;    /* output list */
    MadparmList * measParms = NULL;
    MadparmList * dervParms = NULL;
    char * thisMnem = NULL;  /* individual mnemonic */
    int i;
    int numMnem = 0;
    char errString[ERR_STR_LEN];
    
    // get input list
    if (!PyArg_ParseTuple(args, "O!", &PyList_Type, &MnemList))
        return NULL;

    /* create measParms list */
    measParms = createMadparmList();

    /* populate measParms list */
    numMnem = PyList_Size(MnemList); 
    
    for (i=0; i<numMnem; i++)
    {
        PyItem = PyList_GetItem(MnemList, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errString, "Bad requested mnemonic in getDerivableParms at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errString);
	    return NULL;
	}
	
        if (appendMadparm(measParms, thisMnem) == -1)
	{
	    sprintf(errString, "Bad requested mnemonic in getDerivableParms at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errString);
	    return NULL;
	}
	free(thisMnem);
    }

    /* get list of derivable parameters */
    dervParms = getDerivedParms(measParms);

    /* create outputList from dervParms list */
    outputList = PyList_New(0);
    
    for (i=0; i<dervParms->numParm; i++)
    {
	PyList_Append(outputList, PyString_FromString(dervParms->mnemList[i]));
    }

    /* free memory */
    destroyMadparmList(measParms);
    destroyMadparmList(dervParms);
    
    
    //return all info; 
    return Py_BuildValue("O", outputList);
}



/***********************************************************************
*
* getIsprintReport     writes an isprint-formatted report to the file pointer
*
*   arguments:
*       1. Python string representing full path to file
*       2. Python string representing information string to print
*       3. Python list of parcodes or mnemonics of requested parameters
*       4. Python list giving filter types, len = # filters, chars are '1','*','/','+', or '-'
*       5. Python list of parcodes or mnemonics of filter parameter 1 (len = # filters)
*       6. Python list of parcodes or mnemonics of filter parameter 2 (len = # filters) (items may be None)
*       7. Python list of number of ranges per filter (len = # filters)
*       8. Python list of doubles of filter lower limits (len = sum of number of ranges above - items may be None)
*       9. Python list of doubles of filter upper limits (len = sum of number of ranges above - items may be None)
*      10. Python int - if 1, displayHeaders, if 0, don't
*      11. Python int - if 1, displaySummary, if 0, don't
*      12. Python int - maxCharsPerLine.  Can not be less than 50, or set to 50.  If 0, no limit.
*      13. Python string to use for missing data
*      14. Python string to use for assumed error data
*      15. Python string to use for known bad error data
*      16. Python string representing file to append data to (may be 'stdout' or 'stderr', or a file path)
*
*	Note that all empty items should be passed in as None
*
*   returns:
*       Python int 1
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_getIsprintReport(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int i = 0;
    int numFilters = 0;
    PyObject * PyItem;       /* list item */
    char * thisMnem = NULL;  /* individual mnemonic */
    FILE * fp = NULL;        /* file to print to, if not stdout or stderr*/
    char ** parm1Arr = NULL; /* array of parm 1 strings, length = # filters */
    char ** parm2Arr = NULL; /* array of parm 2 strings, length = # filters */
    int * numRanArr = NULL;  /* array of integers holding number of ranges, length = # filters */
    int numRanges = 0;       /* total number of ranges found */
    double * lowArr = NULL;  /* array of doubles holding lower ranges */
    double * highArr = NULL; /* array of doubles holding upper ranges */
    int thisNumRanges = 0;   /* number of ranges in present filter */
    int numRangesSoFar = 0;  /* running total of number of ranges used so far */
    Filter_type filtType = SINGLE_FILT;    /* filter type of present filter */
    
    /* error string buffer */
    char errBuf[1000] = "";

    /* maddata objects */
    MadfilterList * madFiltList = NULL;
    MadparmList * reqParmList = NULL;
    Maddata * maddata = NULL;
    
    /* input variables */
    char * filename = NULL;
    char * inputStr = NULL;
    PyObject * inParmList;
    PyObject * filtTypeList;
    PyObject * filtParm1List;
    PyObject * filtParm2List;
    PyObject * filtNumRangeList;
    PyObject * filterLowList;
    PyObject * filterHighList;
    
    int    displayHeaders = 0;
    int    displaySummary = 0;
    int    maxCharsPerLine = 0;
    
    char * missingStr = NULL;
    char * assumedStr = NULL;
    char * knownBadStr = NULL;
    char * outFile = NULL;
    

    // get input arguments
    if (!PyArg_ParseTuple(args, "ssO!O!O!O!O!O!O!iiissss",
			  &filename,
			  &inputStr,
			  &PyList_Type, &inParmList,
			  &PyList_Type, &filtTypeList,
			  &PyList_Type, &filtParm1List,
			  &PyList_Type, &filtParm2List,
			  &PyList_Type, &filtNumRangeList,
			  &PyList_Type, &filterLowList,
			  &PyList_Type, &filterHighList,
			  &displayHeaders,
			  &displaySummary,
			  &maxCharsPerLine,
			  &missingStr,
			  &assumedStr,
			  &knownBadStr,
			  &outFile))
    {
        return NULL;
    }
    
    /* verify all rules about filters */

    /* get number of filters */
    numFilters = PyList_Size(filtTypeList);

    /* create the list of requested parameters */
    reqParmList = createMadparmList();
    
    for (i=0; i<PyList_Size(inParmList); i++)
    {
        PyItem = PyList_GetItem(inParmList, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errBuf, "Bad parm in input Parm list in getIsprintReport - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	
        if (appendMadparm(reqParmList, thisMnem) == -1)
	{
	    sprintf(errBuf, "Bad parm %s in input Parm list in getIsprintReport - item %i", thisMnem, i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	if (thisMnem != NULL)
	    free(thisMnem);
    }
    
    /* verify filterLowList and filterHighList have same length */
    if (PyList_Size(filterLowList) != PyList_Size(filterHighList))
    {
        sprintf(errBuf, "lengths of low filter (%i) and high filter (%i) lists unequal",
	        (int)PyList_Size(filterLowList), (int)PyList_Size(filterHighList));
	PyErr_SetString(PyExc_TypeError, errBuf);
        return NULL;
    }


    /* create array of doubles to hold low and high range info if needed */
    if (PyList_Size(filterLowList) > 0)
    {
	lowArr = (double *)malloc(sizeof(double)*PyList_Size(filterLowList));
	highArr = (double *)malloc(sizeof(double)*PyList_Size(filterHighList));
	if (lowArr == NULL || highArr ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
    }
    
    for (i=0; i<PyList_Size(filterLowList); i++)
    {
	PyItem = PyList_GetItem(filterLowList, i);
	lowArr[i] = getDoubleFromPy(PyItem);
	PyItem = PyList_GetItem(filterHighList, i);
	highArr[i] = getDoubleFromPy(PyItem);
    }

    /* verify filtTypeList and filtParm1List have same length */
    if (PyList_Size(filtTypeList) != PyList_Size(filtParm1List))
    {
        sprintf(errBuf, "lengths of filter types (%i) and first parameter lists (%i) lists unequal",
	        (int)PyList_Size(filtTypeList), (int)PyList_Size(filtParm1List));
	PyErr_SetString(PyExc_TypeError, errBuf);
        return NULL;
    }
    

    /* create array of strings to hold parm 1 info */
    parm1Arr = (char **)malloc(sizeof(char *)*numFilters);
    if (parm1Arr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }

    for (i=0; i<numFilters; i++)
    {
	
	PyItem = PyList_GetItem(filtParm1List, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errBuf, "Bad parm in first filter parameters in getIsprintReport - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	
	parm1Arr[i] = malloc(sizeof(char) * (strlen(thisMnem)+1));
	if (parm1Arr[i] ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
	
	strcpy(parm1Arr[i], thisMnem);
	if (thisMnem != NULL)
	    free(thisMnem);
    }


    /* create array of strings to hold parm 2 info */
    parm2Arr = (char **)malloc(sizeof(char *)*numFilters);
    if (parm2Arr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }
    for (i=0; i<numFilters; i++)
    {
	
	PyItem = PyList_GetItem(filtParm2List, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL && PyItem == Py_None)
	{
	    /* for second list NULL is allowed - means no second parameter */
	    parm2Arr[i] = malloc(sizeof(char) * 1);
	}
	else if (thisMnem == NULL && PyItem != Py_None)
	{
	    /* bad parameter passed in */
	    sprintf(errBuf, "Bad parm in second filter parameters in getIsprintReport - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	else
	    parm2Arr[i] = malloc(sizeof(char) * (strlen(thisMnem)+1));
	if (parm2Arr[i] ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
	if (thisMnem == NULL)
	    strcpy(parm2Arr[i], "");
	else
	    strcpy(parm2Arr[i], thisMnem);
	if (thisMnem != NULL)
	    free(thisMnem);
    }
    

    /* create array of ints to hold number of range info */
    numRanArr = (int *)malloc(sizeof(int)*numFilters);
    if (numRanArr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }
    
    for (i=0; i<PyList_Size(filtNumRangeList); i++)
    {
	PyItem = PyList_GetItem(filtNumRangeList, i);
	thisNumRanges = getIntFromPy(PyItem);
	numRanArr[i] = thisNumRanges;
	numRanges += thisNumRanges;
    }
    
    /* verify numRanges == len(filterLowList) */
    if (numRanges != PyList_Size(filterLowList))
    {
        PyErr_SetString(PyExc_TypeError, "Total number of ranges does not match the number of lower limits");
	return NULL;
    }

    /* create madFiltList */
    madFiltList = createMadfilterList();
    
    for (i=0; i<numFilters; i++)
    {
	/* loop through each filter */

	/* get the number of ranges in this filter */
	thisNumRanges = numRanArr[i];

	/* check that no error has occurred */
	if (numRangesSoFar + thisNumRanges > numRanges)
	{
	    PyErr_SetString(PyExc_TypeError, "Number of range mismatch in getIsprintReport");
	    return NULL;
	}

	/* get filter type */
	PyItem = PyList_GetItem(filtTypeList, i);
	if (!PyString_Check(PyItem))
	{
	    PyErr_SetString(PyExc_TypeError, "Non string passed into filter type list");
	    return(NULL);
	}
	switch (PyString_AsString(PyItem)[0])
	{
	    case '1':
		filtType = SINGLE_FILT;
		break;
	    case '*':
		filtType = MULT_FILT;
		break;
	    case '/':
		filtType = DIV_FILT;
		break;
	    case '+':
		filtType = ADD_FILT;
		break;
	    case '-':
		filtType = SUB_FILT;
		break;
	    default:
		sprintf(errBuf, "Error in type string in getIsprintReport: %c",
		        PyString_AsString(PyItem)[0]);
	        PyErr_SetString(PyExc_TypeError, errBuf);
		return NULL;
	}

	/* append next filter */
	if (appendMadfilter(madFiltList,
			    filtType,
			    thisNumRanges,
			    lowArr + numRangesSoFar,
			    highArr + numRangesSoFar,
			    parm1Arr[i],
			    parm2Arr[i]) == -1)
	{
	    PyErr_SetString(PyExc_TypeError, "Problem appending filter in getIsprintReport");
	    return NULL;
	}
	
	/* prepare for next filter */
	numRangesSoFar += thisNumRanges;
    }

    /* create the maddata  */
    maddata = createMaddata(filename,
                            inputStr,
                            reqParmList,
                            madFiltList,
                            NULL);

    /* throw error if failed */
    if (maddata == NULL)
    {
	sprintf(errBuf, "Unable to open requested input file: %s", filename);
	PyErr_SetString(PyExc_TypeError, errBuf);
	return NULL;
    }

    /* figure out where to write the data */
    if (strcmp("stdout", outFile) != 0 && strcmp("stderr", outFile))
    {
	fp = fopen(outFile, "a");
	if (fp == NULL)
	{
	    sprintf(errBuf, "Problem opening outFile %s in getIsprintReport", outFile);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	/* print all data in isprint format */
	classicIsprint(maddata,
		       displayHeaders,
		       displaySummary,
		       maxCharsPerLine,
		       missingStr,
		       assumedStr,
		       knownBadStr,
		       fp);
	fclose(fp);
    }
    else if (strcmp("stdout", outFile) == 0)
	/* print all data in isprint format to stdout */
	classicIsprint(maddata,
		       displayHeaders,
		       displaySummary,
		       maxCharsPerLine,
		       missingStr,
		       assumedStr,
		       knownBadStr,
		       stdout);
    else
	/* print all data in isprint format to stderr */
	classicIsprint(maddata,
		       displayHeaders,
		       displaySummary,
		       maxCharsPerLine,
		       missingStr,
		       assumedStr,
		       knownBadStr,
		       stderr);

    
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(reqParmList);
    destroyMadfilterList(madFiltList);
    if (lowArr != NULL)
	free(lowArr);
    if (highArr != NULL)
	free(highArr);
    if (numRanArr != NULL)
	free(numRanArr);
    for (i=0; i<numFilters; i++)
    {
	free(parm1Arr[i]);
	free(parm2Arr[i]);
    }
    if (parm1Arr != NULL)
	free(parm1Arr);
    if (parm2Arr != NULL)
	free(parm2Arr);


    /* success */
    retObj = Py_BuildValue("i", 1);
    return(retObj);
}


/***********************************************************************
*
* getFileType     returns a string representing the file type
*
*
*   arguments:
*       1. Python string representing full path to file
*
*   returns:
*       1. Python string 'Madrigal', 'BlockedBinary', 'Cbf',
*          'UnblockedBinary', 'Ascii', or 'Unknown'
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_getFileType(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    char madrigalStr[] = "Madrigal";
    char blockedBinaryStr[] = "BlockedBinary";
    char cbfStr[] = "Cbf";
    char unblockedBinaryStr[] = "UnblockedBinary";
    char asciiStr[] = "Ascii";
    char unknownStr[] = "Unknown";

    char * resultStr = NULL;
    
    char * filename;
    int result = 0;
    
    
    // get input arguments
    if (!PyArg_ParseTuple(args, "s",
			  &filename))
    {
        return NULL;
    }

    result = cedarFileType(filename, MADRIGALBLOCKSIZE, CBFBLOCKSIZE);

    switch(result)
    {
        case 0:
            resultStr = madrigalStr;
            break;

        case 1:
            resultStr = blockedBinaryStr;
            break;

        case 2:
            resultStr = cbfStr;
            break;

        case 3:
            resultStr = unblockedBinaryStr;
            break;

        case 4:
            resultStr = asciiStr;
            break;

        default:
            resultStr = unknownStr;
    }

    
    /* success */
    retObj = Py_BuildValue("s", resultStr);
    return(retObj);
}

/***********************************************************************
*
* looker     prints a Madrecord for a range of lat, lon, and alt
*
*
*   arguments:
*       1. Python list of mnemonics of requested parameters
*          (Do not include gdlat, glon, or gdalt)
*       Python doubles:
*               2.start_lat  3.stop_lat  4.step_lat
*               5.start_lon  6.stop_lon  7.step_lon
*               8.start_alt  9.stop_alt  10.step_alt  11. ut (secs since 1/1/1950)
*       12. printHeaderFlag - if 0, do not print header
*
*   returns:
*       1. Python int 1
*
*   affects: madrecord for given range of lat, lon, and alt printed. 
*
*   Notes: start_lon may be greater than stop_lon, in which case scan will go through 0
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_looker(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    MadparmList * parmListShow;  /* list of parameters to show */
    MadparmList * parmMeas1D;    /* list of 1D parameters for which data will be given */
    MadparmList * parmMeas2D;    /* list of 2D parameters for which data will be given */
    Maddata * maddata = NULL;
    
    /* input data */
    double * input1D = NULL;
    double ** input2D = NULL;
    int kinst = 0;
    double ut1=0.0, ut2=0.0;
    double start_lat, stop_lat, step_lat;
    double start_lon, stop_lon, step_lon;
    double start_alt, stop_alt, step_alt;
    int printHeaderFlag = 0;
    int numLat = 0;
    int numLon = 0;
    int numAlt = 0;
    int numTotal = 0;
    int i=0;
    int stepNum = 0;
    
    PyObject * inParmList;   /* requested parameters */
    PyObject * PyItem;       /* list item */
    char * thisMnem = NULL;  /* individual mnemonic */
 
    char errBuf[1000] = "";  /* error string buffer */
    
    // get input arguments
    if (!PyArg_ParseTuple(args, "O!ddddddddddi",
			  &PyList_Type, &inParmList,
			  &start_lat,
			  &stop_lat,
			  &step_lat,
			  &start_lon,
			  &stop_lon,
			  &step_lon,
			  &start_alt,
			  &stop_alt,
			  &step_alt,
			  &ut1,
                          &printHeaderFlag))
    {
        return NULL;
    }
    
    ut2 = ut1;
    
    /* create parmListShow */
    parmListShow = createMadparmList();
    appendMadparm(parmListShow, "GDLAT");
    appendMadparm(parmListShow, "GLON");
    appendMadparm(parmListShow, "GDALT");
    
    for (i=0; i<PyList_Size(inParmList); i++)
    {
        PyItem = PyList_GetItem(inParmList, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errBuf, "Bad parm in input Parm list in looker - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	
        if (appendMadparm(parmListShow, thisMnem) == -1)
	{
	    sprintf(errBuf, "Bad parm %s in input Parm list in looker - item %i", thisMnem, i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	if (thisMnem != NULL)
	    free(thisMnem);
    }
    
    /* enforce minimum step size */
    if (step_lat < 0.01)
        step_lat = 0.01;
    if (step_lon < 0.01)
        step_lon = 0.01;
    if (step_alt < 0.01)
        step_alt = 0.01;
	
    /* force long values to be between -180 and 180 */
    while (start_lon > 180.0)
        start_lon -= 360;
    while (start_lon < -180.0)
        start_lon += 360;
    while (stop_lon > 180.0)
        stop_lon -= 360;
    while (stop_lon < -180.0)
        stop_lon += 360;
        
    /* calculate number of steps for each */
    numLat = (int)((stop_lat - start_lat) / step_lat) + 1;
    numAlt = (int)((stop_alt - start_alt) / step_alt) + 1;
    if (stop_lon >= start_lon)
        numLon = (int)((stop_lon - start_lon) / step_lon) + 1;
    else
        numLon = (int)(((stop_lon + 360.0) - start_lon) / step_lon) + 1;
    
    /* check for num less than 1 */
    if (numLat < 1) numLat = 1;
    if (numLon < 1) numLon = 1;
    if (numAlt < 1) numAlt = 1;
    
    numTotal = numLat * numLon * numAlt;
    
    /* check for a huge number of points */
    if (numTotal > 1000000)
    {
	sprintf(errBuf, "Too many points in looker: %i", numTotal);
	PyErr_SetString(PyExc_TypeError, errBuf);
	return NULL;
    }
    
    /* malloc input2D */
    input2D = (double **)malloc(3*sizeof(double *));
    if (input2D == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "Malloc");
        return(NULL);
    }
    for (i=0; i<3; i++)
    {
        input2D[i] = (double *)malloc(numTotal * sizeof(double));
        if (input2D[i] == NULL)
        {
            PyErr_SetString(PyExc_TypeError, "Malloc");
            return(NULL);
        }
    }
    
    /* write lat data first */
    for (i=0; i<numTotal; i++)
    {
        stepNum = i / (numLon * numAlt);
        input2D[0][i] = start_lat + (stepNum * step_lat);
    }
    
    /* write lon data next */
    for (i=0; i<numTotal; i++)
    {
        stepNum = (i / numAlt) % numLon;
        input2D[1][i] = start_lon + (stepNum * step_lon);
	if (input2D[1][i] > 180.0)
	    input2D[1][i] -= 360.0;
    }
    
    /* last, write alt data */
    for (i=0; i<numTotal; i++)
    {
        stepNum = i % numAlt;
        input2D[2][i] = start_alt + (stepNum * step_alt);
    }
    
    /* create parmMeas1D - None */
    parmMeas1D = createMadparmList();
    
    /* create parmMeas2D */
    parmMeas2D = createMadparmList();
    appendMadparm(parmMeas2D, "GDLAT");
    appendMadparm(parmMeas2D, "GLON");
    appendMadparm(parmMeas2D, "GDALT");
    

    /* create the data you want */
    maddata = createNonfileMaddata(parmListShow,
                                   ut1,
                                   ut2,
                                   kinst,
                                   parmMeas1D,
                                   parmMeas2D,
                                   numTotal,
                                   input1D,
                                   input2D,
                                   stdout);
                                   
    
    lookerMadrecordPrint(maddata, 
                         "Missing",
                         "Assumed",
                         "KnownBad",
                         printHeaderFlag,
                         stdout);
                            
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListShow);
    destroyMadparmList(parmMeas1D);
    destroyMadparmList(parmMeas2D);
    
    for (i=0; i<3; i++)
        free(input2D[i]);
    free(input2D);
    
    /* success */
    retObj = Py_BuildValue("i", 1);
    return(retObj);
}


/***********************************************************************
*
* looker_nonFile     prints a Madrecord for a range of lat, lon, and alt.
*                    Differs from looker in that allows the setting of any
*                    number of input 1D parameter2.
*
*
*   arguments:
*       1. Python list of mnemonics of requested parameters
*          (Do not include gdlat, glon, or gdalt)
*       Python doubles:
*               2.start_lat  3.stop_lat  4.step_lat
*               5.start_lon  6.stop_lon  7.step_lon
*               8.start_alt  9.stop_alt  10.step_alt  11. ut (secs since 1/1/1950)
*       12. printHeaderFlag - if 0, do not print header
*       13. Python list of 1D parameters as mnemonic strings.  May be 0 length.
*           Cannot include gdalt, glon, or gdlat.
*       14. Python list of 1D parameter values as doubles.  Len must =
*           len of python list of parameters
*
*   returns:
*       1. Python int 1
*
*   affects: madrecord for given range of lat, lon, and alt printed. 
*
*   Notes: start_lon may be greater than stop_lon, in which case scan will go through 0
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_looker_nonFile(PyObject * self, PyObject * args)
{
    PyObject * retObj;
    PyObject * pyItem;
    MadparmList * parmListShow;  /* list of parameters to show */
    MadparmList * parmMeas1D;    /* list of 1D parameters for which data will be given */
    MadparmList * parmMeas2D;    /* list of 2D parameters for which data will be given */
    Maddata * maddata = NULL;
    
    /* error string buffer */
    char errBuf[1000] = "";
    
    /* input data */
    double * input1D = NULL;
    double ** input2D = NULL;
    int kinst = 0; /* default, but may be overwritten by input 1D data */
    double ut1=0.0, ut2=0.0;
    double start_lat, stop_lat, step_lat;
    double start_lon, stop_lon, step_lon;
    double start_alt, stop_alt, step_alt;
    int printHeaderFlag = 0;
    int numLat = 0;
    int numLon = 0;
    int numAlt = 0;
    int numTotal = 0;  /* number of rows */
    int num1DParms = 0;
    int numMnem = 0;
    
    int i=0;
    int stepNum = 0;
    
    PyObject * MnemList;
    PyObject * OneDParmList;
    PyObject * OneDParmValues;
    PyObject * doubleObj; /* python double object */
    double oneDValue = 0.0;
   
    char * mnem;

    
    // get input arguments
    if (!PyArg_ParseTuple(args, "O!ddddddddddiO!O!",
			  &PyList_Type, &MnemList,
			  &start_lat,
			  &stop_lat,
			  &step_lat,
			  &start_lon,
			  &stop_lon,
			  &step_lon,
			  &start_alt,
			  &stop_alt,
			  &step_alt,
			  &ut1,
                          &printHeaderFlag,
			  &PyList_Type, &OneDParmList,
			  &PyList_Type, &OneDParmValues))
    {
        return NULL;
    }
    
    
    ut2 = ut1;
     
    /* get number of requested parameters */
    numMnem = PyList_Size(MnemList); 
    
    if (numMnem == 0)
    {
        PyErr_SetString(PyExc_TypeError, "Must pass in at least one requested parameter to looker_nonFile");
        return NULL;
    }  
    
    /* create parmListShow */
    parmListShow = createMadparmList();
    appendMadparm(parmListShow, "GDLAT");
    appendMadparm(parmListShow, "GLON");
    appendMadparm(parmListShow, "GDALT");
    
    for (i=0; i<numMnem; i++)
    {
	pyItem = PyList_GetItem(MnemList, i);
	mnem = getMnemFromPy(pyItem);
	if (mnem == NULL)
	{
	    sprintf(errBuf, "Illegal requested parameter at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return(NULL);
	}
        if (appendMadparm(parmListShow, mnem) == -1)
	{
	    sprintf(errBuf, "Illegal requested parameter at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return(NULL);
	}
	free(mnem);
    }
    
    /* enforce minimum step size */
    if (step_lat < 0.01)
        step_lat = 0.01;
    if (step_lon < 0.01)
        step_lon = 0.01;
    if (step_alt < 0.01)
        step_alt = 0.01;
	
    /* force long values to be between -180 and 180 */
    while (start_lon > 180.0)
        start_lon -= 360;
    while (start_lon < -180.0)
        start_lon += 360;
    while (stop_lon > 180.0)
        stop_lon -= 360;
    while (stop_lon < -180.0)
        stop_lon += 360;
        
    /* calculate number of steps for each */
    numLat = (int)((stop_lat - start_lat) / step_lat) + 1;
    numAlt = (int)((stop_alt - start_alt) / step_alt) + 1;
    if (stop_lon >= start_lon)
        numLon = (int)((stop_lon - start_lon) / step_lon) + 1;
    else
        numLon = (int)(((stop_lon + 360.0) - start_lon) / step_lon) + 1;
    
    /* check for num less than 1 */
    if (numLat < 1) numLat = 1;
    if (numLon < 1) numLon = 1;
    if (numAlt < 1) numAlt = 1;
    
    /* get number of 1D parms */
    num1DParms = PyList_Size(OneDParmList);
    
    /* malloc input1D if needed */
    if (num1DParms > 0)
        input1D = (double *)malloc(num1DParms*sizeof(double));
    
    /* create parmMeas1D */
    parmMeas1D = createMadparmList();
    for (i=0; i<num1DParms; i++)
    {
        pyItem = PyList_GetItem(OneDParmList, i);
	mnem = getMnemFromPy(pyItem);
	if (mnem == NULL)
	{
	    sprintf(errBuf, "Illegal 1D parameter at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return(NULL);
	}
        appendMadparm(parmMeas1D, mnem);
	
	/* make sure its not an illegal parameter */
	if (!str_caseins_cmp(mnem, "GDLAT") || \
	    !str_caseins_cmp(mnem, "GLON") || \
	    !str_caseins_cmp(mnem, "GDALT"))
	{
	    PyErr_SetString(PyExc_TypeError, "Cannot set GDALT, GLON, or GDLAT as 1D parm in looker_nonFile");
	    return NULL;
	}
	
	/* now set values */
	doubleObj = PyList_GetItem(OneDParmValues, i);
	oneDValue = PyFloat_AS_DOUBLE(doubleObj);;
	input1D[i] = oneDValue;
	
	/* finally, check for special case of setting kinst */
	if (!str_caseins_cmp(mnem, "KINST"))
	    kinst = (int)oneDValue;
    };
    
    /* create parmMeas2D */
    parmMeas2D = createMadparmList();
    appendMadparm(parmMeas2D, "GDLAT");
    appendMadparm(parmMeas2D, "GLON");
    appendMadparm(parmMeas2D, "GDALT");
    
    numTotal = numLat * numLon * numAlt;
    
    /* check for a huge number of points */
    if (numTotal > 1000000)
    {
        sprintf(errBuf, "Too many points in looker_nonFile: %i", numTotal);
	PyErr_SetString(PyExc_TypeError, errBuf);
	return NULL;
    }
    
    /* malloc input2D */
    input2D = (double **)malloc(3*sizeof(double *));
    if (input2D == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "Malloc");
        return(NULL);
    }
    for (i=0; i<3; i++)
    {
        input2D[i] = (double *)malloc(numTotal * sizeof(double));
        if (input2D[i] == NULL)
        {
            PyErr_SetString(PyExc_TypeError, "Malloc");
            return(NULL);
        }
    }
    
    /* write lat data first */
    for (i=0; i<numTotal; i++)
    {
        stepNum = i / (numLon * numAlt);
        input2D[0][i] = start_lat + (stepNum * step_lat);
    }
    
    /* write lon data next */
    for (i=0; i<numTotal; i++)
    {
        stepNum = (i / numAlt) % numLon;
        input2D[1][i] = start_lon + (stepNum * step_lon);
	if (input2D[1][i] > 180.0)
	    input2D[1][i] -= 360.0;
    }
    
    /* last, write alt data */
    for (i=0; i<numTotal; i++)
    {
        stepNum = i % numAlt;
        input2D[2][i] = start_alt + (stepNum * step_alt);
    };
    
    /* create the data you want */
    maddata = createNonfileMaddata(parmListShow,
                                   ut1,
                                   ut2,
                                   kinst,
                                   parmMeas1D,
                                   parmMeas2D,
                                   numTotal,
                                   input1D,
                                   input2D,
                                   stdout);
                                   
    
    lookerMadrecordPrint(maddata, 
                         "Missing",
                         "Assumed",
                         "KnownBad",
                         printHeaderFlag,
                         stdout);
                            
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListShow);
    destroyMadparmList(parmMeas1D);
    destroyMadparmList(parmMeas2D);
    
    for (i=0; i<3; i++)
        free(input2D[i]);
    free(input2D);
    if (num1DParms > 0)
        free(input1D);
    
    /* success */
    retObj = Py_BuildValue("i", 1);
    return(retObj);
}


/***********************************************************************
*
* getParmsAtTime     prints a Madrecord for a single time (parameters must not depend on location)
*
*
*   arguments:
*       1. Python list of mnemonics of requested parameters
*          (Do not include any that depend on location)
*       2. ut (secs since 1/1/1950 as double)
*       3. printHeaderFlag - if 0, do not print header
*
*   returns:
*       1. Python int 1
*
*   affects: madrecord for given time printed. 
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_getParmsAtTime(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    PyObject * MnemList;         /* list of requested parameters */
    PyObject * pyItem;           /* individual python item in list */
    MadparmList * parmListShow;  /* list of parameters to show */
    MadparmList * parmMeas1D;    /* list of 1D parameters for which data will be given */
    MadparmList * parmMeas2D;    /* list of 2D parameters for which data will be given */
    Maddata * maddata = NULL;
    char * mnem;                 /* temp string to hold mnemonic */
    int i;
    char errBuf[1000] = "";      /* error string buffer */
    
    /* input data */
    double * input1D = NULL;
    double ** input2D = NULL;
    int kinst = 0;
    double ut1=0.0, ut2=0.0;
    int printHeaderFlag = 0;
    
    // get input arguments
    if (!PyArg_ParseTuple(args, "O!di",
			  &PyList_Type, &MnemList,
			  &ut1,
                          &printHeaderFlag))
    {
        return NULL;
    }
    
    ut2 = ut1;
    
    /* create parmListShow */
    parmListShow = createMadparmList();
    appendMadparm(parmListShow, "YEAR");
    appendMadparm(parmListShow, "MONTH");
    appendMadparm(parmListShow, "DAY");
    appendMadparm(parmListShow, "HOUR");
    appendMadparm(parmListShow, "MIN");
    appendMadparm(parmListShow, "SEC");
    
    if (PyList_Size(MnemList) == 0)
    {
        PyErr_SetString(PyExc_TypeError, "Must pass in at least one requested parameter to getParmsAtTime");
        return NULL;
    }  
    
    for (i=0; i<PyList_Size(MnemList); i++)
    {
	pyItem = PyList_GetItem(MnemList, i);
	mnem = getMnemFromPy(pyItem);
	if (mnem == NULL)
	{
	    sprintf(errBuf, "Illegal requested parameter at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return(NULL);
	}
        if (appendMadparm(parmListShow, mnem) == -1)
	{
	    sprintf(errBuf, "Illegal requested parameter at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return(NULL);
	}
	free(mnem);
    }
    
    /* create parmMeas1D - None */
    parmMeas1D = createMadparmList();
    
    /* create parmMeas2D - None */
    parmMeas2D = createMadparmList();
    

    /* create the data you want */
    maddata = createNonfileMaddata(parmListShow,
                                   ut1,
                                   ut2,
                                   kinst,
                                   parmMeas1D,
                                   parmMeas2D,
                                   0,
                                   input1D,
                                   input2D,
                                   stdout);
                                   
    
    lookerMadrecordPrint(maddata, 
                         "Missing",
                         "Assumed",
                         "KnownBad",
                         printHeaderFlag,
                         stdout);
                            
    /* clean up */
    destroyMaddata(maddata);
    destroyMadparmList(parmListShow);
    destroyMadparmList(parmMeas1D);
    destroyMadparmList(parmMeas2D);
    
    /* success */
    retObj = Py_BuildValue("i", 1);
    return(retObj);
} 



/***********************************************************************
*
* radarToGeodetic     returns gdalt,gdlat,glon given radar location and az,el,range
*
*
*   arguments:
*       1. radar geodetic latitude
*       2. radar longitude
*       3. radar geodetic altitude
*       4. azimuth
*       5. elevation
*       6. range in km
*
*   returns:
*       1. Python tuple with (gdlat,glon,gdalt) of point 
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_radarToGeodetic(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    
    /* input arguments */
    double slatgd = 0.0;
    double slon = 0.0;
    double saltgd = 0.0;
    double az = 0.0;
    double el = 0.0;
    double range = 0.0;
    
    /* return values */
    double gdalt = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    
    /* arguments needed by point */
    double sr = 0.0;
    double slatgc = 0.0;
    int direction = 1;
    double pr = 0.0;
    double gclat = 0.0;
    
    // get input arguments
    if (!PyArg_ParseTuple(args, "dddddd",
			  &slatgd,
			  &slon,
			  &saltgd,
			  &az,
			  &el,
			  &range))
    {
        return NULL;
    }
    
    if (range < 0.0)
    {
        PyErr_SetString(PyExc_TypeError, "Range must be > 0 for radarToGeodetic");
        return NULL;
    }
    
    
    /* force slon to between -180 and 180 */
    while (slon < -180.0) slon += 360.0;
    while (slon > +180.0) slon -= 360.0;
    
    /* get geocentric coordinates needed by point */
    convrt_(&direction,&slatgd,&saltgd,&slatgc,&sr);
    
    point_(&sr,&slatgc,&slon,&az,&el,&range,
           &pr,&gclat,&glon);
	   
    direction = 2;
    convrt_(&direction,&gdlat,&gdalt,&gclat,&pr);
    
    /* success */
    retObj = Py_BuildValue("(ddd)", gdlat,glon,gdalt);
    return(retObj);
} 


/***********************************************************************
*
* geodeticToRadar     given gdalt,gdlat,glon and radar location, returns az,el,range
*
*
*   arguments:
*       1. radar geodetic latitude
*       2. radar longitude
*       3. radar geodetic altitude
*       4. point geodetic latitude
*       5. point longitude
*       6. point geodetic altitude
*
*   returns:
*       1. Python tuple with (az,el,range) of radar view of point 
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/    
static PyObject * _Madrec_geodeticToRadar(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    
    /* input arguments */
    double slatgd = 0.0;
    double slon = 0.0;
    double saltgd = 0.0;
    double gdalt = 0.0;
    double gdlat = 0.0;
    double glon = 0.0;
    
    /* return values */
    double az = 0.0;
    double el = 0.0;
    double range = 0.0; 
    
    /* arguments needed by look */
    double sr = 0.0;
    double slatgc = 0.0;
    double pr = 0.0;
    double gclat = 0.0;
    int direction = 1;
    
    // get input arguments
    if (!PyArg_ParseTuple(args, "dddddd",
			  &slatgd,
			  &slon,
			  &saltgd,
			  &gdlat,
			  &glon,
			  &gdalt))
    {
        return NULL;
    }
    
    
    /* force slon to between -180 and 180 */
    while (slon < -180.0) slon += 360.0;
    while (slon > +180.0) slon -= 360.0;
    
    /* force glon to between -180 and 180 */
    while (glon < -180.0) glon += 360.0;
    while (glon > +180.0) glon -= 360.0;
    
    /* get geocentric coordinates needed by look */
    convrt_(&direction,&slatgd,&saltgd,&slatgc,&sr);
    convrt_(&direction,&gdlat,&gdalt,&gclat,&pr);
    
    look_(&sr,&slatgc,&slon,&pr,&gclat,&glon,&az,&el,&range);
    
    /* force az to between -180 and 180 */
    while (az < -180.0) az += 360.0;
    while (az > +180.0) az -= 360.0;
    
    /* success */
    retObj = Py_BuildValue("(ddd)", az,el,range);
    return(retObj);
} 



/***********************************************************************
*
* createMaddata     creates a pointer to a maddata structure for later use
*
*   Be sure to call destroyMaddata when done to free malloc'ed memory
*
*   arguments:
*       1. Python string representing full path to file
*       2. Python string representing information string to print
*       3. Python list of parcodes or mnemonics of requested parameters
*       4. Python list giving filter types, len = # filters, chars are '1','*','/','+', or '-'
*       5. Python list of parcodes or mnemonics of filter parameter 1 (len = # filters)
*       6. Python list of parcodes or mnemonics of filter parameter 2 (len = # filters) (items may be None)
*       7. Python list of number of ranges per filter (len = # filters)
*       8. Python list of doubles of filter lower limits (len = sum of number of ranges above - items may be None)
*       9. Python list of doubles of filter upper limits (len = sum of number of ranges above - items may be None)
*
*   returns:
*       1. Python int - pointer to malloc'ed data.  If 0, failed
*       2. Python int - number of cycles
*       3. Python list - list of integers, giving num of records for each cycle
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_createMaddata(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    PyObject * PyItem;       /* list item */
    char * thisMnem = NULL;  /* individual mnemonic */
    int i = 0;
    int numFilters = 0;
    char ** parm1Arr = NULL; /* array of parm 1 strings, length = # filters */
    char ** parm2Arr = NULL; /* array of parm 2 strings, length = # filters */
    int * numRanArr = NULL;  /* array of integers holding number of ranges, length = # filters */
    int numRanges = 0;       /* total number of ranges found */
    double * lowArr = NULL;  /* array of doubles holding lower ranges */
    double * highArr = NULL; /* array of doubles holding upper ranges */
    int thisNumRanges = 0;   /* number of ranges in present filter */
    int numRangesSoFar = 0;  /* running total of number of ranges used so far */
    Filter_type filtType = SINGLE_FILT;    /* filter type of present filter */
    
    /* error string buffer */
    char errBuf[1000] = "";

    /* maddata objects */
    MadfilterList * madFiltList = NULL;
    MadparmList * reqParmList = NULL;
    Maddata * maddata = NULL;
    
    /* input variables */
    char * filename = NULL;
    char * inputStr = NULL;
    PyObject * inParmList;
    PyObject * filtTypeList;
    PyObject * filtParm1List;
    PyObject * filtParm2List;
    PyObject * filtNumRangeList;
    PyObject * filterLowList;
    PyObject * filterHighList;
    
    PyObject * outputList;    /* output list */

    // get input arguments
    if (!PyArg_ParseTuple(args, "ssO!O!O!O!O!O!O!",
			  &filename,
			  &inputStr,
			  &PyList_Type, &inParmList,
			  &PyList_Type, &filtTypeList,
			  &PyList_Type, &filtParm1List,
			  &PyList_Type, &filtParm2List,
			  &PyList_Type, &filtNumRangeList,
			  &PyList_Type, &filterLowList,
			  &PyList_Type, &filterHighList))
    {
        return NULL;
    }

    /* get number of filters */
    numFilters = PyList_Size(filtTypeList);

    /* create the list of requested parameters */
    reqParmList = createMadparmList();
    
    for (i=0; i<PyList_Size(inParmList); i++)
    {
        PyItem = PyList_GetItem(inParmList, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errBuf, "Bad parm in input Parm list in createMaddata - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	
        if (appendMadparm(reqParmList, thisMnem) == -1)
	{
	    sprintf(errBuf, "Bad parm %s in input Parm list in createMaddata - item %i", thisMnem, i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	if (thisMnem != NULL)
	    free(thisMnem);
    }
    
    /* verify filterLowList and filterHighList have same length */
    if (PyList_Size(filterLowList) != PyList_Size(filterHighList))
    {
        sprintf(errBuf, "lengths of low filter (%i) and high filter (%i) lists unequal",
	        (int)PyList_Size(filterLowList), (int)PyList_Size(filterHighList));
	PyErr_SetString(PyExc_TypeError, errBuf);
        return NULL;
    }


    /* create array of doubles to hold low and high range info if needed */
    if (PyList_Size(filterLowList) > 0)
    {
	lowArr = (double *)malloc(sizeof(double)*PyList_Size(filterLowList));
	highArr = (double *)malloc(sizeof(double)*PyList_Size(filterHighList));
	if (lowArr == NULL || highArr ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
    }
    
    for (i=0; i<PyList_Size(filterLowList); i++)
    {
	PyItem = PyList_GetItem(filterLowList, i);
	lowArr[i] = getDoubleFromPy(PyItem);
	PyItem = PyList_GetItem(filterHighList, i);
	highArr[i] = getDoubleFromPy(PyItem);
    }

    /* verify filtTypeList and filtParm1List have same length */
    if (PyList_Size(filtTypeList) != PyList_Size(filtParm1List))
    {
        sprintf(errBuf, "lengths of filter types (%i) and first parameter lists (%i) lists unequal",
	        (int)PyList_Size(filtTypeList), (int)PyList_Size(filtParm1List));
	PyErr_SetString(PyExc_TypeError, errBuf);
        return NULL;
    }
    

    /* create array of strings to hold parm 1 info */
    parm1Arr = (char **)malloc(sizeof(char *)*numFilters);
    if (parm1Arr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }

    for (i=0; i<numFilters; i++)
    {
	
	PyItem = PyList_GetItem(filtParm1List, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL)
	{
	    sprintf(errBuf, "Bad parm in first filter parameters in createMaddata - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	
	parm1Arr[i] = malloc(sizeof(char) * (strlen(thisMnem)+1));
	if (parm1Arr[i] ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
	
	strcpy(parm1Arr[i], thisMnem);
	if (thisMnem != NULL)
	    free(thisMnem);
    }


    /* create array of strings to hold parm 2 info */
    parm2Arr = (char **)malloc(sizeof(char *)*numFilters);
    if (parm2Arr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }
    for (i=0; i<numFilters; i++)
    {
	
	PyItem = PyList_GetItem(filtParm2List, i);
	thisMnem = getMnemFromPy(PyItem);
	if (thisMnem == NULL && PyItem == Py_None)
	{
	    /* for second list NULL is allowed - means no second parameter */
	    parm2Arr[i] = malloc(sizeof(char) * 1);
	}
	else if (thisMnem == NULL && PyItem != Py_None)
	{
	    /* bad parameter passed in */
	    sprintf(errBuf, "Bad parm in second filter parameters in createMaddata - item %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
	    return NULL;
	}
	else
	    parm2Arr[i] = malloc(sizeof(char) * (strlen(thisMnem)+1));
	if (parm2Arr[i] ==  NULL)
	{
	    perror("malloc");
	    exit(-1);
	}
	if (thisMnem == NULL)
	    strcpy(parm2Arr[i], "");
	else
	    strcpy(parm2Arr[i], thisMnem);
	if (thisMnem != NULL)
	    free(thisMnem);
    }
    

    /* create array of ints to hold number of range info */
    numRanArr = (int *)malloc(sizeof(int)*numFilters);
    if (numRanArr ==  NULL)
    {
	perror("malloc");
	exit(-1);
    }
    
    for (i=0; i<PyList_Size(filtNumRangeList); i++)
    {
	PyItem = PyList_GetItem(filtNumRangeList, i);
	thisNumRanges = getIntFromPy(PyItem);
	numRanArr[i] = thisNumRanges;
	numRanges += thisNumRanges;
    }
    
    /* verify numRanges == len(filterLowList) */
    if (numRanges != PyList_Size(filterLowList))
    {
        PyErr_SetString(PyExc_TypeError, "Total number of ranges does not match the number of lower limits");
	return NULL;
    }

    /* create madFiltList */
    madFiltList = createMadfilterList();
    
    for (i=0; i<numFilters; i++)
    {
	/* loop through each filter */

	/* get the number of ranges in this filter */
	thisNumRanges = numRanArr[i];

	/* check that no error has occurred */
	if (numRangesSoFar + thisNumRanges > numRanges)
	{
	    PyErr_SetString(PyExc_TypeError, "Number of range mismatch in createMaddata");
	    return NULL;
	}

	/* get filter type */
	PyItem = PyList_GetItem(filtTypeList, i);
	if (!PyString_Check(PyItem))
	{
	    PyErr_SetString(PyExc_TypeError, "Non string passed into filter type list");
	    return(NULL);
	}
	switch (PyString_AsString(PyItem)[0])
	{
	    case '1':
		filtType = SINGLE_FILT;
		break;
	    case '*':
		filtType = MULT_FILT;
		break;
	    case '/':
		filtType = DIV_FILT;
		break;
	    case '+':
		filtType = ADD_FILT;
		break;
	    case '-':
		filtType = SUB_FILT;
		break;
	    default:
		sprintf(errBuf, "Error in type string in createMaddata: %c",
		        PyString_AsString(PyItem)[0]);
	        PyErr_SetString(PyExc_TypeError, errBuf);
		return NULL;
	}

	/* append next filter */
	if (appendMadfilter(madFiltList,
			    filtType,
			    thisNumRanges,
			    lowArr + numRangesSoFar,
			    highArr + numRangesSoFar,
			    parm1Arr[i],
			    parm2Arr[i]) == -1)
	{
	    PyErr_SetString(PyExc_TypeError, "Problem appending filter in createMaddata");
	    return NULL;
	}
	
	/* prepare for next filter */
	numRangesSoFar += thisNumRanges;
    }
    
    
    /* create the maddata  */
    maddata = createMaddata(filename,
                            inputStr,
                            reqParmList,
                            madFiltList,
                            NULL);

    /* build list of number of records per cycle */
    /* create outputList  list */
    outputList = PyList_New(0);
    for (i=0; i<maddata->numCycles; i++)
    {
	PyList_Append(outputList, 
	              PyInt_FromLong((long)maddata->madCycleList[i]->numMadrecords));
    }

    
    /* clean up */
    destroyMadparmList(reqParmList);
    destroyMadfilterList(madFiltList);
    if (lowArr != NULL)
	free(lowArr);
    if (highArr != NULL)
	free(highArr);
    if (numRanArr != NULL)
	free(numRanArr);
    for (i=0; i<numFilters; i++)
    {
	free(parm1Arr[i]);
	free(parm2Arr[i]);
    }
    if (parm1Arr != NULL)
	free(parm1Arr);
    if (parm2Arr != NULL)
	free(parm2Arr);


    /* success */
    retObj = Py_BuildValue("liO",
			   (long)maddata,
			   maddata->numCycles,
			   outputList);
    return(retObj);
}



/***********************************************************************
*
* destroyMaddata     frees the maddata structure 
*
*   arguments:
*       1. Python int - pointer to malloc'ed data returned by createMaddata
*
*   returns: Python int 0
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_destroyMaddata(PyObject * self, PyObject * args)
{
    Maddata * maddata = NULL;
    long maddatap = 0;
    
    if (!PyArg_ParseTuple(args, "l", &maddatap))
    {
        return NULL;
    }
    maddata = (Maddata *)maddatap;

    destroyMaddata(maddata);

    return Py_BuildValue("i", 0);
}

/***********************************************************************
*
* getMadrecord     gets Madrecord data given cycle and record number
*
*   Be sure to call destroyMaddata when done to free malloc'ed memory
*
*   arguments:
*       1. Python int representing pointer to Maddata
*       2. Python int representing desired cycle (starts at 0)
*       3. Python int representing desired record number (starts at 0)
*
*   returns:
*       Python string - String formated to be printed as a header (contains inst, time, etc)
*       Python string - String formated to be comma-separated list of requested parameter mnemonics
*       Python string - mnemonic string formated to be printed as a label									   
*       Python string - data values. Formatted as will be printed if needed,
*                       with commas separating rows
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_getMadrecord(PyObject * self, PyObject * args)
{
    Maddata * maddata = NULL;
    
    /* input arguments */
    long maddatap = 0;
    int cycNum = 0;
    int recNum = 0;

    /* strings to be built */
    char * headerStr = NULL;
    char * mnemStr = NULL;
    char * labelStr = NULL;
    char * dataStr = NULL;
    
    PyObject *retObj;


    if (!PyArg_ParseTuple(args, "lii", &maddatap, &cycNum, &recNum))
    {
        return NULL;
    }
    maddata = (Maddata *)maddatap;

    /* verify that requested rec exists */
    if (cycNum >= maddata->numCycles)
    {
	PyErr_SetString(PyExc_TypeError, "Too large a cycle number passed to getMadrecord");
        return NULL;
    }
    if (recNum >= maddata->madCycleList[cycNum]->numMadrecords)
    {
	PyErr_SetString(PyExc_TypeError, "Too large a record number passed to getMadrecord");
        return NULL;
    }


    getClassicMadrecordStrings(maddata, 
                               cycNum, 
                               recNum, 
                               32000,
                               "Missing",
                               "assumed",
                               "knownBad",
                               &headerStr,
                               &mnemStr,
                               &labelStr,
                               &dataStr);
    
    retObj = Py_BuildValue("ssss", headerStr, mnemStr, labelStr, dataStr);

    /* free strings returned by getClassicMadrecordStrings */
    free(headerStr);
    free(mnemStr);
    free(labelStr);
    free(dataStr);
    
    return(retObj);
}


/***********************************************************************
*
* madrecOpen     creates a pointer to a madrec struct for later use
*
*   Be sure to call madrecClose when done to free malloc'ed memory
*
*   arguments:
*       1. Python string representing full path to file
*       2. Python mode string (either 'r' to read file or 'w' to create new)
*       3. Python string representing format to save file in.  If mode == 'r',
*          ignored. Otherwise, allowed values are 'Madrigal', 'BlockedBinary',
*          'Cbf', 'UnblockedBinary', and 'Ascii'
*
*   returns:
*       1. Python int - pointer to malloc'ed data.  If 0, failed
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madrecOpen(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int result = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    
    /* input variables */
    char * filename = NULL;
    char * mode = NULL;
    char * format = NULL;

    int formatCode = 0;
    
    /* error string buffer */
    char errBuf[1000] = "";

    /* get input argument */
    if (!PyArg_ParseTuple(args, "sss",
			  &filename,
                          &mode,
                          &format))
    {
        return 0;
    }

    /* more checks on the validity of input */
    if (strcmp(mode, "r") != 0 && strcmp(mode, "w") != 0)
    {
	sprintf(errBuf, "madrecOpen mode argument must be r or w, not <%s>", mode);
	PyErr_SetString(PyExc_TypeError, errBuf);
        return 0;
    }

    if (strcmp(format, "Madrigal") == 0)
        formatCode = 20;

    if (strcmp(format, "BlockedBinary") == 0)
        formatCode = 21;

    if (strcmp(format, "Cbf") == 0)
        formatCode = 22;

    if (strcmp(format, "UnblockedBinary") == 0)
        formatCode = 23;

    if (strcmp(format, "Ascii") == 0)
        formatCode = 24;

    if (strcmp(mode, "w") == 0 && formatCode == 0)
    {
	sprintf(errBuf, "madrecOpen format argument <%s> unknown", format);
	PyErr_SetString(PyExc_TypeError, errBuf);
        return 0;
    }

    madrecp = madrecCreate();

    /* handle read case */
    if (strcmp(mode, "r") == 0)
    {
        result = madrecOpen(madrecp, 1, filename);
        if (result != 0)
        {
            PyErr_SetString(PyExc_TypeError, madrecGetError(madrecp));
            madrecDestroy(madrecp);
            return 0; 
        }
    }
    
    /* handle write case */
    else
    {
        result = madrecOpen(madrecp, formatCode, filename);
        if (result != 0)
        {
            PyErr_SetString(PyExc_TypeError, madrecGetError(madrecp));
            madrecDestroy(madrecp);
            return 0; 
        }
    }

    /* success */
    retObj = Py_BuildValue("l",
			   (long)madrecp);
    return(retObj);
}


/***********************************************************************
*
* madrecClose     closes the madrec file, and frees memory
*
*
*   arguments:
*       1. Python int representing pointer to madrec memory
*
*   returns: 0 
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecClose(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int result = 0;
    long pointer = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    
    /* error string buffer */
    char errBuf[1000] = "";

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* try to close */
    result = madrecClose(madrecp);
    if (result != 0)
    {
	sprintf(errBuf, "problem calling madrecClose - error code %i\n", result);
	PyErr_SetString(PyExc_TypeError, errBuf);
        return 0;
    }

    madrecDestroy(madrecp);

    /* success */
    retObj = Py_BuildValue("i", 0);
    return(retObj);
}


/***********************************************************************
*
* madrecGetNextRec     reads next cedar record into memory, and returns type
*
*
*   arguments:
*       1. Python int representing pointer to madrec memory
*
*   returns: Python int - 0 for data record, 1 for catalog, 2 for header 
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecGetNextRec(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int result = 0;
    int krec = 0;
    long pointer = 0;
    char errStr[1000] = "";

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* try to call madrecGetNextRec */
    result = madrecGetNextRec(madrecp);

    if (result != 0)
    {
        sprintf(errStr, "problem calling madrecGetNextRec - error code %i\n", result);
        PyErr_SetString(PyExc_TypeError, errStr);
        return 0;
    }

    /* determine type */
    if (isDataRecord(madrecp->recordp))
        result = 0;
    
    else
    {
        krec = cedarGetKrec(madrecp->recordp);
        if (krec == 2001 || krec == 2101)
            result = 1;
        else
            result = 2;
    }

    /* success */
    retObj = Py_BuildValue("i", result);
    return(retObj);
}



/***********************************************************************
*
* madrecDumpDataRecord     returns all information from a single Cedar data record
*
*
*   arguments:
*       1. Python int representing pointer to madrec memory
*
*   returns: Python tuple, containing:
*            1. Kinst (integer)
*            2. Kindat (integer)
*            3. Starting time (float - seconds since 1/1/1950)
*            4. Ending time (float - seconds since 1/1/1950)
*            5. List of 1D parameter codes 
*            6. List of 2D parameter codes 
*            7. Number of 2D rows (integer)
*            8. 1D data (python list of floats in same order as list of 1D parameter codes, 
*               or special value strings 'missing', 'assumed', or 'knownbad')
*            9. 2D data (python list of floats in same order as list of 2D parameter codes, 
*               or special value strings 'missing', 'assumed', or 'knownbad', with one row
*               following the next)
*
*   Additional increment parameters will not be included in the returned list of codes and data, since
*   their data is automatically included in the main parameter.  Their existence is simply an
*   artifact of the Cedar file format, and is hidden from the user of this module.
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecDumpDataRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int i = 0, j = 0;
    long pointer = 0;

    /* data to be read */
    int kinst = 0;
    int kindat = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    int jpar = 0;                   /* number of one dim parameters */
    int * oneDCodeList = NULL;      /* list of one dim parameter codes */
    PyObject * oneDParmList;        /* python list of one D parameter codes (integers) to be returned */
    int mpar = 0;                   /* number of two dim parameters */
    int * twoDCodeList = NULL;      /* list of two dim parameter codes */
    PyObject * twoDParmList;        /* python list of two D parameter codes (integers) to be returned */
    PyObject * oneDDataList;        /* python list of one D data to be returned */
    PyObject * twoDDataList;        /* python list of two D data to be returned */
    int nrow = 0;
    double data = 0.0;
    char * desc = NULL;
    int thisCode = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* make sure this is a data record */
    if (!isDataRecord(madrecp->recordp))
    {
        PyErr_SetString(PyExc_TypeError, "Non-data record passed to madrecDumpDataRecord");
        return 0;
    }

    /* start reading data */
    kinst = cedarGetKinst(madrecp->recordp);
    kindat = cedarGetKindat(madrecp->recordp);
    startTime = cedarGetStartIndex(madrecp->recordp);
    endTime = cedarGetEndIndex(madrecp->recordp);

    /* create oneDParmList */
    oneDParmList = PyList_New(0);
    jpar = cedarGetJpar(madrecp->recordp);
    oneDCodeList = cedarGet1dParcodes(madrecp->recordp);

    /* now find which oneD parameters codes are actually additional increment
       parameters and should be ignored */
    for (i=0; i<jpar; i++)
    {
        desc = cedarGetParDescription(oneDCodeList[i]);
        if (strstr(desc, "dditional increment") == NULL)
        {
	    PyList_Append(oneDParmList, PyInt_FromLong((long)oneDCodeList[i]));
        }
        free(desc);
    }
    free(oneDCodeList);

    /* create twoDParmList */
    twoDParmList = PyList_New(0);
    mpar = cedarGetMpar(madrecp->recordp);
    twoDCodeList = cedarGet2dParcodes(madrecp->recordp);

    /* now find which twoD parameters codes are actually additional increment
       parameters and should be ignored */
    for (i=0; i<mpar; i++)
    {
        desc = cedarGetParDescription(twoDCodeList[i]);
        if (strstr(desc, "dditional increment") == NULL)
        {
	    PyList_Append(twoDParmList, PyInt_FromLong((long)twoDCodeList[i]));
        }
        free(desc);
    }
    free(twoDCodeList);


    /* create oneDDataList */
    oneDDataList = PyList_New(0);
    for (i=0; i<PyList_Size(oneDParmList); i++)
    {
        thisCode = (int)PyLong_AsLong(PyList_GetItem(oneDParmList, i));
	
        data = cedarGet1dParm(madrecp->recordp, thisCode);

        /* check special cases */
        if ((data == missing))
            PyList_Append(oneDDataList, PyString_FromString("missing"));
        else if (data == assumed && thisCode < 0)
	    PyList_Append(oneDDataList, PyString_FromString("assumed"));
        else if (data == knownbad && thisCode < 0)
	    PyList_Append(oneDDataList, PyString_FromString("knownbad"));
        /* handle real data */
        else
            PyList_Append(oneDDataList, PyFloat_FromDouble(data));
    }

    /* create twoDDataList (1 + nrow*mpar_real*20*(sizeof(char))) */
    nrow = cedarGetNrow(madrecp->recordp);
    twoDDataList = PyList_New(0);
    for (j=0; j<nrow; j++)
    {
        for (i=0; i<PyList_Size(twoDParmList); i++)
        {
            thisCode = (int)PyLong_AsLong(PyList_GetItem(twoDParmList, i));
	    
	    data = cedarGet2dParmValue(madrecp->recordp, thisCode, j);
	    
	    /* check special cases */
            if ((data == missing))
        	PyList_Append(twoDDataList, PyString_FromString("missing"));
            else if (data == assumed && thisCode < 0)
		PyList_Append(twoDDataList, PyString_FromString("assumed"));
            else if (data == knownbad && thisCode < 0)
		PyList_Append(twoDDataList, PyString_FromString("knownbad"));
            /* handle real data */
            else
        	PyList_Append(twoDDataList, PyFloat_FromDouble(data));
        }
    }
        
    /* success */
    retObj = Py_BuildValue("iiddOOiOO",
                           kinst,
                           kindat,
                           startTime,
                           endTime,
                           oneDParmList,
                           twoDParmList,
                           nrow,
                           oneDDataList,
                           twoDDataList);
        
    return(retObj);
}                                                                          



/***********************************************************************
*
* madrecDumpCatalogRecord     returns all information from a single Cedar catalog record
*
*
*   arguments:
*       1. Python int representing pointer to madrec memory
*
*   returns: Python tuple, containing:
*            1. Kinst (integer)
*            2. Modexp (integer)
*            3. Starting time (float - seconds since 1/1/1950)
*            4. Ending time (float - seconds since 1/1/1950)
*            5. text (multiple of 80 long)
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecDumpCatalogRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int i = 0;
    long pointer = 0;

    /* data to be read */
    int kinst = 0;
    int modexp = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    char * text = NULL;
    char * newText = NULL;
    int numLines = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* make sure this is a catalog record */
    if (cedarGetKrec(madrecp->recordp) != 2001 && cedarGetKrec(madrecp->recordp) != 2101)
    {
        PyErr_SetString(PyExc_TypeError, "Non-catalog record passed to madrecDumpCatalogRecord");
        return 0;
    }

    /* start reading data */
    kinst = cedarGetKinst(madrecp->recordp);
    modexp = cedarGetKindat(madrecp->recordp);
    startTime = cedarGetStartIndex(madrecp->recordp);
    endTime = cedarGetEndIndex(madrecp->recordp);

    text = cedarGetInformation(madrecp->recordp);

    /* remove newlines; copy into newtext */
    newText = malloc((1+ strlen(text))*sizeof(char));
    strcpy(newText, "");
    numLines = strlen(text)/81;
    for (i=0; i<numLines; i++)
        memcpy(newText + (i*80), text + (i*81), 80);
    newText[80*numLines] = '\0';

        
    /* success */
    retObj = Py_BuildValue("iidds",
                           kinst,
                           modexp,
                           startTime,
                           endTime,
                           newText);

    /* free all C resources */
    free(text);
    free(newText);
        
    return(retObj);
}


/***********************************************************************
*
* madrecDumpHeaderRecord     returns all information from a single Cedar header record
*
*
*   arguments:
*       1. Python int representing pointer to madrec memory
*
*   returns: Python tuple, containing:
*            1. Kinst (integer)
*            2. Kindat (integer)
*            3. Starting time (float - seconds since 1/1/1950)
*            4. Ending time (float - seconds since 1/1/1950)
*            5. jpar - number of 1d records in following records
*            6. mpar - number of 2d records in following records
*            5. text (multiple of 80 long)
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecDumpHeaderRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    int i = 0;
    long pointer = 0;

    /* data to be read */
    int kinst = 0;
    int kindat = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    int jpar=0, mpar=0;
    char * text = NULL;
    char * newText = NULL;
    int numLines = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* make sure this is a header record */
    if (cedarGetKrec(madrecp->recordp) != 3002 && cedarGetKrec(madrecp->recordp) != 3101)
    {
        PyErr_SetString(PyExc_TypeError, "Non-catalog record passed to madrecDumpHeaderRecord");
        return 0;
    }

    /* start reading data */
    kinst = cedarGetKinst(madrecp->recordp);
    kindat = cedarGetKindat(madrecp->recordp);
    startTime = cedarGetStartIndex(madrecp->recordp);
    endTime = cedarGetEndIndex(madrecp->recordp);
    jpar = cedarGetJpar(madrecp->recordp);
    mpar = cedarGetMpar(madrecp->recordp);

    text = cedarGetInformation(madrecp->recordp);

    /* remove newlines; copy into newtext */
    newText = malloc((1+ strlen(text))*sizeof(char));
    strcpy(newText, "");
    numLines = strlen(text)/81;
    for (i=0; i<numLines; i++)
        memcpy(newText + (i*80), text + (i*81), 80);
    newText[80*numLines] = '\0';

        
    /* success */
    retObj = Py_BuildValue("iiddiis",
                           kinst,
                           kindat,
                           startTime,
                           endTime,
                           jpar,
                           mpar,
                           newText);

    /* free all C resources */
    free(text);
    free(newText);
        
    return(retObj);
}


/***********************************************************************
*
* madrecCreateDataRecord     creates a single Cedar data record
*
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*            2. format (string)
*            3. Kinst (integer)
*            4. Kindat (integer)
*            5. Starting time (float - seconds since 1/1/1950)
*            6. Ending time (float - seconds since 1/1/1950)
*            7. Number of 1D parameters (jpar) (integer)
*            8. Number of 2D parameters (mpar) (integer)
*            9. Number of 2D rows (integer)
*
*   Returns: 0 if success, -1 if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecCreateDataRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    /* data to be read */
    int lprol = 0;
    int kinst = 0;
    int kindat = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    int jpar = 0;                   /* number of one dim parameters */
    int mpar = 0;                   /* number of two dim parameters */
    int nrow = 0;
    char * format = NULL;
    int result = 0;
    int krec = 0;

    /* time parameters */
    int year1 = 0;
    int month1 = 0;
    int day1 = 0;
    int hour1 = 0;
    int minute1 = 0;
    int second1 = 0;
    int centisecond1 = 0;
    int year2 = 0;
    int month2 = 0;
    int day2 = 0;
    int hour2 = 0;
    int minute2 = 0;
    int second2 = 0;
    int centisecond2 = 0;
    int iyr = 0;
    int imd = 0;
    int ihm = 0;
    int ics = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "lsiiddiii",
			  &pointer,
                          &format,
                          &kinst,
                          &kindat,
                          &startTime,
                          &endTime,
                          &jpar,
                          &mpar,
                          &nrow))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    if (madrecp->recordp != (Int16 *)NULL)
        free(madrecp->recordp);

    /* figure out krec */
    if (strcmp(format, "Ascii") == 0)
        krec = 1101;
    else
        krec = 1002;

    /* set time parameters */
    if(dinvmadptr(startTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year1 = iyr;
    month1 = imd/100;
    day1 = imd - month1*100;
    hour1 = ihm/100;
    minute1 = ihm - hour1*100;
    second1 = ics/100;
    centisecond1 = ics - second1*100;

    if(dinvmadptr(endTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year2 = iyr;
    month2 = imd/100;
    day2 = imd - month2*100;
    hour2 = ihm/100;
    minute2 = ihm - hour2*100;
    second2 = ics/100;
    centisecond2 = ics - second2*100;
    
    if (krec == 1101)
        lprol = 16;
    else
        lprol = 21;
    
    madrecp->recordp = cedarCreateRecord(lprol, jpar, mpar, nrow,
                         krec, kinst, kindat,
                         year1, month1, day1,
                         hour1, minute1, second1, centisecond1,
                         year2, month2, day2,
                         hour2, minute2, second2, centisecond2);

    if (madrecp->recordp == NULL)
        result = -1;
        
    /* success */
    retObj = Py_BuildValue("i",
                           result);

        
    return(retObj);
}


/***********************************************************************
*
* cedarSet1dParm     sets a single 1d parm in a cedar record
*
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*            2. parameter code (integer)
*            3. value (string) - either a number, or missing, assumed, or knownbad
*            4. index (integer) - which 1d parameter
*
*   Returns: 0 if success, -1 if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_cedarSet1dParm(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    /* data to be read */
    int code = 0;
    char * value = NULL;
    double doubleValue = 0.0;
    int index = 0;
    int result = 0;
    
    /* madrec object */
    Madrec * madrecp = NULL;

    /* get input argument */
    if (!PyArg_ParseTuple(args, "lisi",
			  &pointer,
                          &code,
                          &value,
                          &index))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* check for special values */
    if (strcmp(value, "missing") == 0)
        result = cedarSet1dInt(madrecp->recordp, code, -32767, index);

    else if (strcmp(value, "assumed") == 0 && (code < 0))
        result = cedarSet1dInt(madrecp->recordp, code, -32766, index);

    else if (strcmp(value, "knownbad") == 0 && (code < 0))
        result = cedarSet1dInt(madrecp->recordp, code, 32767, index);

    else
    {
        doubleValue = strtod(value, NULL);
        result = cedarSetNorm1dParm(madrecp->recordp, code, doubleValue, index);
    }
        
    retObj = Py_BuildValue("i",
                           result);

        
    return(retObj);
}


/***********************************************************************
*
* cedarSet2dParm     sets all rows for a single 2d parm in a cedar record
*
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*            2. parameter code (integer)
*            3. values (python list of doubles - length = nrows.
*                 May also include the values 'missing', 'assumed', 'knownbad'
*            4. index (integer) - which 2d parameter
*
*   Returns: 0 if success, -1 if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_cedarSet2dParm(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    /* data to be read */
    int code = 0, i=0;
    PyObject * inputList;
    PyObject * PyItem;      /* list item */
    double * valueList = NULL;
    int index = 0;
    int result = 0;
    int nrow = 0;
    
    /* error string buffer */
    char errBuf[1000] = "";
    
    /* madrec object */
    Madrec * madrecp = NULL;

    /* get input argument */
    if (!PyArg_ParseTuple(args, "liO!i",
			  &pointer,
                          &code,
                          &PyList_Type, &inputList,
                          &index))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    /* convert value string into an array of doubles */
    nrow = PyList_Size(inputList);
    valueList = (double *)malloc((nrow)*sizeof(double));
    
    for (i=0; i<nrow; i++)
    {
        PyItem = PyList_GetItem(inputList, i);
	
	/* check for special values */
	if (PyString_Check(PyItem))
	{
	    if (strcmp(PyString_AsString(PyItem), "missing")==0)
                valueList[i] = missing;
	    else if (strcmp(PyString_AsString(PyItem), "assumed")==0)
                valueList[i] = assumed;
	    else if (strcmp(PyString_AsString(PyItem), "knownbad")==0)
                valueList[i] = knownbad;
            else
            {
                sprintf(errBuf, "Bad special parameter indicator <%s> in cedarSet2dParm, position %i", 
		                PyString_AsString(PyItem), i);
	        PyErr_SetString(PyExc_TypeError, errBuf);
		return(0);
            }
	}
	else if (PyFloat_Check(PyItem))
	    valueList[i] = PyFloat_AsDouble(PyItem);
	else if (PyLong_Check(PyItem))
	    valueList[i] = (double)PyLong_AsLong(PyItem);
	else if (PyInt_Check(PyItem))
	    valueList[i] = (double)PyInt_AsLong(PyItem);
        else
	{    
	    sprintf(errBuf, "Bad item passed to cedarSet2dParm at position %i", i);
	    PyErr_SetString(PyExc_TypeError, errBuf);
            return(0);
        }   
    }

    if (nrow > 0)
        result = cedarSetNorm2dParm(madrecp->recordp, code, valueList, index);
        
    retObj = Py_BuildValue("i", result);

    free(valueList);

    return(retObj);
}
                                                                           

/***********************************************************************
*
* madrecPutNextRec     writes the existing record to disk
*
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*
*   Returns: 0 if success, non-zero if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecPutNextRec(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    int result = 0;
    
    /* madrec object */
    Madrec * madrecp = NULL;

    /* get input argument */
    if (!PyArg_ParseTuple(args, "l",
			  &pointer))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    result = madrecPutNextRec (madrecp);
        
    retObj = Py_BuildValue("i",
                           result);

        
    return(retObj);
}



/***********************************************************************
*
* madrecCreateCatalogRecord     creates a cedar catalog record
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*            2. format (string)
*            3. Kinst (integer)
*            4. Modexp (integer)
*            5. Starting time (float - seconds since 1/1/1950)
*            6. Ending time (float - seconds since 1/1/1950)
*            7. text (string - multiple of 80 characters, no linefeeds)
*
*   Returns: 0 if success, -1 if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecCreateCatalogRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    /* data to be read */
    int kinst = 0;
    int modexp = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    char * format = NULL;
    char * text = NULL;
    int result = 0;
    int krec = 0;

    /* time parameters */
    int year1 = 0;
    int month1 = 0;
    int day1 = 0;
    int hour1 = 0;
    int minute1 = 0;
    int second1 = 0;
    int centisecond1 = 0;
    int year2 = 0;
    int month2 = 0;
    int day2 = 0;
    int hour2 = 0;
    int minute2 = 0;
    int second2 = 0;
    int centisecond2 = 0;
    int iyr = 0;
    int imd = 0;
    int ihm = 0;
    int ics = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "lsiidds",
			  &pointer,
                          &format,
                          &kinst,
                          &modexp,
                          &startTime,
                          &endTime,
                          &text))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    if (madrecp->recordp != (Int16 *)NULL)
        free(madrecp->recordp);

    /* figure out krec */
    if (strcmp(format, "Ascii") == 0)
        krec = 2101;
    else
        krec = 2001;

    /* set time parameters */
    if(dinvmadptr(startTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year1 = iyr;
    month1 = imd/100;
    day1 = imd - month1*100;
    hour1 = ihm/100;
    minute1 = ihm - hour1*100;
    second1 = ics/100;
    centisecond1 = ics - second1*100;

    if(dinvmadptr(endTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year2 = iyr;
    month2 = imd/100;
    day2 = imd - month2*100;
    hour2 = ihm/100;
    minute2 = ihm - hour2*100;
    second2 = ics/100;
    centisecond2 = ics - second2*100;
    
    madrecp->recordp = cedarCreateCatalogRecord(kinst, modexp,
                                year1, month1, day1,
                                hour1, minute1, second1, centisecond1,
                                year2, month2, day2,
                                hour2, minute2, second2, centisecond2,
				text);

    if (madrecp->recordp == NULL)
        result = -1;
        
    /* success */
    retObj = Py_BuildValue("i",
                           result);

        
    return(retObj);
}


/***********************************************************************
*
* madrecCreateHeaderRecord     creates a cedar header record
*
*   arguments:
*            1. madrec pointer created by madrecOpen (integer)
*            2. format (string)
*            3. Kinst (integer)
*            4. Kindat (integer)
*            5. Starting time (float - seconds since 1/1/1950)
*            6. Ending time (float - seconds since 1/1/1950)
*            7. jpar (integer) - number of 1d parms
*            8. mpar (integer) - number of 2d parms                                                                       
*            9. text (string - multiple of 80 characters, no linefeeds)
*
*   Returns: 0 if success, -1 if failure
*
*   throws:
*       PyExc_TypeError if error
*
*/
static PyObject * _Madrec_madrecCreateHeaderRecord(PyObject * self, PyObject * args)
{
    PyObject *retObj;
    long pointer = 0;

    /* data to be read */
    int kinst = 0;
    int kindat = 0;
    double startTime = 0.0;
    double endTime = 0.0;
    int jpar=0, mpar=0;
    char * format = NULL;
    char * text = NULL;
    int result = 0;
    int krec = 0;

    /* time parameters */
    int year1 = 0;
    int month1 = 0;
    int day1 = 0;
    int hour1 = 0;
    int minute1 = 0;
    int second1 = 0;
    int centisecond1 = 0;
    int year2 = 0;
    int month2 = 0;
    int day2 = 0;
    int hour2 = 0;
    int minute2 = 0;
    int second2 = 0;
    int centisecond2 = 0;
    int iyr = 0;
    int imd = 0;
    int ihm = 0;
    int ics = 0;

    /* madrec object */
    Madrec * madrecp = NULL;
    

    /* get input argument */
    if (!PyArg_ParseTuple(args, "lsiiddiis",
			  &pointer,
                          &format,
                          &kinst,
                          &kindat,
                          &startTime,
                          &endTime,
                          &jpar,
                          &mpar,
                          &text))
    {
        return 0;
    }

    /* cast int to madrecp */
    madrecp = (Madrec *)pointer;

    if (madrecp->recordp != (Int16 *)NULL)
        free(madrecp->recordp);

    /* figure out krec */
    if (strcmp(format, "Ascii") == 0)
        krec = 3101;
    else
        krec = 3002;

    /* set time parameters */
    if(dinvmadptr(startTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year1 = iyr;
    month1 = imd/100;
    day1 = imd - month1*100;
    hour1 = ihm/100;
    minute1 = ihm - hour1*100;
    second1 = ics/100;
    centisecond1 = ics - second1*100;

    if(dinvmadptr(endTime, &iyr, &imd, &ihm, &ics))
        result = -1;
    year2 = iyr;
    month2 = imd/100;
    day2 = imd - month2*100;
    hour2 = ihm/100;
    minute2 = ihm - hour2*100;
    second2 = ics/100;
    centisecond2 = ics - second2*100;
    
    madrecp->recordp = cedarCreateHeaderRecord(kinst, kindat,
                                year1, month1, day1,
                                hour1, minute1, second1, centisecond1,
                                year2, month2, day2,
                                hour2, minute2, second2, centisecond2,
                                jpar, mpar,
				text);

    if (madrecp->recordp == NULL)
        result = -1;
        
    /* success */
    retObj = Py_BuildValue("i",
                           result);

        
    return(retObj);
}


                                                                           
                                                                           
/***********************************************************************
*
* cedarGetParCodeType     gets cedar parameter code type description
*
*   arguments:
*       Python integer for cedar parameter code 
*
*   returns:
*       Python String giving Cedar parameter code Type
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_cedarGetParCodeType(PyObject * self, PyObject * args)
{
    int NCAR;
    char * categoryName;
    PyObject *retObj;


    if (!PyArg_ParseTuple(args, "i", &NCAR))
    {
        return NULL;
    }
    categoryName = cedarGetParCodeType(NCAR);


    retObj = Py_BuildValue("s", categoryName);
    free(categoryName);
    return(retObj);
}



/***********************************************************************
*
* madGetParMnemType     gets Madrigal parameter type description given mnemonic
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Cedar parameter code Type
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParMnemType(PyObject * self, PyObject * args)
{
    char * categoryName;
    char *mnem = "";
    PyObject *retObj;


    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    categoryName = madGetParMnemType(mnem);


    retObj = Py_BuildValue("s", categoryName);
    free(categoryName);
    return(retObj);
}


/***********************************************************************
*
* madGetParType     gets Madrigal parameter type given mnemonic (1 for std mnem, 0 for error,
*                   and -1 if not found)
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python int: 1 for std mnem, 0 for error parameter, and -1 if not found
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParType(PyObject * self, PyObject * args)
{
    char *mnem = "";
    int result = 0;
    PyObject *retObj;


    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    result = isMadparmError(mnem);

    if (result == 0)
	retObj = Py_BuildValue("i", 1);
    else if (result == 1)
	retObj = Py_BuildValue("i", 0);
    else
	retObj = Py_BuildValue("i", -1);

    return(retObj);
}



/***********************************************************************
*
* cedarGetParMnemonic     gets cedar parameter code mnemonic
*
*   arguments:
*       Python integer for cedar parameter code
*
*   returns:
*       Python String giving Cedar parameter code mnemonic
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_cedarGetParMnemonic(PyObject * self, PyObject * args)
{
    int NCAR;
    char * mnemonic;
    PyObject *retObj;
    
    if (!PyArg_ParseTuple(args, "i", &NCAR))
    {
        return NULL;
    }

    mnemonic = cedarGetParMnemonic(NCAR);
	  
    retObj = Py_BuildValue("s", mnemonic);
    free(mnemonic);
    return(retObj);    
}



/***********************************************************************
*
* cedarGetParDescription     gets cedar parameter code description, including  units
*
*   arguments:
*       Python integer for cedar parameter code
*
*   returns:
*       Python String giving Cedar parameter code description, with units
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_cedarGetParDescription(PyObject * self, PyObject * args)
{
    int NCAR;
    char description[1000];
    char * descStr;
    char * unitStr; 

    if (!PyArg_ParseTuple(args, "i", &NCAR))
    {
        return NULL;
    }
    
    descStr = cedarGetParDescription(NCAR);
    unitStr = cedarGetParUnits(NCAR);
    sprintf(description, 
            "%s  -  Units: %s",
	    descStr,
	    unitStr);

    free(descStr);
    free(unitStr);
	  
    return Py_BuildValue("s", description);
}

                                                                           
/***********************************************************************
*
* cedarGetParScaleFactor     gets cedar parameter scaling factor
*
*   arguments:
*       Python integer for cedar parameter code
*
*   returns:
*       Python double giving Cedar parameter scaling factor
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_cedarGetParScaleFactor(PyObject * self, PyObject * args)
{
    int NCAR;
    double scale = 0.0; 

    if (!PyArg_ParseTuple(args, "i", &NCAR))
    {
        return NULL;
    }
    
    scale = cedarGetParScaleFactor(NCAR);

	  
    return Py_BuildValue("f", scale);
}
                                                                           

/***********************************************************************
*
* madGetParDescription     gets Madrigal parameter description, including  units
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Cedar parameter code description, with units
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParDescription(PyObject * self, PyObject * args)
{

    char description[1000];
    char * descStr;
    char * unitStr;
    char *mnem = "";   

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    
    descStr = madGetParDescription(mnem);
    unitStr = madGetParUnits(mnem);
    sprintf(description, 
            "%s  -  Units: %s",
	    descStr,
	    unitStr);
    
    free(descStr);
    free(unitStr);
	  
    return Py_BuildValue("s", description);
}

    

/***********************************************************************
*
* madGetSimpleParDescription     gets Madrigal parameter description, without units
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Cedar parameter code description, without units
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetSimpleParDescription(PyObject * self, PyObject * args)
{

    char description[1000];
    char * descStr;
    char *mnem = "";   

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    
    descStr = madGetParDescription(mnem);
    sprintf(description, 
            "%s",
	    descStr);
    
    free(descStr);
	  
    return Py_BuildValue("s", description);
}

/***********************************************************************
*
* madGetParUnits     gets Madrigal parameter units string
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Cedar parameter units string
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParUnits(PyObject * self, PyObject * args)
{

    char units[1000];
    char * unitStr;
    char *mnem = "";   

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    
    unitStr = madGetParUnits(mnem);
    sprintf(units, 
            "%s",
	    unitStr);
    
    free(unitStr);
	  
    return Py_BuildValue("s", units);
}


/***********************************************************************
*
* madGetParFormat     gets Madrigal parameter format string
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Madrigal parameter format string
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParFormat(PyObject * self, PyObject * args)
{

    char format[1000];
    char * formatStr;
    char *mnem = "";   

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    
    formatStr = madGetParFormat(mnem);
    if (formatStr == NULL)
    {
	PyErr_SetString(PyExc_TypeError, "mnem not found in parcods.tab");
        return NULL;
    }
    sprintf(format, 
            "%s",
	    formatStr);
    
	  
    return Py_BuildValue("s", format);
}


/***********************************************************************
*
* madGetParWidth     gets Madrigal parameter width
*
*   arguments:
*       Python string mnemonic
*
*   returns:
*       Python String giving Madrigal parameter width (number of spaces used in isprint)
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetParWidth(PyObject * self, PyObject * args)
{

    int width = 0;
    char *mnem = "";   

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }
    
    width = madGetParWidth(mnem);
    
	  
    return Py_BuildValue("i", width);
}


/***********************************************************************
*
* cedarGetParCodeFromMnemonic     gets cedar parameter code from mnemonic string
*
*   arguments:
*       Python string for cedar parameter mnemonic (case insensitive)
*
*   returns:
*       Python Integer giving Cedar parameter code, -32767 if not found
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_cedarGetParCodeFromMnemonic(PyObject * self, PyObject * args)
{
    int parameter;
    char * mnem;

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }

    parameter = cedarGetParCodeFromMnemonic(mnem);
	

    return Py_BuildValue("i", parameter);

}


/***********************************************************************
*
* madGetCategoryIndex     gets Madrigal category index for category name
*
*   arguments:
*       Python string for Madrigal category name (case, whitespace sensitive)
*
*   returns:
*       Python Integer giving Madrigal Category index, -32767 if not found
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madGetCategoryIndex(PyObject * self, PyObject * args)
{
    int index;
    char * catname;

    if (!PyArg_ParseTuple(args, "s", &catname))
    {
        return NULL;
    }

    index = madGetCategoryIndex(catname);

    return Py_BuildValue("i", index);

}


/***********************************************************************
*
* cedarGetInformation     returns header or catalog info from a particular
*                        cedar record as a string
*
*   arguments:
*       Python string representing full path to file
*       Python integer representing the record number in the file to examine
*
*   returns:
*       Python string containing all the text in the header or catalog record.
*       If not a catalog or header record, or if no text found, returns an empty
*       string.  Each line in the header or catalog record will end with a newline.
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*       PyExc_IOError if unable to open madrigal file
*
*/
static PyObject * _Madrec_cedarGetInformation(PyObject * self, PyObject * args)
{
    char * filename;                 // full path to file
    char errString[ERR_STR_LEN];
    int  errInt;
    Madrec * madrecp;
    int recNum;                      // sets which record to examine (0 is first)
    
    int recCount = 0;                // counts records examined so far
    char * infoStr = "";             // pointer to returned string allocated by libmadrec
    PyObject *retObj;

    
    // get file name and record number
    if (!PyArg_ParseTuple(args, "si", &filename, &recNum))
    {
        return NULL;
    }
	
    /* Create a madrec object */
    madrecp = madrecCreate();

    // try to open madrigal file
    errInt = madrecOpen(madrecp, 1, filename);
    if (errInt)
    {
    	sprintf(errString, "Unable to open %s, due to error %i", filename, errInt);
        PyErr_SetString(PyExc_IOError, errString);
        return NULL;
    }
    
    // loop through file
    while(madrecGetNextRec(madrecp) == 0)
    {
	// check if its the right record
        if (recNum > recCount)
	{
	    recCount++;
	    continue;
	}
	    
	// correct record found
    	infoStr = cedarGetInformation(madrecp->recordp);
	break;
    }
    
    // close file
    madrecClose(madrecp);
    
    // destroy madrec
    madrecDestroy(madrecp);
    
    retObj = Py_BuildValue("s", infoStr);
    free(infoStr);
    return(retObj);
}


/***********************************************************************
*
*  cedarCatalogHeaderList    returns a tuple containing:
*                                - list of catalog records in file
*                                - list of header records in file
*
*   arguments:
*       Python string representing full path to file
*
*   returns:
*       Python tuple containing two items:
*          1. list of catalog records in file (record num integers, first is 0)
*          2. list of header records in file (record num integers, first is 0)                                                                           
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*       PyExc_IOError if unable to open madrigal file
*
*/
static PyObject * _Madrec_cedarCatalogHeaderList(PyObject * self, PyObject * args)
{
    char * filename;                 // full path to file
    char errString[ERR_STR_LEN];
    int  errInt;
    Madrec * madrecp;
    int krec = 0;                    // krec from prolog

    int recCount = 0;                // counts total records found so far
    
    PyObject *retObj;

    // create an empty python list to hold catalog recno values
    PyObject * retCatalogList;
    
    //create an empty python list to hold header recno values
    PyObject * retHeaderList;

    // initialize python lists
    retCatalogList = PyList_New(0);
    retHeaderList = PyList_New(0);

    
    // get file name and record number
    if (!PyArg_ParseTuple(args, "s", &filename))
    {
        return NULL;
    }
	
    /* Create a madrec object */
    madrecp = madrecCreate();

    // try to open madrigal file
    errInt = madrecOpen(madrecp, 1, filename);
    if (errInt)
    {
    	sprintf(errString, "Unable to open %s, due to error %i", filename, errInt);
        PyErr_SetString(PyExc_IOError, errString);
        return NULL;
    }
    
    // loop through file
    while(madrecGetNextRec(madrecp) == 0)
    {
        krec = cedarGetKrec(madrecp->recordp);
	// check if its a catalog record
        if (krec == BINARY_CAT_KREC || krec == CHAR_CAT_KREC)
            PyList_Append(retCatalogList, PyInt_FromLong(recCount));

        // else check if its a header record
        else if (krec == BINARY_HEAD_KREC || krec == CHAR_HEAD_KREC)
            PyList_Append(retHeaderList, PyInt_FromLong(recCount));
	    
	recCount++;
    }
    
    // close file
    madrecClose(madrecp);
    
    // destroy madrec
    madrecDestroy(madrecp);
    
    retObj = Py_BuildValue("OO", retCatalogList, retHeaderList);
    return(retObj);
}



/***********************************************************************
*
* madHasHtmlDesc     returns 1 if parameters has Html description, 0 if not
*
*   arguments:
*       Python string for Madrigal parameter name (case insensitive)
*
*   returns:
*       Python Integer 1 if parameter has Html description, 0 if not (or if not found)
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_madHasHtmlDesc(PyObject * self, PyObject * args)
{
    char * mnem;;

    if (!PyArg_ParseTuple(args, "s", &mnem))
    {
        return NULL;
    }

    return Py_BuildValue("i", madHasHtmlDesc(mnem));
}



/***********************************************************************
*
* getUtFromDate     returns the number of seconds as a float since 1/1/1950
*
*   arguments:
*       Year as Python int
*       Month as Python int
*       Day as Python int
*       Hour as Python int
*       Min as Python int
*       Sec as Python int
*       Centisec as Python int
*
*   returns:
*       Python double giving the number of seconds as a float since 1/1/1950
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_getUtFromDate(PyObject * self, PyObject * args)
{

    int year = 0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int min = 0;
    int sec = 0;
    int centisec = 0;

    double ut = 0.0;
    
    PyObject *retObj;

    
    // get date arguments
    if (!PyArg_ParseTuple(args, "iiiiiii", &year, &month, &day, &hour, &min, &sec, &centisec))
    {
        return NULL;
    }

    ut = getKey(year, month, day, hour, min, sec);

    /* add centiseconds */
    ut += centisec/100.0;

    retObj = Py_BuildValue("d", ut);
    return(retObj);
}


/***********************************************************************
*
* getDateFromUt     returns a date as a list of integers given ut as seconds since 1/1/1950
*
*   arguments:
*       Python double giving the number of seconds as a float since 1/1/1950									   
*
*   returns: list containing
*       Year as Python int
*       Month as Python int
*       Day as Python int
*       Hour as Python int
*       Min as Python int
*       Sec as Python int
*       Centisec as Python int
*       Day of year as Python int
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_getDateFromUt(PyObject * self, PyObject * args)
{

    int year = 0;
    int month = 0;
    int day = 0;
    int imd = 0;
    int hour = 0;
    int min = 0;
    int ihm = 0;
    int sec = 0;
    int centisec = 0;
    int ics = 0;
    int dayOfYear = 0;

    double ut = 0.0;
    
    PyObject * retDateList;
    
    // get ut
    if (!PyArg_ParseTuple(args, "d", &ut))
    {
        return NULL;
    }

    dinvmadptr(ut, &year, &imd, &ihm, &ics);

    month = imd/100;
    day = imd - month*100;

    hour = ihm/100;
    min = ihm - hour*100;

    sec = ics/100;
    centisec = ics - sec*100;

    dayOfYear = madGetDayno(year, month, day);
    
    retDateList = PyList_New(0);
    
    PyList_Append(retDateList, PyInt_FromLong(year));
    PyList_Append(retDateList, PyInt_FromLong(month));
    PyList_Append(retDateList, PyInt_FromLong(day));
    PyList_Append(retDateList, PyInt_FromLong(hour));
    PyList_Append(retDateList, PyInt_FromLong(min));
    PyList_Append(retDateList, PyInt_FromLong(sec));
    PyList_Append(retDateList, PyInt_FromLong(centisec));
    PyList_Append(retDateList, PyInt_FromLong(dayOfYear));
    
    return(retDateList);
}


/***********************************************************************
*
* convertGeodeticGeomagnetic     converts geodetic <--> corrected geomagnetic coords
*
*   arguments:
*       Python double giving input latitude (geodetic or geomagnetic)
*       Python double giving input longitude (geodetic or geomagnetic)
*       Python double giving altitude of point in km
*       Python integer giving year
*       Python integer specifying direction 
*                  1:  convert geodetic to corrected geomagnetic coordinates
*                 -1:  convert corrected geomagnetic coordinates to geodetic
*
*   returns: list containing
*      Python double giving output latitude (geodetic or geomagnetic)
*       Python double giving output longitude (geodetic or geomagnetic) 
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_convertGeodeticGeomagnetic(PyObject * self, PyObject * args)
{

    double orgLat, orgLon, alt;
    double newLat, newLon;
    int direction, year;
    double gclat, rkm;
    double dat[44]; /* geocgm01_ returns 11 parameters (slar, slor, clar, clor,
                       rbm, btr, bfr, brr, ovl, azm, utm) organized for the start point
                       (*,1), its conjugate point (*,2), then for the footprints at 1-Re
                       of the start (*,3) and conjugate (*,4) points - however, to speed
		       the code up, I return after clar, clor calculated for original point,
		       so no other data available*/
    double pla[4];
    double plo[4];    /* pla  = array of geocentric latitude and
                         plo  = array of geocentric longitudes for the CGM poles
                         in the Northern and Southern hemispheres at a given
                         altitude (indices 1 and 2) and then at the Earth's
                         surface - 1-Re or zero altitude - (indices 3 and 4) */	
			 	       
    PyObject * retList;
    
    /* error string buffer */
    char errString[1000] = "";
    
    // get arguments
    if (!PyArg_ParseTuple(args, "dddii", &orgLat, &orgLon, &alt, &year, &direction))
    {
        return NULL;
    }
    
    if (orgLat < -90.1 || orgLat > 90.1)
    {
        sprintf(errString, "Input lat must be between -90 and 90, not %f", orgLat);
	PyErr_SetString(PyExc_TypeError, errString);
	return NULL; 
    }
    
    if (orgLon < 0.0)
        orgLon += 360.0;
	
    if (orgLon < 0.0 || orgLon > 360.1)
    {
        sprintf(errString, "Invalid input longitude: %f", orgLon);
	PyErr_SetString(PyExc_TypeError, errString);
	return NULL; 
    }
    
    if (alt < 0.1)
    {
        sprintf(errString, "Invalid input altitude: %f", alt);
	PyErr_SetString(PyExc_TypeError, errString);
	return NULL;
    }
    
    if (direction != 1 && direction != -1)
    {
        sprintf(errString, "Invalid direction - must be 1 for GEO->CGM, or -1 for CGM->GEO, not %i", direction);
	PyErr_SetString(PyExc_TypeError, errString);
	return NULL;
    }
    
    if (year < 1950)
    {
        sprintf(errString, "Invalid year, must be > 1950, not %i", year);
	PyErr_SetString(PyExc_TypeError, errString);
	return NULL;
    }
    
    if (direction == 1)
    {
        /* geodetic to geomagnetic */
        /* first convert to geocentric */
        convrt_(&direction, &orgLat, &alt, &gclat, &rkm);
	
	/* write gclat and glon into dat */
        dat[0] = gclat;
        dat[1] = orgLon;
	
        geocgm01_(&direction, &year, &alt, dat, pla, plo);
	/* CGM_LAT */
        newLat = dat[2];
        /* CGM_LONG */
        newLon = dat[3];
    }
    else
    {
        /* write cgmlat and cgmlon into dat */
        dat[2] = orgLat;
        dat[3] = orgLon;
        geocgm01_(&direction, &year, &alt, dat, pla, plo);
	
	gclat = dat[0];
	newLon = dat[1];
        
        /* get radius */
        rkm = alt + 6378.137;
        
        /* geocentric to geodetic */
	direction = 2;
        convrt_(&direction, &newLat, &alt, &gclat, &rkm);
    }

    retList = PyList_New(0);
    
    PyList_Append(retList, PyFloat_FromDouble(newLat));
    PyList_Append(retList, PyFloat_FromDouble(newLon));
    
    return(retList);
}



/***********************************************************************
*
* pyTraceMagneticField     returns the endpoint of a magnetic line
*
*   Traces to conjugate point, intersection with a given altitude in the
*   northern or southern hemisphere, to the apex, or to GSM XY plane, depending on qualifier
*   argument.  Uses Tsyganenko or IGRF fields, depending on model argument.
*   Input arguments are either GSM or Geodetic, depending on inputType argument.
*   Output arguments are either GSM or Geodetic, depending on outputType 
*   argument.
*
*   arguments:
*      Python int - year
*      Python int - month
*      Python int - day
*      Python int - hour
*      Python int - min
*      Python int - sec
*      Python int - inputType (0 for geodetic, 1 for GSM)
*      Python int - outputType (0 for geodetic, 1 for GSM)
*          (the following parameter depend on inputType)
*      double in1 - geodetic altitude or ZGSM of starting point
*      double in2 - geodetic latitude or XGSM of starting point
*      double in3 - longitude or YGSM of starting point
*      int model - 0 for Tsyganenko, 1 for IGRF
*      int qualifier - 0 for conjugate, 1 for north_alt, 2 for south_alt, 3 for apex, 4 for GSM XY plane
*      double * stopAlt - altitude to stop trace at, if qualifier is north_alt or south_alt.
*                         If other qualifier, this parameter is ignored
*
*   returns: list of three doubles, whose meaning depends on outputType
*      double * end_1 - geodetic altitude or ZGSM of starting point
*      double * end_2 - geodetic latitude or XGSM of starting point
*      double * end_3 - longitude or YGSM of starting point
*  
*   If trace fails, all three return values will be set to missing  									   
*
*
*   throws:
*       PyExc_TypeError if illegal argument passed
*
*/
static PyObject * _Madrec_pyTraceMagneticField(PyObject * self, PyObject * args)
{

    int year = 0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int min = 0;
    int sec = 0;
    int inputType = 0;
    int outputType = 0;
    double in1 = 0.0;
    double in2 = 0.0;
    double in3 = 0.0;
    int model = 0;
    int qualifier = 0;
    double stopAlt = 0.0;
    
    double out1 = 0.0;
    double out2 = 0.0;
    double out3 = 0.0;
    
    /* temporary variable for holding position during conversion */
    double dim1 = 0.0;
    double dim2 = 0.0;
    double dim3 = 0.0;
    
    int imod = 0; /* flag to set conversion direction */
    int result = 0;
    int dayOfYear = 0;

    
    PyObject * retList;
    
    // get arguments
    if (!PyArg_ParseTuple(args, "iiiiiiiidddiid", &year,
                                                  &month,
						  &day,
						  &hour,
						  &min,
						  &sec,
						  &inputType,
						  &outputType,
						  &in1,
						  &in2,
						  &in3,
						  &model,
						  &qualifier,
						  &stopAlt))
    {
        return NULL;
    }
    
    /* get day of year as required by Tsyganenko */
    dayOfYear = madGetDayno(year, month, day);
    
    /* set Tsyganenko common block data to the right time - not thread safe!!!! */
    TS_RECALC_F77(&year, &dayOfYear, &hour, &min, &sec);
    
    /* convert in* to geodetic if needed */
    if (inputType == 1)
    {
        imod = -1; /* converst gsm to cartesian */
	geogsm_(&dim1, &dim2, &dim3, &in2, &in3, &in1, &imod);
	/* now dim1 XGEO, dim2 YGEO, dim3 ZGEO */
	/* convert cartesian to spherical */
	TS_SPHCAR_F77(&in1, &in2, &in3, &dim1, &dim2, &dim3, &imod);
	/* now in1 R, in2 theta, in3 phi */
	/* convert spherical to geocentric */
        dim1 = in1*6371.2; 
	dim2 = 90.0 - (in2/0.01745329);
	dim3 = in3/0.01745329;
	imod = 2; /* convert geocentric coordinates to geodetic */
	in3 = dim3;
    	convrt_(&imod, &in2, &in1, &dim2, &dim1);
	/* now in1 is gdalt, in2 is gdlat, in3 is glon */
    }
    
    result = traceMagneticField(year,
                       month,
		       day,
		       hour,
		       min,
		       sec,
                       in2,
		       in3,
		       in1,
		       model,
		       qualifier,
		       stopAlt,
		       &out2,
		       &out3,
		       &out1);
		       
    /* out1 gdalt or ZGSM, out2 gdlat or XGSM, out3 glon or YGSM */
		       
    /* convert to GSM if result okay and outputType == 1 and qualifier != 4 (since then already GSM output) */
    if (outputType == 1 && result == 0 && qualifier != 4)
    {
        imod = 1;
        /* get geocentric coordinates from geodetic */
        convrt_(&imod, &out2, &out1, &dim2, &dim1);
	dim3 = out3;
	
	/* dim2 now gclat, dim1 = rkm, dim3 = glon */
      
        /* convert from geocentric to spherical */
        out2 = (90.0 - dim2)*0.01745329;
        out3 = dim3*0.01745329;
        out1 = dim1/6371.2;
	
	/* now out1 = R, out2 = theta, out3 = phi */
    
        /* get cartesian coordinates from spherical */
        TS_SPHCAR_F77(&out1, &out2, &out3, &dim1, &dim2, &dim3, &imod);
	
	/* now dim1-XGEO, dim2=YGEO, dim3=ZGEO */
    
        /* convert cartesian into solar magnetospheric ones */
        geogsm_(&dim1, &dim2, &dim3, &out2, &out3, &out1, &imod);
	
	/* now out1 = ZGSM, out2=XGSM, out3 = YGSM */
    }
    
    /* convert to Geodetic if result okay and outputType == 0 and qualifier == 4 (since then GSM output) */
    if (outputType == 0 && result == 0 && qualifier == 4)
    {
        imod = -1; /* converst gsm to cartesian */
	geogsm_(&dim1, &dim2, &dim3, &out2, &out3, &out1, &imod);
	/* now dim1 XGEO, dim2 YGEO, dim3 ZGEO */
	/* convert cartesian to spherical */
	TS_SPHCAR_F77(&out1, &out2, &out3, &dim1, &dim2, &dim3, &imod);
	/* now out1 R, out2 theta, out3 phi */
	/* convert spherical to geocentric */
        dim1 = out1*6371.2; 
	dim2 = 90.0 - (out2/0.01745329);
	dim3 = out3/0.01745329;
	imod = 2; /* convert geocentric coordinates to geodetic */
	out3 = dim3;
    	convrt_(&imod, &out2, &out1, &dim2, &dim1);
	/* now out1 is gdalt, out2 is gdlat, out3 is glon */
    
    }
    
    retList = PyList_New(0);

    PyList_Append(retList, PyFloat_FromDouble(out1));
    PyList_Append(retList, PyFloat_FromDouble(out2));
    PyList_Append(retList, PyFloat_FromDouble(out3));
    
    return(retList);
}


    
     
/******** Internal helper functions not exported to Python ***********/



/***********************************************************************
*
* addIntListToPythonList     appends int array to python list up to first 0
*
*   arguments:
*       int array with terminating zero value
*       pointer to Python list on integers to be appended to
*
*   returns:
*       void
*
*   throws:
*       None
*
*/
static void addIntListToPythonList(int * intArray, PyObject * pyList)
{
    int i;

    for (i = 0; intArray[i] != 0; i++)
        PyList_Append(pyList, PyInt_FromLong(intArray[i]));

}


/***********************************************************************
*
* setPyTime     sets Python list to time specified by key
*
*   arguments:
*       pointer to Python list to be set to new time (Python list has
*         format of 6 Py integers: year, month, day, hour, min, sec)
*       double sec - seconds since 1/1/1950
*
*   returns:
*       void
*
*   throws:
*       None
*
*/
static void setPyTime(PyObject * retDateTime, double sec)
{
    int iyr= 0, imd= 0, ihm= 0, ics= 0;
    int month = 0, day = 0, hour = 0, min = 0;

    dinvmadptr(sec, &iyr, &imd, &ihm, &ics);

    month = imd/100;
    day = imd - month*100;

    hour = ihm/100;
    min = ihm - hour*100;
    
    PyList_Append(retDateTime, PyInt_FromLong(iyr));
    PyList_Append(retDateTime, PyInt_FromLong(month));
    PyList_Append(retDateTime, PyInt_FromLong(day));
    PyList_Append(retDateTime, PyInt_FromLong(hour));
    PyList_Append(retDateTime, PyInt_FromLong(min));
    PyList_Append(retDateTime, PyInt_FromLong(ics/100));
}

/***********************************************************************
*
* getIntFromPy     returns a int given a Python int, long, or float
*
*   arguments:
*       PyObject * pyNum
*
*   returns:
*       int.  If not a Python int, long, or float, returns 0
*/
static double getIntFromPy(PyObject * pyNum)
{
    if (PyInt_Check(pyNum))
        return((int)PyInt_AsLong(pyNum));
    if (PyLong_Check(pyNum))
        return((int)PyLong_AsLong(pyNum));
    if (PyFloat_Check(pyNum))
        return((int)PyFloat_AsDouble(pyNum));
    /* wrong type of object passed in */
    return(0);
}

/***********************************************************************
*
* getDoubleFromPy     returns a double given a Python int, long, or float
*
*   arguments:
*       PyObject * pyNum
*
*   returns:
*       double.  If not a Python int, long, or float, returns missing
*/
static double getDoubleFromPy(PyObject * pyNum)
{
    if (PyInt_Check(pyNum))
        return((double)PyInt_AsLong(pyNum));
    if (PyLong_Check(pyNum))
        return((double)PyLong_AsLong(pyNum));
    if (PyFloat_Check(pyNum))
        return(PyFloat_AsDouble(pyNum));
    /* wrong type of object passed in */
    return(missing);
}


/***********************************************************************
*
* getMnemFromPy     returns a char * mnemonic given a python int or
*                   a python string containing an int or a mnemonic
*
*   arguments:
*       PyObject * pyArg
*
*   returns:
*       char * mnem - must be freed by user after use.  If error,
*       returns NULL
*/
static char * getMnemFromPy(PyObject * pyArg)
{
    char * thisMnem = NULL;
    char * temp = NULL;
    
    /* check if its an integer */
    if (PyInt_Check(pyArg))
	thisMnem = cedarGetParMnemonic((int)PyInt_AsLong(pyArg));

    /* check if its a long */
    else if (PyLong_Check(pyArg))
	thisMnem = cedarGetParMnemonic((int)PyLong_AsLong(pyArg));

    /* check if its c string */
    else if (PyString_Check(pyArg))
    {
	temp = PyString_AsString(pyArg);
	thisMnem = (char *)malloc(sizeof(char)*(strlen(temp)+1));
	strcpy(thisMnem, temp);
	if (cedarGetParCodeFromMnemonic(thisMnem) == missingData)
	{
	    free(thisMnem);
	    return(NULL);
	}
    }
    else
        return(NULL);
	
    return(thisMnem);
}



/***********************************************************************
*
* str_caseins_cmp     case insensitive C string compare
*
*   arguments:
*       char * str1 - pointer to first c string
*       char * str2 - pointer to second c string
*
*   returns:
*       0 if string match after all characters converted to lower case,
*       1 if not
*
*/
static int str_caseins_cmp(char * str1, char * str2)
{
    int i = 0;
    
    if (strlen(str1) != strlen(str2))
        return(1);
	
    for (i=0; i<strlen(str1); i++)
    {
        if (tolower(str1[i]) != tolower(str2[i]))
	    return(1);
    }
    return(0);
}


/********** Initialization code for module ******************************/

static PyMethodDef _MadrecMethods[] = 
{
	  {"getSummary",           	   _Madrec_getSummary,          	METH_VARARGS},
	  {"getDerivableParms",            _Madrec_getDerivableParms,           METH_VARARGS},
	  {"getIsprintReport",             _Madrec_getIsprintReport,            METH_VARARGS},
          {"getFileType",                  _Madrec_getFileType,                 METH_VARARGS},
	  {"looker",                       _Madrec_looker,                      METH_VARARGS},
	  {"looker_nonFile",               _Madrec_looker_nonFile,              METH_VARARGS},
          {"getParmsAtTime",               _Madrec_getParmsAtTime,              METH_VARARGS},
	  {"radarToGeodetic",              _Madrec_radarToGeodetic,             METH_VARARGS},
	  {"geodeticToRadar",              _Madrec_geodeticToRadar,             METH_VARARGS},
	  {"createMaddata",                _Madrec_createMaddata,          	METH_VARARGS},
	  {"destroyMaddata",               _Madrec_destroyMaddata,              METH_VARARGS},
	  {"getMadrecord",                 _Madrec_getMadrecord,          	METH_VARARGS},
          {"madrecOpen",                   _Madrec_madrecOpen,                  METH_VARARGS},
          {"madrecClose",                  _Madrec_madrecClose,                 METH_VARARGS},
          {"madrecGetNextRec",             _Madrec_madrecGetNextRec,            METH_VARARGS},
          {"madrecDumpDataRecord",         _Madrec_madrecDumpDataRecord,        METH_VARARGS},
          {"madrecDumpCatalogRecord",      _Madrec_madrecDumpCatalogRecord,     METH_VARARGS},
          {"madrecDumpHeaderRecord",       _Madrec_madrecDumpHeaderRecord,      METH_VARARGS},
          {"madrecCreateDataRecord",       _Madrec_madrecCreateDataRecord,      METH_VARARGS},
          {"cedarSet1dParm",               _Madrec_cedarSet1dParm,              METH_VARARGS},
          {"cedarSet2dParm",               _Madrec_cedarSet2dParm,              METH_VARARGS},
          {"madrecPutNextRec",             _Madrec_madrecPutNextRec,            METH_VARARGS},
          {"madrecCreateCatalogRecord",    _Madrec_madrecCreateCatalogRecord,   METH_VARARGS},
          {"madrecCreateHeaderRecord",     _Madrec_madrecCreateHeaderRecord,    METH_VARARGS},
          {"cedarGetParCodeType",     	   _Madrec_cedarGetParCodeType,    	METH_VARARGS},
          {"madGetParMnemType",     	   _Madrec_madGetParMnemType,    	METH_VARARGS},
	  {"madGetParType",     	   _Madrec_madGetParType,    	        METH_VARARGS},
	  {"madGetCategoryIndex",          _Madrec_madGetCategoryIndex,    	METH_VARARGS},
	  {"cedarGetParMnemonic",     	   _Madrec_cedarGetParMnemonic,    	METH_VARARGS},
	  {"cedarGetParDescription",  	   _Madrec_cedarGetParDescription, 	METH_VARARGS},
          {"cedarGetParScaleFactor",  	   _Madrec_cedarGetParScaleFactor, 	METH_VARARGS},
	  {"madGetParDescription",  	   _Madrec_madGetParDescription, 	METH_VARARGS},
	  {"madGetSimpleParDescription",   _Madrec_madGetSimpleParDescription, 	METH_VARARGS},
	  {"madGetParUnits",  	   	   _Madrec_madGetParUnits, 		METH_VARARGS},
          {"madGetParFormat",  	   	   _Madrec_madGetParFormat, 		METH_VARARGS},
          {"madGetParWidth",  	   	   _Madrec_madGetParWidth, 		METH_VARARGS},
	  {"cedarGetParCodeFromMnemonic",  _Madrec_cedarGetParCodeFromMnemonic, METH_VARARGS},
	  {"cedarGetInformation",          _Madrec_cedarGetInformation,         METH_VARARGS},
          {"cedarCatalogHeaderList",       _Madrec_cedarCatalogHeaderList,      METH_VARARGS},
	  {"madHasHtmlDesc",          	   _Madrec_madHasHtmlDesc,         	METH_VARARGS},
	  {"getUtFromDate",          	   _Madrec_getUtFromDate,         	METH_VARARGS},
	  {"getDateFromUt",                _Madrec_getDateFromUt,         	METH_VARARGS},
	  {"pyTraceMagneticField",         _Madrec_pyTraceMagneticField,        METH_VARARGS},
	  {"convertGeodeticGeomagnetic",   _Madrec_convertGeodeticGeomagnetic,  METH_VARARGS},
          {NULL,      NULL}        /* Sentinel */
};

   
void init_Madrec()
{
	  
    PyImport_AddModule("_Madrec");
    Py_InitModule("_Madrec", _MadrecMethods);
       
    if (cedarReadParCodes())  // check for failure
    {
        PyErr_SetString(PyExc_IOError, "Failed to read parcods.tab file");
    }
}
