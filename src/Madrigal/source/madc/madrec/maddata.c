/*
  See maddata.h for overview of this module.
  
  Written 11/2002 by B. Rideout
*/

#define _REENTRANT
#include <maddata.h>
#include <madDeriveEngine.h>



/***********************************************************************
*
* createMadparmList   initializes a new MadparmList
*
*   arguments: None
*
*   returns - pointer to newly created MadparmList.  Use 
*             destroyMadparmList when done
*/
MadparmList * createMadparmList()
{
    MadparmList * madParmList;

    if ((madParmList = (MadparmList *)malloc(sizeof(MadparmList)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    madParmList->numParm = 0;
    madParmList->typeParmList = NULL;
    madParmList->errParmList = NULL;
    madParmList->mnemList = NULL;
    madParmList->minList = NULL;
    madParmList->maxList = NULL;

    return(madParmList);
}


/***********************************************************************
*
* copyMadparmList   copies an existing MadparmList
*
*   arguments: Pointer to existing MadparmList
*
*   returns - pointer to newly copied MadparmList, newly allocated
*             on the heap.  Use destroyMadparmList when done
*             Returns NULL if NULL passed in
*/
MadparmList * copyMadparmList(MadparmList * madparmList)
{
    MadparmList * newMadparmList = NULL;
    int i = 0;

    if (madparmList == NULL)
        return NULL;

    /* malloc new MadparmList */
    if ((newMadparmList = (MadparmList *)malloc(sizeof(MadparmList)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    newMadparmList->numParm = madparmList->numParm;

    /* malloc and copy new MadparmList->typeParmList */
    if ((newMadparmList->typeParmList = (Parm_type *)malloc(sizeof(Parm_type) * madparmList->numParm))==0)
    {
        perror("malloc");
        exit(-1);
    }
    for (i=0; i<madparmList->numParm; i++)
        newMadparmList->typeParmList[i] = madparmList->typeParmList[i];
        
    /* malloc and copy new MadparmList->errParmList */
    if ((newMadparmList->errParmList = (Error_type *)malloc(sizeof(Error_type) * madparmList->numParm))==0)
    {
        perror("malloc");
        exit(-1);
    }
    for (i=0; i<madparmList->numParm; i++)
        newMadparmList->errParmList[i] = madparmList->errParmList[i];

    /* malloc new MadparmList->mnemList */
    if ((newMadparmList->mnemList = (char **)malloc(sizeof(char *) * madparmList->numParm))==0)
    {
        perror("malloc");
        exit(-1);
    }

    /* malloc and copy mnemonics */
    for (i=0; i<madparmList->numParm; i++)
    {
        if ((newMadparmList->mnemList[i] = (char *)malloc(sizeof(char) * MNEM_LEN))==0)
        {
            perror("malloc");
            exit(-1);
        }
        strcpy(newMadparmList->mnemList[i], madparmList->mnemList[i]);
    }  

    /* malloc and copy new MadparmList->minList */
    if ((newMadparmList->minList = (double *)malloc(sizeof(double) * madparmList->numParm))==0)
    {
        perror("malloc");
        exit(-1);
    }
    for (i=0; i<madparmList->numParm; i++)
        newMadparmList->minList[i] = madparmList->minList[i];

    /* malloc and copy new MadparmList->maxList */
    if ((newMadparmList->maxList = (double *)malloc(sizeof(double) * madparmList->numParm))==0)
    {
        perror("malloc");
        exit(-1);
    }
    for (i=0; i<madparmList->numParm; i++)
        newMadparmList->maxList[i] = madparmList->maxList[i];
    
    return(newMadparmList);
}


/***********************************************************************
*
* destroyMadparmList   releases an existing MadparmList
*
*   arguments: pointer to existing MadparmList
*
*   returns - void
*/
void destroyMadparmList(MadparmList * madParmList)
{
    int i = 0;
    
    if (madParmList == NULL)
        return;

    if (madParmList->mnemList != NULL)
    {
        for (i=0; i<madParmList->numParm; i++)
            free(madParmList->mnemList[i]);
        free(madParmList->mnemList);
    }
    
    if (madParmList->typeParmList != NULL)
        free(madParmList->typeParmList);
        
    if (madParmList->errParmList != NULL)
        free(madParmList->errParmList);
    
    if (madParmList->minList != NULL)
        free(madParmList->minList);
    
    if (madParmList->maxList != NULL)
        free(madParmList->maxList);
    
    free(madParmList);
}


/***********************************************************************
*
* appendMadparm   adds a new parameter to the MadparmList
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              const char * mnem - string containing name
*
*      mnem is copied into newly allocated memory, so user is free to
*      release mnem after this method.  mnem converted to standard form.
*
*   returns - 0 if success, -1 if failure (if mnem too long or unknown)
*/
int appendMadparm(MadparmList * madparmList, const char * mnem)
{
    /* check that madparmList exists */
    if (madparmList == NULL)
        return (-1);

    /* check that mnem is not too long */
    if (strlen(mnem) >= MNEM_LEN)
        return (-1);
        
    /* check that mnemonic exists */
    if (isMadparmError(mnem) == -1)
        return (-1);

    madparmList->numParm++;

    /* append type of parameter - undetermined at the moment */
    madparmList->typeParmList = (Parm_type *)realloc(madparmList->typeParmList, madparmList->numParm * sizeof(Parm_type *));
    if (madparmList->typeParmList == 0)
    {
        perror("realloc");
        exit(-1);
    }
    madparmList->typeParmList[madparmList->numParm - 1] = UNDETERMINED_PARM;
    
    /* append whether parameter is error mnem */
    madparmList->errParmList = (Error_type *)realloc(madparmList->errParmList, madparmList->numParm * sizeof(Error_type *));
    if (madparmList->errParmList == 0)
    {
        perror("realloc");
        exit(-1);
    }
    if (isMadparmError(mnem) == 1)
        madparmList->errParmList[madparmList->numParm - 1] = ERROR_MNEM;
    else
        madparmList->errParmList[madparmList->numParm - 1] = STANDARD_MNEM;

    /* append mnemonic */
    madparmList->mnemList = (char **)realloc(madparmList->mnemList, madparmList->numParm * sizeof(char **));
    if (madparmList->mnemList == 0)
    {
        perror("realloc");
        exit(-1);
    }
    madparmList->mnemList[madparmList->numParm - 1] = malloc(sizeof(char) * MNEM_LEN);
    if (madparmList->mnemList[madparmList->numParm - 1] == 0)
    {
        perror("malloc");
        exit(-1);
    }
    /* copy mnem in std form to newly allocated memory */
    getStdMnem(mnem, madparmList->mnemList[madparmList->numParm - 1]);


    /* append minimum value of parameter - missing at the moment */
    madparmList->minList = (double *)realloc(madparmList->minList, madparmList->numParm * sizeof(double));
    if (madparmList->minList == 0)
    {
        perror("realloc");
        exit(-1);
    }
    madparmList->minList[madparmList->numParm - 1] = missing;

    /* append maximum value of parameter - missing at the moment */
    madparmList->maxList = (double *)realloc(madparmList->maxList, madparmList->numParm * sizeof(double));
    if (madparmList->maxList == 0)
    {
        perror("realloc");
        exit(-1);
    }
    madparmList->maxList[madparmList->numParm - 1] = missing;

    return (0);  
}


/***********************************************************************
*
*  hasParm   returns 1 if madparmList has parameter, 0 otherwise
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              const char * mnem - string containing name
*
*      comparision is done after coverting mnem to standard form
*
*   returns - 0 if success, -1 if failure (if mnem too long)
*/
int hasParm(MadparmList * madparmList, const char * mnem)
{
    int i=0;
    char stdMnem[MNEM_LEN] = "";

    getStdMnem(mnem, stdMnem);

    if (madparmList == NULL)
        return 0;

    for (i=0; i<madparmList->numParm; i++)
    {
        if (strcmp(stdMnem, madparmList->mnemList[i]) == 0)
            return 1;
    }

    return 0;
}


/***********************************************************************
*
*  isErrorParm   returns 1 parameter at index is error, 0 if standard
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              int index - index into madparmList
*
*   returns - 1 parameter at index is error, 0 if standard, -1 if
*             index out of bounds
*/
int isErrorParm(MadparmList * madparmList, int index)
{
    if (index >= madparmList->numParm)
        return (-1);
    
    if (madparmList->errParmList[index] == ERROR_MNEM)
        return (1);
    else
        return (0);
}


/***********************************************************************
*
*  getIndex   returns index of parameter if found, -1 otherwise
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              const char * mnem - string containing name
*
*      comparision is done after coverting mnem to standard form
*
*   returns - index of parameter if found, -1 otherwise
*/
int getIndex(MadparmList * madparmList, const char * mnem)
{
    int i=0;
    char stdMnem[MNEM_LEN] = "";

    getStdMnem(mnem, stdMnem);

    if (madparmList == NULL)
        return -1;

    for (i=0; i<madparmList->numParm; i++)
    {
        if (strcmp(stdMnem, madparmList->mnemList[i]) == 0)
            return i;
    }

    return -1;
}


/***********************************************************************
*
*  getMinParm   returns minimum value of mnem, or missing if unknown
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              char * mnem - string containing name of parameter
*
*   returns - minimum value of mnem, or missing if unknown or not in list
*/
double getMinParm(MadparmList * madparmList, char * mnem)
{
    int i=0;
    char stdMnem[MNEM_LEN] = "";

    getStdMnem(mnem, stdMnem);

    if (madparmList == NULL)
        return missing;

    for (i=0; i<madparmList->numParm; i++)
    {
        if (strcmp(stdMnem, madparmList->mnemList[i]) == 0)
            return madparmList->minList[i];
    }

    /* mnem not in list */
    return missing;
}


/***********************************************************************
*
*  getMaxParm   returns maximum value of mnem, or missing if unknown
*
*   arguments: MadparmList * madparmList - pointer to existing MadparmList
*              char * mnem - string containing name of parameter
*
*   returns - maximum value of mnem, or missing if unknown or not in list
*/
double getMaxParm(MadparmList * madparmList, char * mnem)
{
    int i=0;
    char stdMnem[MNEM_LEN] = "";

    getStdMnem(mnem, stdMnem);

    if (madparmList == NULL)
        return missing;

    for (i=0; i<madparmList->numParm; i++)
    {
        if (strcmp(stdMnem, madparmList->mnemList[i]) == 0)
            return madparmList->maxList[i];
    }

    /* mnem not in list */
    return missing;
}


/***********************************************************************
*
* analyzeFileParms   get 4 lists of parameters from file:
*                     1) all 1D measured parameters
*                     1) all 2D measured parameters
*                     1) all 1D derivable parameters
*                     1) all 2D derivable parameters
*
*   arguments: char * filename - full path to file
*              MadparmList * list1DMeasParms - pointer to MadparmList to be
*                  populated with all 1D measured parameters found in file 
*              MadparmList * list2DMeasParms - pointer to MadparmList to be
*                  populated with all 2D measured parameters found in file 
*              MadparmList * list1DDervParms - pointer to MadparmList to be
*                  populated with all 1D parameters that could be derived
*              MadparmList * list2DDervParms - pointer to MadparmList to be
*                  populated with all 2D parameters that could be derived
*              FILE * errFile   - errFile to write an error messages to
*
*   returns - 0 if success, -1 otherwise
*
*   affects - populates the four input lists.  All four pointers should point
*   to NULL when passed in.  When done with these four lists, user should call
*   destroyMadparmList for each to free memory.
*
*   Note: Since a file may contain more than one type of record, these lists contain
*   parameters from any record that fits into each list.  For example, a certain 1D 
*   parameter is measured in one type of record, but can be derived from another type
*   where its not measured, that parameter would appear in both list1DMeasParms and
*   list1DDervParms.  
*
*   See also method getDerivableParms, which accepts two lists of measured 1D and 
*   measured 2D parameters, and returns two lists of derivable 1D and 
*   derivable 2D parameters.  Since this other method does not analyze a file, it does not
*   have the ambiguities of analyzeFileParms discussed above.
*/
int analyzeFileParms(char * filename, 
                     MadparmList ** list1DMeasParms,
                     MadparmList ** list2DMeasParms,
                     MadparmList ** list1DDervParms,
                     MadparmList ** list2DDervParms,
                     FILE * errFile)
{    
    Madrec * madrecp = NULL;
    InfoDervFile * infoDervFile = NULL;
    InfoDerived * infoDerv = NULL;
    int i=0;
    int j=0;
    MadparmList * requestParmList = NULL;
    MadfilterList * madfilterList = NULL;
    
    /* set default errFile */
    if (errFile == NULL)
    	errFile = stderr;
    
    /* Create a madrec object */
    madrecp = madrecCreate();
    if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
    {
        fwrite(madrecGetError(madrecp),
               sizeof(char),
               strlen(madrecGetError(madrecp)) + 1,
               errFile);
        return (-1);
     }

    /* Read the parameter code table */
    cedarReadParCodes();

    /* Connect the madrec object to a madrigal file and load rapidly into memory */
    madrecOpen(madrecp, 50, filename);
    if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
    {
        fwrite(madrecGetError(madrecp),
               sizeof(char),
               strlen(madrecGetError(madrecp)) + 1,
               errFile);
        madrecDestroy(madrecp);
        return (-1);
    }
     
    /* create an empty requestParmList, since request irrelevant */
    requestParmList = createMadparmList();
    
    /* create empty filter list, since none wanted */
    madfilterList = createMadfilterList();

    /* calculate plan for how to derive data */
    infoDervFile = createInfoDervFile(madrecp, requestParmList, madfilterList, errFile);
    if (infoDervFile == NULL)
    {
        madrecDestroy(madrecp);
        return (-1);
    }
    
    /* create the four lists to return */
    *list1DMeasParms = createMadparmList();
    *list2DMeasParms = createMadparmList();
    *list1DDervParms = createMadparmList();
    *list2DDervParms = createMadparmList();
    
    /* populate lists from info in infoDervFile */
    for (i=0; i<infoDervFile->numTypesRec; i++)
    {
        infoDerv = infoDervFile->infoDervList[i];
        
        /* read measured 1D, but skip prolog parameters */
        for (j=NUM_PROLOG_PARM; j<infoDerv->meas1DParmList->numParm; j++)
        {
            /* append this parm if not already there */
            if (!hasParm(*list1DMeasParms, infoDerv->meas1DParmList->mnemList[j]))
  	              appendMadparm(*list1DMeasParms, infoDerv->meas1DParmList->mnemList[j]);
        }
        
        /* read measured 2D, but skip prolog parameters  */
        for (j=NUM_2D_PROLOG_PARM; j<infoDerv->meas2DParmList->numParm; j++)
        {
            /* append this parm if not already there */
            if (!hasParm(*list2DMeasParms, infoDerv->meas2DParmList->mnemList[j]))
                appendMadparm(*list2DMeasParms, infoDerv->meas2DParmList->mnemList[j]);
        }
        
        /* get all derivable 1D parameters that weren't measured, and skip prolog parameters */
        for (j=NUM_PROLOG_PARM; j<infoDerv->allAvail1DParmList->numParm; j++)
        {
            /* append this parameter is not already there, not measured */
            if (!hasParm(*list1DMeasParms, infoDerv->allAvail1DParmList->mnemList[j]))
                if (!hasParm(*list1DDervParms, infoDerv->allAvail1DParmList->mnemList[j]))
                    appendMadparm(*list1DDervParms, infoDerv->allAvail1DParmList->mnemList[j]);
        }
        
        /* get all derivable 2D parameters that weren't measured, and skip prolog parameters  */
        for (j=NUM_2D_PROLOG_PARM; j<infoDerv->allAvail2DParmList->numParm; j++)
        {
            /* append this parameter is not already there, not measured */
            if (!hasParm(*list2DMeasParms, infoDerv->allAvail2DParmList->mnemList[j]))
                if (!hasParm(*list2DDervParms, infoDerv->allAvail2DParmList->mnemList[j]))
                    appendMadparm(*list2DDervParms, infoDerv->allAvail2DParmList->mnemList[j]);
        }
    
    } /* next record type */
    
    destroyInfoDervFile(infoDervFile);
    destroyMadparmList(requestParmList);
    destroyMadfilterList(madfilterList);
    madrecDestroy(madrecp);
    
    return (0);
}


/***********************************************************************
*
* getDerivedParms   gets a list of derivable parameters given a list of
*                   measured parameters
*
*   arguments: MadparmList * listMeasParms - pointer to MadparmList 
*                  containing measured parameters
*
*   returns - MadparmList * listDervParms - pointer to MadparmList 
*                  containing all parameters that could be derived.
*                  User is responsible for calling destroyMadparmList
*                  when done with this list
*
*/
MadparmList * getDerivedParms(MadparmList * listMeasParms)
{    
    InfoDerived * infoDerv = NULL;
    int i=0;
    MadparmList * requestParmList = NULL;
    MadparmList * meas1DParmList = NULL;
    MadparmList * meas2DParmList = NULL;
    MadparmList * listDervParms = NULL;
    
    MadfilterList * madfilterList = NULL;
     
    /* create an empty requestParmList, since irrelevant */
    requestParmList = createMadparmList();
    
    /* createInfoDerived assumes first NUM_PROLOG_PARM in meas1DParmsList, so prepend prolog parameters */
    meas1DParmList = createMadparmList();
    appendMadparm(meas1DParmList, "FIRST_IBYR");
    appendMadparm(meas1DParmList, "FIRST_IBDT");
    appendMadparm(meas1DParmList, "FIRST_IBHM");
    appendMadparm(meas1DParmList, "FIRST_IBCS");
    appendMadparm(meas1DParmList, "KINST");
    appendMadparm(meas1DParmList, "KINDAT");
    appendMadparm(meas1DParmList, "IBYR");
    appendMadparm(meas1DParmList, "IBDT");
    appendMadparm(meas1DParmList, "IBHM");
    appendMadparm(meas1DParmList, "IBCS");
    appendMadparm(meas1DParmList, "IEYR");
    appendMadparm(meas1DParmList, "IEDT");
    appendMadparm(meas1DParmList, "IEHM");
    appendMadparm(meas1DParmList, "IECS");
    appendMadparm(meas1DParmList, "NROW");
    appendMadparm(meas1DParmList, "UT1");
    appendMadparm(meas1DParmList, "UT2");
    appendMadparm(meas1DParmList, "RECNO");
    
    /* createInfoDerived assumes first NUM_2D_PROLOG_PARM in meas2DParmsList, so prepend 2D prolog parameters */
    meas2DParmList = createMadparmList();
    appendMadparm(meas2DParmList, "ROW");
    
    
    for (i=0; i<listMeasParms->numParm; i++)
        appendMadparm(meas1DParmList, listMeasParms->mnemList[i]);
    
    /* create empty filter list, since none wanted */
    madfilterList = createMadfilterList();
    
    /* let madDeriveEngine analyze these parameters */
    infoDerv = createInfoDerived(meas1DParmList, 
                                 meas2DParmList,
                                 requestParmList,
                                 madfilterList);

    /* create the list to return */
    listDervParms = createMadparmList();
    
    /* populate list from info in infoDerv->allAvail1DParmList */
    for (i=0; i<infoDerv->allAvail1DParmList->numParm; i++)
    {
        /* append parm if not in measured list */
        if (!hasParm(listMeasParms, infoDerv->allAvail1DParmList->mnemList[i]))
            appendMadparm(listDervParms, infoDerv->allAvail1DParmList->mnemList[i]);
    
    } /* next parm */
    
    /* next populate list from info in infoDerv->allAvail2DParmList */
    for (i=0; i<infoDerv->allAvail2DParmList->numParm; i++)
    {
        /* append parm if not in measured list */
        if (!hasParm(listMeasParms, infoDerv->allAvail2DParmList->mnemList[i]))
            appendMadparm(listDervParms, infoDerv->allAvail2DParmList->mnemList[i]);
    
    } /* next parm */
    
    /* release temporary objects */
    destroyInfoDerived(infoDerv);
    destroyMadparmList(requestParmList);
    destroyMadparmList(meas1DParmList);
    destroyMadparmList(meas2DParmList);
    destroyMadfilterList(madfilterList);
    
    return (listDervParms);
}


/***********************************************************************
*
* createMadfilterList   initializes a new MadfilterList
*
*   arguments: None
*
*   returns - pointer to newly created MadfilterList.  Use 
*             destroyMadfilterList when done
*/
MadfilterList * createMadfilterList()
{
    MadfilterList * madFiltList;

    if ((madFiltList = (MadfilterList *)malloc(sizeof(MadfilterList)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    madFiltList->numFilters = 0;    
    madFiltList->madfilt_list = NULL; 

    return(madFiltList);
}


/***********************************************************************
*
* destroyMadfilterList   releases an existing MadfilterList
*
*   arguments: pointer to existing MadfilterList
*
*   returns - void
*/
void destroyMadfilterList(MadfilterList * madFiltList)
{
    int i = 0;
    
    if (madFiltList == NULL)
        return;

    if (madFiltList->madfilt_list != NULL)
    {
        for (i=0; i<madFiltList->numFilters; i++)
        {
            free(madFiltList->madfilt_list[i].lower);
            free(madFiltList->madfilt_list[i].upper);
            free(madFiltList->madfilt_list[i].madParm1);
            free(madFiltList->madfilt_list[i].madParm2);
        }
        free(madFiltList->madfilt_list);
    }
    
    free(madFiltList);
}

/***********************************************************************
*
* appendMadfilter   adds a new Madfilter to the MadfilterList
*
*   arguments: MadfilterList * madfilt_list - pointer to existing MadfilterList
*              Filter_type filtType - enum used to identify filter types
*              int numRange - number of ranges included - must be greater than 0
*              double * lower - array of lower limits of range - if "missing", no 
*                               lower limit for that range
*              double * upper - array of upper limits of range - if "missing", no 
*                               upper limit for that range
*              char * - madParm1 - Mnemonic of first parameter - cannot be 0 length
*              char * - madParm2 - Mnemonic of second parameter - can be 0 length if SINGLE_FILT
*
*      lower, upper, madParm1 and madParm2 are copied into newly allocated memory, so user is free to
*      release them after this method.  madParm1 and madParm2 converted to standard mnemonic form.
*
*   returns - 0 if success, -1 if failure (if either mnemonic too long, if madfilt_list NULL,
*             or numRange < 1)
*/
int appendMadfilter(MadfilterList * madFiltList,
                    Filter_type filtType, 
                    int numRange,
                    double * lower, 
                    double * upper, 
                    char * madParm1,
                    char * madParm2)
{
    int i = 0;

    /* check that all data exists */
    if (madFiltList == NULL || lower == NULL || upper == NULL ||
        madParm1 == NULL || madParm2 == NULL)
        return (-1);

    /* check that mnem is not too long */
    if (strlen(madParm1) >= MNEM_LEN || strlen(madParm2) >= MNEM_LEN)
        return (-1);
        
    /* check that numRange > 0 */
    if (numRange < 1)
        return (-1);

    madFiltList->numFilters++;

    /* realloc madFiltList->madfilt_list for one more filter */
    madFiltList->madfilt_list = (Madfilter *)realloc(madFiltList->madfilt_list, madFiltList->numFilters * sizeof(Madfilter));
    if (madFiltList->madfilt_list == NULL)
    {
        perror("realloc");
        exit(-1);
    }
    
    /* set values in newly-added filter */
    madFiltList->madfilt_list[madFiltList->numFilters - 1].filtType = filtType;
    madFiltList->madfilt_list[madFiltList->numFilters - 1].numRange = numRange;
    
    /* append lower */
    madFiltList->madfilt_list[madFiltList->numFilters - 1].lower = (double *)malloc(sizeof(double) * numRange);
    if (madFiltList->madfilt_list[madFiltList->numFilters - 1].lower == 0)
    {
        perror("realloc");
        exit(-1);
    }
    /* copy lower in to newly allocated memory */
    for (i=0; i<numRange; i++)
        madFiltList->madfilt_list[madFiltList->numFilters - 1].lower[i] = lower[i];
        
    /* append upper */
    madFiltList->madfilt_list[madFiltList->numFilters - 1].upper = (double *)malloc(sizeof(double) * numRange);
    if (madFiltList->madfilt_list[madFiltList->numFilters - 1].upper == 0)
    {
        perror("realloc");
        exit(-1);
    }
    /* copy upper in to newly allocated memory */
    for (i=0; i<numRange; i++)
        madFiltList->madfilt_list[madFiltList->numFilters - 1].upper[i] = upper[i];

    /* append madParm1 */
    madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm1 = (char *)malloc(sizeof(char) * MNEM_LEN);
    if (madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm1 == 0)
    {
        perror("realloc");
        exit(-1);
    }
    /* copy madParm1 in std form to newly allocated memory */
    getStdMnem(madParm1, madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm1);

    /* append madParm2 */
    madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm2 = (char *)malloc(sizeof(char) * MNEM_LEN);
    if (madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm2 == 0)
    {
        perror("realloc");
        exit(-1);
    }
    /* copy madParm2 in std form to newly allocated memory */
    getStdMnem(madParm2, madFiltList->madfilt_list[madFiltList->numFilters - 1].madParm2);

    return(0);
}


/***********************************************************************
*
* copyMadfilterList   copies an existing MadfilterList
*
*   arguments: Pointer to existing MadfilterList
*
*   returns - pointer to newly copied MadfilterList, newly allocated
*             on the heap.  Use destroyMadfilterList when done
*             Returns NULL if NULL passed in
*/
MadfilterList * copyMadfilterList(MadfilterList * madfilterList)
{
    MadfilterList * newMadfilterList = NULL;
    int i = 0;

    if (madfilterList == NULL)
        return NULL;

    /* malloc new MadfilterList */
    if ((newMadfilterList = (MadfilterList *)malloc(sizeof(MadfilterList)))==0)
    {
        perror("malloc");
        exit(-1);
    }

    newMadfilterList->numFilters = madfilterList->numFilters;

    /* malloc and copy new MadfilterList->madfilt_list */
    if ((newMadfilterList->madfilt_list = (Madfilter *)malloc(sizeof(Madfilter) * madfilterList->numFilters))==0)
    {
        perror("malloc");
        exit(-1);
    }
    for (i=0; i<madfilterList->numFilters; i++)
    {
        newMadfilterList->madfilt_list[i].filtType = madfilterList->madfilt_list[i].filtType;
        newMadfilterList->madfilt_list[i].numRange = madfilterList->madfilt_list[i].numRange;
        
        /* lower */
        if ((newMadfilterList->madfilt_list[i].lower = (double *)malloc(sizeof(double) * \
                                                        madfilterList->madfilt_list[i].numRange))==0)
        {
            perror("malloc");
            exit(-1);
        }
        memcpy(newMadfilterList->madfilt_list[i].lower, 
               madfilterList->madfilt_list[i].lower, sizeof(double)*madfilterList->madfilt_list[i].numRange);
               
        /* upper */
        if ((newMadfilterList->madfilt_list[i].upper = (double *)malloc(sizeof(double) * \
                                                        madfilterList->madfilt_list[i].numRange))==0)
        {
            perror("malloc");
            exit(-1);
        }
        memcpy(newMadfilterList->madfilt_list[i].upper, 
               madfilterList->madfilt_list[i].upper, sizeof(double)*madfilterList->madfilt_list[i].numRange);
        
        /* madParm1 */
        if ((newMadfilterList->madfilt_list[i].madParm1 = (char *)malloc(sizeof(char) * \
                                                           (strlen(madfilterList->madfilt_list[i].madParm1)+1)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        strcpy(newMadfilterList->madfilt_list[i].madParm1, 
               madfilterList->madfilt_list[i].madParm1);

        /* madParm2 */
        if ((newMadfilterList->madfilt_list[i].madParm2 = (char *)malloc(sizeof(char) * \
                                                           (strlen(madfilterList->madfilt_list[i].madParm2)+1)))==0)
        {
            perror("malloc");
            exit(-1);
        }
        strcpy(newMadfilterList->madfilt_list[i].madParm2, 
               madfilterList->madfilt_list[i].madParm2);
 
    }
        
    return(newMadfilterList);
}



/***********************************************************************
*
* getMadfilterListFromStr   creates a MadfilterList based on an isprint-like command string
*
*   arguments: str - an isprint-like command string
*
*      lower, upper, madParm1 and madParm2 are copied into newly allocated memory, so user is free to
*      release them after this method.  madParm1 and madParm2 converted to standard mnemonic form.
*
*      The filter string is the same string that is used in the new isprint
*      command line.  Filters are separated by spaces.  The allowed filters
*      are:  
*  
*         date1=mm/dd/yyyy  (starting date to be examined. If time1 not given, defaults to 0 UT.)
*            Example: date1=01/20/1998 
* 
*         time1=hh:mm:ss (starting UT time to be examined. If date1 given, is applied to date1.
*                         If not, applies on the first day of the experiment.)
*            Example: time1=13:30:00
* 
*         date2=mm/dd/yyyy (ending date to be examined.  If time2 not given, defaults to 0 UT.)
*            Example: date2=01/21/1998
*
*         time2=hh:mm:ss (ending UT time to be examined - If date2 not given, ignored.)
*            Example: time2=15:45:00
* 
*         In the follow arguments ranges are used.  If any range value is not given, it may be used to 
*         indicate no lower or upper limit (but the comma is always required). Ranges are inclusive
*         of the end points:
* 
*         z=lower alt limit1, upper alt limit1 [or lower alt limit2 , upper alt limit2 ...] (km)
*            Example 1: z=100,500  (This would limit the geodetic altitude to 100 to 500 km.)
*            Example 2: z=100,200or300,400  (This would limit the geodetic altitude to 100 to 200 km
*                                            or 300 to 400 km.)
*            Example 3: z=,200or300,400   (Since the lower limit of the first range is missing, this 
*                                          would limit the geodetic altitude to anything below 200 km 
*                                          or from 300 to 400 km.)
* 
*         az=lower az limit1, upper az limit1 [or lower az limit2 , upper az limit2 ...] (from -180 to 180 degrees)
*            Example 1: az=100,120  (This would limit the azimuth to 100 to 120 degrees.)
*            Example 2: z=-180,-90or90,180  (This would limit the azimuth to between -180 and -90 degrees or
*                                            to between 90 and 180 degrees.  Note this allows a filter to go
*                                            through 180 degrees.)
*  
*         el=lower el limit1, upper el limit1 [or lower el limit2 , upper el limit2 ...] (from 0 to 90) 
*            Example 1: z=0,45  (This would limit the elevation from 0 to 45 degrees.) 
* 
*         plen=lower pl limit1, upper pl limit1 [or lower pl limit2 , upper pl limit2 ...] (pulse len in sec)
*            Example 1: z=,5e-4  (This would limit the pulse length to 5e-4 seconds or less.)
*   
*   
*         Free form filters using any mnemonic, or two mnemonics added, subtracted, multiplied, or divided.
*         Any number of filters may be added: 
*  
*         filter=[mnemonic] or [mnemonic1,[+*-/]mnemonic2] , lower limit1 , upper limit1 [or lower limit2 , upper limit2 ...] 
*            Example 1: filter=ti,500,1000or2000,3000   (Limits the data to points where Ti is between 500 and 1000 degrees
*                                                      or between 2000 and 3000 degrees.  Note that the units are always
*                                                      those of the Cedar standard.)
*            Example 2: filter=gdalt,-,sdwht,0,    (This filter implies "gdalt - sdwht" must be greater than 0.0.  Since
*                                                   sdwht is shadow height - the distance above any point on the earth 
*                                                   where the sun is first visible - this filter implies that only data 
*                                                   in direct sunlight will be displayed.)
*            Example 3: filter=ti,/,Dti,100,   (Limits the data to points where the ratio Ti/dTi is more than 100.)
*   
*         So an full FLTSTR argument might be:
*      
*            "date1=01/20/1998 time1=13:30:00 z=,200or300,400 filter=gdalt,-,sdwht,0, filter=ti,/,Dti,100,"
*
*   returns - MadfilterList if success, NULL if failure
*/
MadfilterList * getMadfilterListFromStr(char * str)
{
    MadfilterList * madFiltList = NULL;
    
    double utLower = 0.0;  /* used to set up ut1 filter, if needed */
    double utUpper = 0.0;  
    double lower[100];     /* used for lower ranges - up to 100 */
    double upper[100];     /* used for upper ranges - up to 100 */
    char mnem1[MNEM_LEN] = "";
    char mnem2[MNEM_LEN] = "";
    Filter_type filtType = SINGLE_FILT;
    int numRanges = 0;
    int year = 0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int min = 0;
    int sec = 0;
    int i = 0;
    
    char * mainToken = NULL;      /* used to parse main str */
    char * restrict1 = NULL;      /* used with strtof_r, allows parsing without globals */
    char   subStr[BIG_BUF] = "";  /* used to parse individual tokens in main str */
    char * subToken  = NULL;      /* used to parse individual tokens in main str */
    char * restrict2 = NULL;      /* used with strtof_r, allows parsing without globals */
    char   subStr1[BIG_BUF] = ""; /* used to parse sub tokens in token */
    char * subToken1  = NULL;     /* used to parse sub tokens in token */
    char * restrict3 = NULL;      /* used with strtof_r, allows parsing without globals */
    
    madFiltList = createMadfilterList();
    
    /* parse str using spaces */
    mainToken = (char *)strtok_r(str, " ", &restrict1);
    while (mainToken)
    {     
        /* check each allowed argument */
        if (strncmp(mainToken, "date", 4) == 0)
        {
            /* add this date in ut to utLower or utUpper */
            strcpy(subStr, mainToken);
            /* parse using = and / */
            subToken = (char *)strtok_r(subStr, "=/", &restrict2);
            /* next token should be month */
            subToken = (char *)strtok_r('\0', "=/", &restrict2);
            month = atoi(subToken);
            /* check for error */
            if (month < 1 || month > 12)
            {
                fprintf(stderr, "Illegal month in date argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* next token should be day */
            subToken = (char *)strtok_r('\0', "=/", &restrict2);
            day = atoi(subToken);
            /* check for error */
            if (day < 1 || day > 31)
            {
                fprintf(stderr, "Illegal day in date argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* next token should be year */
            subToken = (char *)strtok_r('\0', "=/", &restrict2);
            year = atoi(subToken);
            /* check for error */
            if (year < 1900 || year > 10000)
            {
                fprintf(stderr, "Illegal year in date argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* now see if we add to utLower or utUpper */
            if (strncmp(mainToken, "date1", 5) == 0)
                utLower += getKey(year, month, day, 0, 0, 0);
            else if (strncmp(mainToken, "date2", 5) == 0)
                utUpper += getKey(year, month, day, 0, 0, 0);
            else
            {
                fprintf(stderr, "Illegal date argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
        }
            
        else if (strncmp(mainToken, "time", 4) == 0)
        {
            /* add this time in ut to utLower or utUpper */
            strcpy(subStr, mainToken);
            /* parse using = and : */
            subToken = (char *)strtok_r(subStr, "=:", &restrict2);
            /* next token should be hour */
            subToken = (char *)strtok_r('\0', "=:", &restrict2);
            hour = atoi(subToken);
            /* check for error */
            if (hour < 0 || hour > 24)
            {
                fprintf(stderr, "Illegal hour in time argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* next token should be min */
            subToken = (char *)strtok_r('\0', "=:", &restrict2);
            min = atoi(subToken);
            /* check for error */
            if (min < 0 || min > 60)
            {
                fprintf(stderr, "Illegal min in time argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* next token should be sec */
            subToken = (char *)strtok_r('\0', "=:", &restrict2);
            sec = atoi(subToken);
            /* check for error */
            if (sec < 0 || sec > 61)
            {
                fprintf(stderr, "Illegal sec in time argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
            /* now see if we add to utLower or utUpper */
            if (strncmp(mainToken, "time1", 5) == 0)
                utLower += hour*3600 + min*60 + sec;
            else if (strncmp(mainToken, "time2", 5) == 0)
                utUpper += hour*3600 + min*60 + sec;
            else
            {
                fprintf(stderr, "Illegal time argument\n");
                destroyMadfilterList(madFiltList);
                return NULL;
            }
        }
        
        else if (strncmp(mainToken, "z", 1) == 0)
        {
            /* gdalt filter */
            strcpy(subStr, mainToken);
            /* parse using '='  */
            subToken = (char *)strtok_r(subStr, "=", &restrict2);
            numRanges = 0;
            for (i=0; i<100; i++)
            {
                /* next token should be range (or NULL) */
                subToken = (char *)strtok_r('\0', "or", &restrict2);
                if (subToken == NULL)
                    break;
                else
                    numRanges++;
                /* if first char = , then lower limit = missing */
                if (subToken[0] == ',')
                    lower[i] = missing;
                else
                    lower[i] = atof(subToken);
                
                /* next parse this range into separate tokens using ',' */
                strcpy(subStr1, subToken);
                subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
                
                /* if first char is not ',', get next token */
                if (subStr1[0] != ',')
                    subToken1 = (char *)strtok_r('\0', ",", &restrict3);
                    
                if (subToken1 == NULL || strlen(subToken1) == 0)
                    upper[i] = missing;
                else
                    upper[i] = atof(subToken1);
            }
            /* create GDALT filter */
            appendMadfilter(madFiltList,
                            SINGLE_FILT, 
                            numRanges,
                            lower, 
                            upper, 
                            "GDALT",
                            "");
        } /* end z= option */
        
        else if (strncmp(mainToken, "az", 2) == 0)
        {
            /* azm filter */
            strcpy(subStr, mainToken);
            /* parse using '='  */
            subToken = (char *)strtok_r(subStr, "=", &restrict2);
            numRanges = 0;
            for (i=0; i<100; i++)
            {
                /* next token should be range (or NULL) */
                subToken = (char *)strtok_r('\0', "or", &restrict2);
                if (subToken == NULL)
                    break;
                else
                    numRanges++;
                /* if first char = , then lower limit = missing */
                if (subToken[0] == ',')
                    lower[i] = missing;
                else
                    lower[i] = atof(subToken);
                
                /* next parse this range into separate tokens using ',' */
                strcpy(subStr1, subToken);
                subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
                
                /* if first char is not ',', get next token */
                if (subStr1[0] != ',')
                    subToken1 = (char *)strtok_r('\0', ",", &restrict3);
                    
                if (subToken1 == NULL || strlen(subToken1) == 0)
                    upper[i] = missing;
                else
                    upper[i] = atof(subToken1);
            }
            /* create AZM filter */
            appendMadfilter(madFiltList,
                            SINGLE_FILT, 
                            numRanges,
                            lower, 
                            upper, 
                            "AZM",
                            "");
        } /* end az= option */
        
        else if (strncmp(mainToken, "el", 2) == 0)
        {
            /* elm filter */
            strcpy(subStr, mainToken);
            /* parse using '='  */
            subToken = (char *)strtok_r(subStr, "=", &restrict2);
            numRanges = 0;
            for (i=0; i<100; i++)
            {
                /* next token should be range (or NULL) */
                subToken = (char *)strtok_r('\0', "or", &restrict2);
                if (subToken == NULL)
                    break;
                else
                    numRanges++;
                /* if first char = , then lower limit = missing */
                if (subToken[0] == ',')
                    lower[i] = missing;
                else
                    lower[i] = atof(subToken);
                
                /* next parse this range into separate tokens using ',' */
                strcpy(subStr1, subToken);
                subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
                
                /* if first char is not ',', get next token */
                if (subStr1[0] != ',')
                    subToken1 = (char *)strtok_r('\0', ",", &restrict3);
                    
                if (subToken1 == NULL || strlen(subToken1) == 0)
                    upper[i] = missing;
                else
                    upper[i] = atof(subToken1);
            }
            /* create ELM filter */
            appendMadfilter(madFiltList,
                            SINGLE_FILT, 
                            numRanges,
                            lower, 
                            upper, 
                            "ELM",
                            "");
        } /* end el= option */
        
        else if (strncmp(mainToken, "plen", 4) == 0)
        {
            /* Pulse length filter */
            strcpy(subStr, mainToken);
            /* parse using '='  */
            subToken = (char *)strtok_r(subStr, "=", &restrict2);
            numRanges = 0;
            for (i=0; i<100; i++)
            {
                /* next token should be range (or NULL) */
                subToken = (char *)strtok_r('\0', "or", &restrict2);
                if (subToken == NULL)
                    break;
                else
                    numRanges++;
                /* if first char = , then lower limit = missing */
                if (subToken[0] == ',')
                    lower[i] = missing;
                else
                    lower[i] = atof(subToken);
                
                /* next parse this range into separate tokens using ',' */
                strcpy(subStr1, subToken);
                subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
                
                /* if first char is not ',', get next token */
                if (subStr1[0] != ',')
                    subToken1 = (char *)strtok_r('\0', ",", &restrict3);
                    
                if (subToken1 == NULL || strlen(subToken1) == 0)
                    upper[i] = missing;
                else
                    upper[i] = atof(subToken1);
            }
            /* create PL filter */
            appendMadfilter(madFiltList,
                            SINGLE_FILT, 
                            numRanges,
                            lower, 
                            upper, 
                            "PL",
                            "");
        } /* end plen= option */
        
        /* general filter option */
        else if (strncmp(mainToken, "filter", 5) == 0)
        {
            strcpy(subStr, mainToken);
            /* parse using '='  */
            subToken = (char *)strtok_r(subStr, "=", &restrict2);
            numRanges = 0;
            /* next parse using comma to get first mnem */
            subToken = (char *)strtok_r('\0', ",", &restrict2);
            strcpy(mnem1, subToken);
            strcpy(mnem2, "");
            /* check if mainToken has a second mnem */
            /* look for +, -, /, or *,              */
            if (mainToken[9+strlen(mnem1)] == ',')
            {
                if (mainToken[8+strlen(mnem1)] == '+'  ||
                    mainToken[8+strlen(mnem1)] == '-'  ||
                    mainToken[8+strlen(mnem1)] == '/'  ||
                    mainToken[8+strlen(mnem1)] == '*')
                {
                    /* second mnemonic given */
                    subToken = (char *)strtok_r('\0', ",", &restrict2);
                    if (strcmp(subToken, "+") == 0)
                        filtType = ADD_FILT;
                    else if (strcmp(subToken, "-") == 0)
                        filtType = SUB_FILT;
                    else if (strcmp(subToken, "/") == 0)
                        filtType = DIV_FILT;
                    else if (strcmp(subToken, "*") == 0)
                        filtType = MULT_FILT;
                    else
                        assert(1 == 0);  /* bug exists */
                    /* get mnemonic 2 */
                    subToken = (char *)strtok_r('\0', ",", &restrict2);
                    strcpy(mnem2, subToken);
                }
            }
            for (i=0; i<100; i++)
            {
                /* next token should be range (or NULL) */
                subToken = (char *)strtok_r('\0', "or", &restrict2);
                if (subToken == NULL)
                    break;
                else
                    numRanges++;
                /* if first char = , then lower limit = missing */
                if (subToken[0] == ',')
                    lower[i] = missing;
                else
                    lower[i] = atof(subToken);
                
                /* next parse this range into separate tokens using ',' */
                strcpy(subStr1, subToken);
                subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
                
                /* if first char is not ',', get next token */
                if (subStr1[0] != ',')
                    subToken1 = (char *)strtok_r('\0', ",", &restrict3);
                    
                if (subToken1 == NULL || strlen(subToken1) == 0)
                    upper[i] = missing;
                else
                    upper[i] = atof(subToken1);
            }
            
            /* create generic filter */
            appendMadfilter(madFiltList,
                            filtType, 
                            numRanges,
                            lower, 
                            upper, 
                            mnem1,
                            mnem2);
        } /* end generic filter option */
        
        else
        {
            /* unknown argument */
            fprintf(stderr, "Illegal argument %s\n", mainToken);
            destroyMadfilterList(madFiltList);
            return NULL;
        }
                    
        mainToken = (char *)strtok_r('\0', " ", &restrict1);
    } /* parse complete */
         
    /* add ut1 filter if needed                             */
    /* ut1 filter based on more than one argument, which is */
    /* why its added outside the parse loop                 */
    if (utLower != 0.0 || utUpper != 0.0)
    {
        /* reject any time less than one day */
        if (utLower < 3600*24.0)
            utLower = missing;
        if (utUpper < 3600*24.0)
            utUpper = missing;
        lower[0] = utLower;
        upper[0] = utUpper;
        appendMadfilter(madFiltList,
                        SINGLE_FILT, 
                        1,
                        lower, 
                        upper, 
                        "UT1",
                        "");
    
    }

    return (madFiltList);
}


/***********************************************************************
*
* createMaddata   creates a new Maddata
*
*   arguments:
*
*     char * filename - full path to the file which was basis of data 
*     char * infoStr  - Information string (may be used in outputing formatted data)
*     MadparmList *   madparmList - list of Madrigal parameters desired
*     MadfilterList * madFiltList - list of Madfilters to apply
*     FILE * errFile   - errFile to write an error messages to
*
*
*   returns - pointer to newly created Maddata.  Use 
*             destroyMaddata when done
*
*   Allocates memory to store all data, so all input may be released or changed
*   after this method is called.  Maddata is the main data structure, and is meant 
*   to be the main way to expose Madrigal data from a single cedar file that 
*   applies filtering and calculates derived data.
*
*   Returns NULL if failure.
*/
Maddata * createMaddata(char * filename,
                        char * infoStr,
                        MadparmList * requestParmList,
                        MadfilterList * madfilterList,
                        FILE * errFile)
{
    int i = 0;
    int row = 0;          /* used to loop through 2D records       */
    int rejected = 0;     /* set if present record or row rejected */
    Madrec * madrecp = NULL;
    Maddata * maddata = NULL;
    InfoDervFile * infoDervFile = NULL;
    int presRec = 0;      /* present rec num in main loop       */
    int presDataRec = 0;  /* present data rec in main loop      */
                          /*  less than presRec if header or catalog records exist in file */
    int presCyc = 0;      /* present cycle num in main loop     */
    int prevCyc = -1;     /* previous cycle num in main loop    */
    int cycId   = -1;     /* cycle id of most recently added cycle */
    int recId   = -1;     /* record id of most recently added record */
    int recType = 0;      /* present record's type in main loop */
    int status = 0;
    double first_ibyr = 0;  /* keeps track of experiment start year */
    double first_ibdt = 0;  /* keeps track of experiment start date */   
    double first_ibhm = 0;  /* keeps track of experiment start hour/min */    
    double first_ibcs = 0;  /* keeps track of experiment start sec/centisec */
    
    /* state variables */
    int newCycFound    = 0;
    int newRecFound    = 0;
    int num2DRecAdded  = 0;
    
    /* set default errFile to stderr */
    if (errFile == NULL)
    	errFile = stderr;


    /* Create a madrec object */
    madrecp = madrecCreate();
    if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
    {
        fwrite(madrecGetError(madrecp),
               sizeof(char),
               strlen(madrecGetError(madrecp)) + 1,
               errFile);
        return NULL;
     }

    /* Read the parameter code table */
    cedarReadParCodes();

    /* Connect the madrec object to a madrigal file and load rapidly into memory */
    madrecOpen(madrecp, 50, filename);
    if (strcmp(NO_ERR_STR, madrecGetError(madrecp)))
    {
        fwrite(madrecGetError(madrecp),
               sizeof(char),
               strlen(madrecGetError(madrecp)) + 1,
               errFile);
        madrecDestroy(madrecp);
        return NULL;
     }

    /* calculate plan for how to derive data */
    infoDervFile = createInfoDervFile(madrecp, requestParmList, madfilterList, errFile);
    if (infoDervFile == NULL)
    {
        madrecDestroy(madrecp);
        return NULL;
    }

    /* start to create maddata */
    if ((maddata = (Maddata *)malloc(sizeof(Maddata)))==0)
    {
        perror("malloc");
        exit(-1);
    }

    /* filename */
    if ((maddata->filename = (char *)malloc(sizeof(char) * (strlen(filename)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(maddata->filename, filename);

    /* infoStr */
    if ((maddata->infoStr = (char *)malloc(sizeof(char) * (strlen(infoStr)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(maddata->infoStr, infoStr);

    /* requestParmList */
    maddata->requestParmList = copyMadparmList(requestParmList);

    /* madFiltList */
    maddata->madFiltList = copyMadfilterList(madfilterList);

    /* numCycles */
    maddata->numCycles = 0;

    /* madCycleList */
    maddata->madCycleList = NULL;

    /* numTypes */
    maddata->numTypes = 0;

    /* madrecParmTypeList */
    maddata->madrecParmTypeList = NULL;
    
    /* now append all the types found in infoDervFile */
    for (i=0; i<infoDervFile->numTypesRec; i++)
    {
        appendMadrecParmType(maddata,
                             infoDervFile->infoDervList[i]->req1DParmList,
                             infoDervFile->infoDervList[i]->req2DParmList);
    }


    /* begin main loop through file */
    madrecRewind(madrecp);
    /* now loop through each record in in-memory file */
    presDataRec = -1;
    for(presRec = 0; presRec < madrecp->nrecords; presRec++)
    {
        status=madrecGetRecByRecno(madrecp, presRec);
        /* check for error */
        if (status)
        {
            fwrite(madrecGetError(madrecp),1, strlen(madrecGetError(madrecp)), errFile);
            return(NULL);
        }
        
        /* set state variables */
        newRecFound = 1;
        num2DRecAdded = 0;
        
        /* if this is the first record, get starting time */
        if (presRec == 0)
        {
            first_ibyr = (double)cedarGetIbyr(madrecp->recordp);
            first_ibdt = (double)cedarGetIbdt(madrecp->recordp);
            first_ibhm = (double)cedarGetIbhm(madrecp->recordp);
            first_ibcs = (double)cedarGetIbcs(madrecp->recordp);
        }
        
        /* for now, skip non-data records */
        if (!isDataRecord(madrecp->recordp))
            continue;
            
        presDataRec++;
            
        /* get this record's cycle */
        presCyc = infoDervFile->cycleNum[presDataRec];
        
        /* check if this is a new cycle */
        if (presCyc != prevCyc)
        {
            newCycFound = 1;
            prevCyc = presCyc;
        }
        
        /* In the future, cycle filtering would be applied here - not yet defined */
            
        /* get this record's type */
        recType = getRecType(infoDervFile, presRec);
        
        /* if this type of record has a filter parameter it cannot calculate, reject it immediately */
        if (!infoDervFile->infoDervList[recType]->validFilters)
            continue;
            
        /* load 1D measured data now from madrecp into infoDervFile */
        load1DMeasData(infoDervFile, 
                       madrecp, 
                       recType,
                       first_ibyr,
                       first_ibdt,
                       first_ibhm,
                       first_ibcs);
                       
        /* see if any 1D filter using only measured parameters reject this record */
        rejected = 0;
        for (i=0; i<infoDervFile->infoDervList[recType]->filt1DList->numFilters; i++)
        {
            /* see if this filter uses only measured data */
            if (infoDervFile->infoDervList[recType]->onlyMeas1DList[i])
            {
                if (!evaluateFilter(infoDervFile, recType, 1, i))
                {
                    /* record is rejected */
                    rejected = 1;
                    break;
                }
            }
        }
        if (rejected)
            continue;
            
        /* next, derive all 1D data */
        for (i=0; i<infoDervFile->infoDervList[recType]->numDervMeth; i++)
        { 
            /* check if its needed */
            if (!infoDervFile->infoDervList[recType]->infoMethodArr[i]->isNeeded)
                continue;
            /* check if its 1D */
            if (!infoDervFile->infoDervList[recType]->infoMethodArr[i]->all1D)
                continue;
                
            /* this method is needed - call it */
            dispatchMethod(infoDervFile->infoDervList[recType], i, errFile);
        }
        
        /* see if any 1D filter using both measured and derived parameters reject this record */
        rejected = 0;
        for (i=0; i<infoDervFile->infoDervList[recType]->filt1DList->numFilters; i++)
        {
            /* see if this filter uses measured and derived data */
            if (!infoDervFile->infoDervList[recType]->onlyMeas1DList[i])
            {
                if (!evaluateFilter(infoDervFile, recType, 1, i))
                {
                    /* record is rejected */
                    rejected = 1;
                    break;
                }
            }
        }
        if (rejected)
            continue;
        
        /* the 1D part of this record has been accepted */
        /* if there are no rows of 2D data, or no 2D filters, add this record */
        if (cedarGetNrow(madrecp->recordp) == 0 || 
            infoDervFile->infoDervList[recType]->filt2DList->numFilters == 0)
        {
            if (newCycFound)
            {
                /* first we need to add the new cycle */
                /* for now, I don't know how to identify cycle types */
                cycId = appendMadcycle(maddata, 0, "");
                assert(cycId != -1); /* bug found */
                newCycFound = 0;
            }
            /* add record */
            recId = appendMadrecord(maddata,
                                    infoDervFile->infoDervList[recType],
                                    cycId,
                                    recType,
                                    DATA_REC,
                                    "",
                                    cedarGetKinst(madrecp->recordp),
                                    cedarGetStartIndex(madrecp->recordp),
                                    cedarGetEndIndex(madrecp->recordp));
            newRecFound = 0;
            
            /* if any mult-row methods are needed, load 1D data now */
            if (infoDervFile->infoDervList[recType]->anyMultRowNeeded)
                load1DMultiRowData(infoDervFile, recType);
        }
        
        /* now loop through the 2D records */
        for (row=0; row<cedarGetNrow(madrecp->recordp); row++)
        {
            /* load this row's measured 2D data */
            load2DMeasData(infoDervFile, 
                           madrecp, 
                           recType,
                           row);
                    
            /* apply any 2D filters that only use measured 2D (and any 1D) data */
            rejected = 0;
            for (i=0; i<infoDervFile->infoDervList[recType]->filt2DList->numFilters; i++)
            {
                /* see if this filter uses only measured 2D (and any 1D) data */
                if (infoDervFile->infoDervList[recType]->onlyMeas2DList[i])
                {
                    if (!evaluateFilter(infoDervFile, recType, 2, i))
                    {
                        /* record is rejected */
                        rejected = 1;
                        break;
                    }
                }
            }
            if (rejected)
                continue;
                
            /* next, derive all 2D data */
            for (i=0; i<infoDervFile->infoDervList[recType]->numDervMeth; i++)
            {
                /* check if its needed */
                if (!infoDervFile->infoDervList[recType]->infoMethodArr[i]->isNeeded)
                    continue;
                /* check if its 1D (already been called) */
                if (infoDervFile->infoDervList[recType]->infoMethodArr[i]->all1D)
                    continue;
                
                /* this method is needed - call it */
                dispatchMethod(infoDervFile->infoDervList[recType], i, errFile);
            }
            
            /* see if any 2D filter using both measured and derived parameters reject this record */
            rejected = 0;
            for (i=0; i<infoDervFile->infoDervList[recType]->filt2DList->numFilters; i++)
            {
                /* see if this filter uses measured and derived data */
                if (!infoDervFile->infoDervList[recType]->onlyMeas2DList[i])
                {
                    if (!evaluateFilter(infoDervFile, recType, 2, i))
                    {
                        /* record is rejected */
                        rejected = 1;
                        break;
                    }
                }
            }
            if (!rejected)
            {
                /* this 2D record has been accepted - add it, and possibly rec and cycle */
                if (newCycFound)
                {
                    /* first we need to add the new cycle */
                    /* for now, I don't know how to identify cycle types */
                    cycId = appendMadcycle(maddata, 0, "");
                    assert(cycId != -1); /* bug found */
                    newCycFound = 0;
                }
                /* add new record if needed */
                if (newRecFound)
                {
                    recId = appendMadrecord(maddata,
                                            infoDervFile->infoDervList[recType],
                                            cycId,
                                            recType,
                                            DATA_REC,
                                            "",
                                            cedarGetKinst(madrecp->recordp),
                                            cedarGetStartIndex(madrecp->recordp),
                                            cedarGetEndIndex(madrecp->recordp));
                    newRecFound = 0;
                    
                    /* if any mult-row methods are needed, load 1D data now */
                    if (infoDervFile->infoDervList[recType]->anyMultRowNeeded)
                        load1DMultiRowData(infoDervFile, recType);
                }
                /* add new 2D row */
                append2DRow(maddata,
                            infoDervFile->infoDervList[recType],
                            cycId,
                            recType,
                            recId);
                            
                /* if any mult-row methods are needed, load 2D data now */
                if (infoDervFile->infoDervList[recType]->anyMultRowNeeded)
                    load2DMultiRowData(infoDervFile, recType, num2DRecAdded);
                    
                num2DRecAdded++;
            }
        
        } /* next 2D record */
        
        /* now that Madrecord may have been fully added, modify it with multi-row methods if needed */               
        if (num2DRecAdded > 0 && infoDervFile->infoDervList[recType]->anyMultRowNeeded)
        {
            /* loop through each multi-row method */
            for (i=0; i<infoDervFile->infoDervList[recType]->numMultiRowMeth; i++)
            {
                /* dispatch the method if needed */
                if (infoDervFile->infoDervList[recType]->infoMultiRowMethArr[i]->isNeeded)
                {
                    dispatchMultiRowMethod(infoDervFile->infoDervList[recType], i, num2DRecAdded, errFile);
                    updateMadrecordWithMultiRow(maddata,
                                                infoDervFile->infoDervList[recType]->infoMultiRowMethArr[i],
                                                i,
                                                cycId,
                                                recId,
                                                recType,
                                                num2DRecAdded);
                }
            }
        }

        
    } /* end main loop through file */

    madrecClose(madrecp);
    madrecDestroy(madrecp);
    destroyInfoDervFile(infoDervFile);

    return(maddata);
}


/***********************************************************************
*
* createNonfileMaddata   creates a new Maddata using user-supplied data
*                        rather than data from a file
*
*   arguments:
*
*     MadparmList * madparmList - list of Madrigal parameters desired
*     double ut1                - start time of integration period
*     double ut2                - end time of integration period 
*     int kinst                 - kinst id - needed since its in the prolog
*     MadparmList * oneDParms,  - list of 1D parameters for which you plan 
*                                 to provide data - may be 0 length
*     MadparmList * twoDParms,  - list of 2D parameters for which you plan 
*                                  to provide data - may be 0 length
*     int num2Drows             - number of 2D rows - may be zero
*     double * oneDdata         - array of 1D data in order of oneDParms
*     double ** twoDdata        - array of num2Drows double * to 2D data 
*                                 Each double * points to array of doubles of
*                                 length = length of twoDParms
*     FILE * errFile   - errFile to write an error messages to
*
*
*   returns - pointer to newly created Maddata.  Use 
*             destroyMaddata when done.  Will contain only one Madrecord.
*
*   Allocates memory to store all data, so all input may be released or changed
*   after this method is called.  Use this method to calculate Maddata when
*   you want to directly provide measured data, rather than get it from a file.  
*/
Maddata * createNonfileMaddata(MadparmList * requestedParms,
                               double ut1,
                               double ut2,
                               int kinst,
                               MadparmList * oneDParms,
                               MadparmList * twoDParms,
                               int num2Drows,
                               double * oneDdata,
                               double ** twoDdata,
                               FILE * errFile)
{
    int i = 0;
    int row = 0;                              /* used to loop through 2D records  */
    Maddata * maddata = NULL;
    MadparmList * new1DList = NULL;
    MadparmList * new2DList = NULL;
    MadfilterList * madfilterList = NULL;     /* an empty filter list             */
    InfoDerived * infoDerv = NULL;
    char filename[] = " - No file used";
    char infoStr[] = "";
    int cycId = 0;
    int recId = 0;
    int num2DRecAdded = 0;
    
    int first_ibyr = 0;  /* keeps track of experiment start year */
    int first_ibdt = 0;  /* keeps track of experiment start date */   
    int first_ibhm = 0;  /* keeps track of experiment start hour/min */    
    int first_ibcs = 0;  /* keeps track of experiment start sec/centisec */
    
    int ieyr = 0;  
    int iedt = 0;  
    int iehm = 0;    
    int iecs = 0;  
    
    /* set default errFile to stderr */
    if (errFile == NULL)
    	errFile = stderr;
    	
    /* create empty filter list */
    madfilterList = createMadfilterList();
    
    /* create new 1D parm list with parameters from prolog appended */
    new1DList = createMadparmList();
    /* append 1D parameters from prolog */
    /* number of prolog parameter set in #define NUM_PROLOG_PARM */
    appendMadparm(new1DList, "FIRST_IBYR");
    appendMadparm(new1DList, "FIRST_IBDT");
    appendMadparm(new1DList, "FIRST_IBHM");
    appendMadparm(new1DList, "FIRST_IBCS");
    appendMadparm(new1DList, "KINST");
    appendMadparm(new1DList, "KINDAT");
    appendMadparm(new1DList, "IBYR");
    appendMadparm(new1DList, "IBDT");
    appendMadparm(new1DList, "IBHM");
    appendMadparm(new1DList, "IBCS");
    appendMadparm(new1DList, "IEYR");
    appendMadparm(new1DList, "IEDT");
    appendMadparm(new1DList, "IEHM");
    appendMadparm(new1DList, "IECS");
    appendMadparm(new1DList, "NROW");
    appendMadparm(new1DList, "UT1");
    appendMadparm(new1DList, "UT2");
    appendMadparm(new1DList, "RECNO");
    
    /* append parms from oneDParms */
    for (i=0; i<oneDParms->numParm; i++)
        appendMadparm(new1DList, oneDParms->mnemList[i]);
        
    /* create new 2D parm list with 2D parameters from prolog appended */
    new2DList = createMadparmList();
    /* append 2D parameters from prolog */
    /* number of prolog parameter set in #define NUM_2D_PROLOG_PARM */
    appendMadparm(new2DList, "ROW");
    
    /* append parms from twoDParms */
    for (i=0; i<twoDParms->numParm; i++)
        appendMadparm(new2DList, twoDParms->mnemList[i]);

    /* calculate plan for how to derive data */
    infoDerv = createInfoDerived(new1DList, new2DList, requestedParms, madfilterList);
    if (infoDerv == NULL)
    {
        fprintf(errFile, "createInfoDerived failed!\n");
        return NULL;
    }

    /* start to create maddata */
    if ((maddata = (Maddata *)malloc(sizeof(Maddata)))==0)
    {
        perror("malloc");
        exit(-1);
    }

    /* filename */
    if ((maddata->filename = (char *)malloc(sizeof(char) * (strlen(filename)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(maddata->filename, filename);

    /* infoStr */
    if ((maddata->infoStr = (char *)malloc(sizeof(char) * (strlen(infoStr)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(maddata->infoStr, infoStr);

    /* requestParmList */
    maddata->requestParmList = copyMadparmList(requestedParms);

    /* madFiltList */
    maddata->madFiltList = copyMadfilterList(madfilterList);

    /* numCycles */
    maddata->numCycles = 0;

    /* madCycleList */
    maddata->madCycleList = NULL;

    /* numTypes */
    maddata->numTypes = 0;

    /* madrecParmTypeList */
    maddata->madrecParmTypeList = NULL;
    
    /* now append all the types found in infoDerv*/
    appendMadrecParmType(maddata,
                         infoDerv->req1DParmList,
                         infoDerv->req2DParmList);

        
    /* get starting and ending time */
    dinvmadptr(ut1, &first_ibyr, &first_ibdt, &first_ibhm, &first_ibcs);
    dinvmadptr(ut2, &ieyr, &iedt, &iehm, &iecs);
            
    /* load 1D measured data now from madrecp into infoDerv */
    
    /* set four parameters that set the time of the first record */
    infoDerv->all1DParm[0] = (double) first_ibyr;
    infoDerv->all1DParm[1] = (double) first_ibdt;
    infoDerv->all1DParm[2] = (double) first_ibhm;
    infoDerv->all1DParm[3] = (double) first_ibcs;
    
    /* set the parameters from the prolog */
    infoDerv->all1DParm[4]  = (double) kinst;
    infoDerv->all1DParm[5]  = 0.0;
    infoDerv->all1DParm[6] = (double) first_ibyr;
    infoDerv->all1DParm[7] = (double) first_ibdt;
    infoDerv->all1DParm[8] = (double) first_ibhm;
    infoDerv->all1DParm[9] = (double) first_ibcs;
    infoDerv->all1DParm[10] = (double) ieyr;
    infoDerv->all1DParm[11] = (double) iedt;
    infoDerv->all1DParm[12] = (double) iehm;
    infoDerv->all1DParm[13] = (double) iecs;
    infoDerv->all1DParm[14] = (double) num2Drows;
    infoDerv->all1DParm[15] = ut1;
    infoDerv->all1DParm[16] = ut2;
    infoDerv->all1DParm[17] = 0; /* RECNO is always 0 */
    
    /* set the rest using infoDerv->meas1DCodeList  */
    /* check for bug */
    assert(infoDerv->meas1DParmList->numParm == oneDParms->numParm + NUM_PROLOG_PARM);
    for (i=0; i<(infoDerv->meas1DParmList->numParm - NUM_PROLOG_PARM); i++)
    {
        infoDerv->all1DParm[i + NUM_PROLOG_PARM] = oneDdata[i];
    }
                       
            
    /* next, derive all 1D data */
    for (i=0; i<infoDerv->numDervMeth; i++)
    {
        /* check if its needed */
        if (!infoDerv->infoMethodArr[i]->isNeeded)
            continue;
        /* check if its 1D */
        if (!infoDerv->infoMethodArr[i]->all1D)
            continue;
                
        /* this method is needed - call it */
        dispatchMethod(infoDerv, i, errFile);
    }
        
    /* add cycle 0 */
    cycId = appendMadcycle(maddata, 0, "");
    assert(cycId == 0); /* bug found */

    /* append Madrecord */
    recId = appendMadrecord(maddata,
                            infoDerv,
                            cycId,
                            0,
                            DATA_REC,
                            "",
                            kinst,
                            ut1,
                            ut2);
            
    /* if any mult-row methods are needed, load 1D data now */
    if (infoDerv->anyMultRowNeeded)
        load1DMultiRowDataNonfile(infoDerv);
        
    /* now loop through the 2D records */
    for (row=0; row<num2Drows; row++)
    {    
        /* load this row's measured 2D data */
        infoDerv->all2DParm[0] = (double) row;
        /* check for bug */
        assert(infoDerv->meas2DParmList->numParm == twoDParms->numParm + NUM_2D_PROLOG_PARM);
        for (i=0; i<(infoDerv->meas2DParmList->numParm - NUM_2D_PROLOG_PARM); i++)
        {
            infoDerv->all2DParm[i + NUM_2D_PROLOG_PARM] = twoDdata[i][row];
        }
                    
                
        /* next, derive all 2D data */
        for (i=0; i<infoDerv->numDervMeth; i++)
        {
            /* check if its needed */
            if (!infoDerv->infoMethodArr[i]->isNeeded)
                continue;
            /* check if its 1D (already been called) */
            if (infoDerv->infoMethodArr[i]->all1D)
                continue;
                
            /* this method is needed - call it */
            dispatchMethod(infoDerv, i, errFile);
        }
            

        /* add this 2D record */
        append2DRow(maddata,
                    infoDerv,
                    0,
                    0,
                    0);
                            
        /* if any mult-row methods are needed, load 2D data now */
        if (infoDerv->anyMultRowNeeded)
            load2DMultiRowDataNonfile(infoDerv, num2DRecAdded);
                    
        num2DRecAdded++;
        
    } /* next 2D record */
        
    /* now that Madrecord may have been fully added, modify it with multi-row methods if needed */               
    if (num2DRecAdded > 0 && infoDerv->anyMultRowNeeded)
    {
        /* loop through each multi-row method */
        for (i=0; i<infoDerv->numMultiRowMeth; i++)
        {
            /* dispatch the method if needed */
            if (infoDerv->infoMultiRowMethArr[i]->isNeeded)
            {
                dispatchMultiRowMethod(infoDerv, i, num2DRecAdded, errFile);
                updateMadrecordWithMultiRow(maddata,
                                            infoDerv->infoMultiRowMethArr[i],
                                            i,
                                            0,
                                            0,
                                            0,
                                            num2DRecAdded);
            }
        }
    }

    destroyInfoDerived(infoDerv);
    destroyMadparmList(new1DList);
    destroyMadparmList(new2DList);
    destroyMadfilterList(madfilterList);

    return(maddata);
}


/***********************************************************************
*
* destroyMaddata   releases an existing Maddata
*
*   arguments: pointer to existing Maddata
*
*   returns - void
*/
void destroyMaddata(Maddata * maddata)
{
    int i = 0;
    
    if (maddata == NULL)
        return;

    if (maddata->filename != NULL)
        free(maddata->filename);

    if (maddata->infoStr != NULL)
        free(maddata->infoStr);

    if (maddata->requestParmList != NULL)
        destroyMadparmList(maddata->requestParmList);

    if (maddata->madFiltList != NULL)
        destroyMadfilterList(maddata->madFiltList);
        
    if (maddata->madCycleList != NULL)
    {
        for (i=0; i<maddata->numCycles; i++)
        {
            if (maddata->madCycleList[i] != NULL)
                destroyMadcycle(maddata->madCycleList[i]);
        }
        free(maddata->madCycleList);
    }
    
    if (maddata->madrecParmTypeList != NULL)
    {
        for (i=0; i<maddata->numTypes; i++)
        {
            destroyMadparmList(maddata->madrecParmTypeList[i].parm1DList);
            destroyMadparmList(maddata->madrecParmTypeList[i].parm2DList);
        }
        free(maddata->madrecParmTypeList);
    }
    
    free(maddata);
}


/***********************************************************************
*
* appendMadrecParmType   appends a new MadrecParmType onto maddata
*
*   arguments: 
*
*     Maddata * maddata - pointer to Maddata to append new cycle to
*     MadparmList * parm1DList - the list of 1D parameters in that type
*     MadparmList * parm1DList - the list of 2D parameters in that type
*
*
*   returns - index of new type.  Starts at 0.  If failure,
*             returns -1
*
*   Allocates memory to store all data, so all input may be released or changed
*   after this method is called.
*/
int appendMadrecParmType(Maddata * maddata,
                         MadparmList * parm1DList,
                         MadparmList * parm2DList)
{
    if (maddata == NULL)
        return -1;

    maddata->numTypes++;

    /* realloc madrecParmTypeList for new type */
    maddata->madrecParmTypeList = (MadrecParmType*)realloc(maddata->madrecParmTypeList,
                                                 sizeof(MadrecParmType)*maddata->numTypes);

    
    maddata->madrecParmTypeList[maddata->numTypes - 1].parm1DList = copyMadparmList(parm1DList);
    maddata->madrecParmTypeList[maddata->numTypes - 1].parm2DList = copyMadparmList(parm2DList);

    return(maddata->numTypes - 1);
}


/***********************************************************************
*
* appendMadcycle   appends a new Madcycle onto maddata
*
*   arguments: 
*
*     Maddata * maddata - pointer to Maddata to append new cycle to
*     int   cycleId     - cycle id (identifies cycle type)
*     char *   cycleDesc - Additional cycle description (may be empty string)
*
*
*   returns - cycle index of new cycle.  Starts at 0.  If failure,
*             returns -1
*
*   Allocates memory to store all data, so all input may be released or changed
*   after this method is called.
*/
int appendMadcycle(Maddata * maddata,
                   int cycleId,
                   char * cycleDesc)
{
    if (maddata == NULL)
        return -1;

    maddata->numCycles++;

    /* realloc madCycleList for new pointer */
    maddata->madCycleList = (Madcycle **)realloc(maddata->madCycleList,
                                                 sizeof(Madcycle *)*maddata->numCycles);

    
    maddata->madCycleList[maddata->numCycles - 1] = createMadcycle(maddata->numCycles - 1,
                                                                  cycleId,
                                                                  cycleDesc);

    return(maddata->numCycles - 1);
}


/***********************************************************************
*
* createMadcycle   creates a new Madcycle
*
*   arguments:
*
*     int   cyclenum  - cycle number 
*     int   cycleId   - cycle id (identifies cycle type)
*     char *   cycleDesc - Additional cycle description (may be empty string)
*
*
*   returns - pointer to newly created Madcycle.  Use 
*             destroyMadcycle when done
*
*   Allocates memory to store all data, so all input may be released or changed
*   after this method is called.  Use appendMadrecord to append a new Madrecord
*/
Madcycle * createMadcycle(int cyclenum,
                          int cycleId,
                          char * cycleDesc)
{
    Madcycle * madcycle = NULL;

    if ((madcycle = (Madcycle *)malloc(sizeof(Madcycle)))==0)
    {
        perror("malloc");
        exit(-1);
    }

    /* cyclenum */
    madcycle->cyclenum = cyclenum;
    /* cycleId */
    madcycle->cycleId = cycleId;

    /* cycleDesc */
    if ((madcycle->cycleDesc = (char *)malloc(sizeof(char) * (strlen(cycleDesc)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(madcycle->cycleDesc, cycleDesc);
 

    /* numMadrecords - always start off with 0 - use appendMadrecord to add new ones */
    madcycle->numMadrecords = 0;
    madcycle->madRecordList = NULL;

    return(madcycle);
}


/***********************************************************************
*
* destroyMadcycle   releases an existing Madcycle
*
*   will also free all Madrecords in this cycle
*
*   arguments: pointer to existing Madcycle
*
*   returns - void
*/
void destroyMadcycle(Madcycle * madcycle)
{
    int i = 0;

    if (madcycle == NULL)
        return;

    if (madcycle->cycleDesc != NULL)
        free(madcycle->cycleDesc);
  
    if (madcycle->numMadrecords > 0)
    {
        for (i=0; i<madcycle->numMadrecords; i++)
            destroyMadrecord(madcycle->madRecordList[i]);

        free(madcycle->madRecordList);
    }

    free(madcycle);
}


/***********************************************************************
*
* createMadrecord   creates a new Madrecord
*
*   arguments:
*
*   Rec_type  rectype - Record type: HEADER_REC, CATALOG_REC, or DATA_REC.  If DATA_REC,
*                       text will be empty string, no matter what passed in. If not, 
*                       data1Dparms will be null.
*   int numType       - index into maddata.madrecParmTypeList that defines the parm type of 
*                       record (that is, its list of 1D and 2D parameters)
*   char   *  text    - Text of header or catalog record. Empty string if data rec.
*   int num1DParms    - Number of 1D parameters to copy
*   double *  data1Dparms - pointer to array of doubles containing 1D data
*   int  kinst  - instrument id
*   double starttime - start time of record in seconds since 1/1/1950
*   double endtime - end time of record in seconds since 1/1/1950
*
*     Number of 1D parameters and order must correspond to
*     maddata.parm1DList
*
*   returns - pointer to newly created Madrecord.  Use 
*             destroyMadrecord when done
*
*   Allocates memory to store all data, so all input arrays may be released or changed
*   after this method is called.  Use createMadrecord to create a record with just the
*   1D data; then append each 2D row using append2DRow
*/
Madrecord * createMadrecord(Rec_type rectype,
                            int numType,
                            char * text,
                            int num1DParms,
                            double * data1Dparms,
                            int kinst,
                            double starttime,
                            double endtime)
{
    Madrecord * madrecord = NULL;
    int i = 0;

    if ((madrecord = (Madrecord *)malloc(sizeof(Madrecord)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    /* rectype */
    madrecord->rectype = rectype;
    
    /* rectype */
    madrecord->numType = numType;

    /* text */
    if(rectype == DATA_REC)
        text = "";
    if ((madrecord->text = (char *)malloc(sizeof(char) * (strlen(text)+1)))==0)
    {
        perror("malloc");
        exit(-1);
    }
    strcpy(madrecord->text, text);
 
    /* data1Dparms */
     if(rectype != DATA_REC)
        madrecord->data1Dparms = NULL;
    else
    {
        if ((madrecord->data1Dparms = (double *)malloc(sizeof(double) * num1DParms))==0)
        {
            perror("malloc");
            exit(-1);
        }
        /* copy doubles from input */
        for (i=0; i<num1DParms; i++)
            madrecord->data1Dparms[i] = data1Dparms[i];
    }

    /* num2Drows - always start off with 0 - use append2DRow to add each one */
    madrecord->num2Drows = 0;
    madrecord->data2Dparms = NULL;
    
    madrecord->kinst = kinst;
    madrecord->starttime = starttime;
    madrecord->endtime = endtime;

    return(madrecord);
}


/***********************************************************************
*
* destroyMadrecord   releases an existing Madrecord
*
*   arguments: pointer to existing Madrecord
*
*   returns - void
*/
void destroyMadrecord(Madrecord * madrecord)
{
    int i = 0;

    if (madrecord == NULL)
        return;

    if (madrecord->text != NULL)
        free(madrecord->text);
        
    if (madrecord->data1Dparms != NULL)
        free(madrecord->data1Dparms);
  
    if (madrecord->num2Drows > 0)
    {
        for (i=0; i<madrecord->num2Drows; i++)
            free(madrecord->data2Dparms[i]);

        free(madrecord->data2Dparms);
    }

    free(madrecord);
}


/***********************************************************************
*
* simpleMadrecordPrint - a simple method that prints all data from one Madrecord
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int cycId - cycle number of Madrecord
*         int recId - record number in cycle of Madrecord to print
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints simple version of Madrecord to FILE
*/
void simpleMadrecordPrint(Maddata * maddata,
                          int cycId,
                          int recId,
                          FILE * fp)
{
    int i = 0;
    int j=0;
    MadparmList * parm1DList;     /* List of 1D parameters for this rec type */
    MadparmList * parm2DList;     /* List of 2D parameters for this rec type */
    Madrecord * madrecord;        /* pointer to this Madrecord */
    int typeId = 0;               /* record type id */
    
    madrecord = maddata->madCycleList[cycId]->madRecordList[recId];
    
    fprintf(fp, "Printing record %i from cycle %i:\n", recId, cycId);
    
    typeId = madrecord->numType;
    
    parm1DList = maddata->madrecParmTypeList[typeId].parm1DList;
    parm2DList = maddata->madrecParmTypeList[typeId].parm2DList;
    
    
    /* print 1D parameter heading */
    fprintf(fp, "1D parameters are:\n");
    for (i=0; i<parm1DList->numParm; i++)
        fprintf(fp, "%10.10s     ", parm1DList->mnemList[i]);

    fprintf(fp, "\n");
    
    /* print 1D data */
    for (i=0; i<parm1DList->numParm; i++)
    {
        if (madrecord->data1Dparms[i] == missing)
            fprintf(fp, "   missing     ");
        else if (isErrorParm(parm1DList, i))
        {
            /* need to check error parameters for special values assumed and knownbad */
            if (madrecord->data1Dparms[i] == assumed)
                fprintf(fp, "   assumed     ");
            else if (madrecord->data1Dparms[i] == knownbad)
                fprintf(fp, "   knownbad    ");
            else
                fprintf(fp, "%12.4g   ", madrecord->data1Dparms[i]);
        }
        else  /* valid, non-error data */
            fprintf(fp, "%12.4g   ", madrecord->data1Dparms[i]);
    }
    fprintf(fp, "\n\n");
    
    /* print 2D parameter heading */
    fprintf(fp, "2D parameters are:\n");
    for (i=0; i<parm2DList->numParm; i++)
        fprintf(fp, "%10.10s     ", parm2DList->mnemList[i]);

    fprintf(fp, "\n");
    
    /* print 2D data */
    for (i=0; i<madrecord->num2Drows; i++)
    {
        for (j=0; j<parm2DList->numParm; j++)
        {
            if (madrecord->data2Dparms[i][j] == missing)
                fprintf(fp, "   missing     ");
            else
                fprintf(fp, "%12.4g   ", madrecord->data2Dparms[i][j]);
        }
        
        fprintf(fp, "\n");
    }
    fprintf(fp, "\n\n");

}


/***********************************************************************
*
* simpleMadfilterPrint - a simple method that prints all data from Maddata
*                        a single Madfilter
*
*   arguments: 
*         Madfilter * madfilter - pointer to Madfilter
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints simple version of Madfilter to FILE
*/
void simpleMadfilterPrint(Madfilter * madfilter, int filterNum, FILE * fp)
{
    int i = 0;
    int syr = 0, smd = 0, shm = 0, scs = 0;
    int eyr = 0, emd = 0, ehm = 0, ecs = 0;
    int smonth = 0, sday = 0;
    int emonth = 0, eday = 0;
    
    fprintf(fp, "Filter %i:\n", filterNum);
    switch (madfilter->filtType)
    {
        case SINGLE_FILT:
            fprintf(fp, "\t%s\n", madfilter->madParm1);
            break;
        case MULT_FILT:
            fprintf(fp, "\t%s * %s\n", madfilter->madParm1, madfilter->madParm2);
            break;
        case DIV_FILT:
            fprintf(fp, "\t%s / %s\n", madfilter->madParm1, madfilter->madParm2);
            break;
        case ADD_FILT:
            fprintf(fp, "\t%s + %s\n", madfilter->madParm1, madfilter->madParm2);
            break;
        case SUB_FILT:
            fprintf(fp, "\t%s - %s\n", madfilter->madParm1, madfilter->madParm2);
            break;
    }
    for (i=0; i<madfilter->numRange; i++)
    {
        /* show ut filter in date-time form */
        if (madfilter->filtType == SINGLE_FILT && strcmp(madfilter->madParm1, "UT1") == 0)
        {
            if (madfilter->lower[i] != missing)
            {
                dinvmadptr(madfilter->lower[i], &syr, &smd, &shm, &scs);
                smonth = smd/100;
                sday = smd - 100*smonth;
            }
            if (madfilter->upper[i] != missing)
            {
                dinvmadptr(madfilter->upper[i], &eyr, &emd, &ehm, &ecs); 
                emonth = emd/100;
                eday = emd - 100*emonth;
            }   
            if (madfilter->lower[i] != missing && madfilter->upper[i] != missing)
                fprintf(fp, "\tRange %i: Lower = %11.1f (%02i/%02i/%4i %04i:%02i), upper = %11.1f (%02i/%02i/%4i %04i:%02i)\n", 
                        i+1, madfilter->lower[i], smonth, sday, syr, shm, scs/100,
                             madfilter->upper[i], emonth, eday, eyr, ehm, ecs/100);
            else if (madfilter->lower[i] == missing && madfilter->upper[i] == missing)
                fprintf(fp, "\tRange %i: no lower limit, no upper limit\n", i+1);
            else if (madfilter->lower[i] == missing)
                fprintf(fp, "\tRange %i: no lower limit, upper = %11.1f  (%02i/%02i/%4i %04i:%02i)\n",
                        i+1, madfilter->upper[i], emonth, eday, eyr, ehm, ecs/100);
            else
                fprintf(fp, "\tRange %i: Lower = %11.1f (%02i/%02i/%4i %04i:%02i), no upper limit\n", 
                        i+1, madfilter->lower[i], smonth, sday, syr, shm, scs/100);
        }
        else
        {
            if (madfilter->lower[i] != missing && madfilter->upper[i] != missing)
                fprintf(fp, "\tRange %i: Lower = %g, upper = %g\n", i+1, madfilter->lower[i], madfilter->upper[i]);
            else if (madfilter->lower[i] == missing && madfilter->upper[i] == missing)
                fprintf(fp, "\tRange %i: no lower limit, no upper limit\n", i+1);
            else if (madfilter->lower[i] == missing)
                fprintf(fp, "\tRange %i: no lower limit, upper = %g\n", i+1, madfilter->upper[i]);
            else
                fprintf(fp, "\tRange %i: Lower = %g, no upper limit\n", i+1, madfilter->lower[i]);
        }
    }
    
    
    fprintf(fp, "\n");
}

/***********************************************************************
*
* simpleMaddataPrint - a simple method that prints all data from Maddata
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints simple version of Maddata to FILE
*/
void simpleMaddataPrint(Maddata * maddata, FILE * fp)
{
    int i=0;
    int j=0;
    
    fprintf(fp, "Printing all maddata info from file %s:\n", maddata->filename);
    
    fprintf(fp, "Filters used:\n");
    
    for (i=0; i<maddata->madFiltList->numFilters; i++)
        simpleMadfilterPrint(maddata->madFiltList->madfilt_list + i, i+1, fp);
        
    fprintf(fp, "\n");
    
    for (i=0; i<maddata->numCycles; i++)
        for (j=0; j<maddata->madCycleList[i]->numMadrecords; j++)
            simpleMadrecordPrint(maddata, i, j, fp);
}


/***********************************************************************
*
* classicIsprint - a method that prints all data in standard isprint format
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int displayHeaders - if 1, display headers, if 0, don't
*         int displaySummary - if 1, display summary at top,
*                              if 0, don't
*         int maxCharsPerLine - if 0, no limit, if < ISPRINT_MIN_CHARS_PER_LINE, 
*                               limit line to ISPRINT_MIN_CHARS_PER_LINE, else
*                               limit line to maxCharsPerLine
*         char * missingStr - string to use when data missing
*         char * assumedStr - string to use when error data assumed
*         char * knownBadStr - string to use when error data knownBad
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints Maddata in standard isprint format to FILE.  Isprint format
*    does not differentiate between 1 and 2D data; everything is treated
*    as 2D data.  Cycle ignored.  If both displayHeaders and displaySummary == 0,
*    only data will be printed without any labels.
*/
void classicIsprint(Maddata * maddata,
                    int displayHeaders,
                    int displaySummary,
                    int maxCharsPerLine,
                    char * missingStr,
                    char * assumedStr,
                    char * knownBadStr,
                    FILE * fp)
{
    int i=0;
    int j=0;
    
    /* check values of maxCharsPerLine */
    if (maxCharsPerLine < 1)
        maxCharsPerLine = 32000;  
    else if (maxCharsPerLine < ISPRINT_MIN_CHARS_PER_LINE)
        maxCharsPerLine = ISPRINT_MIN_CHARS_PER_LINE;
    
    if (displaySummary)
    {
        fprintf(fp, "Data derived from file %s:\n", maddata->filename);
        fprintf(fp, "Filters used:\n");
    
        for (i=0; i<maddata->madFiltList->numFilters; i++)
            simpleMadfilterPrint(maddata->madFiltList->madfilt_list + i, i+1, fp);
    }
        
    fprintf(fp, "\n");
    
    /* print one header if displayHeaders ==  0 but displaySummary == 1 */
    if (displayHeaders ==  0 && displaySummary == 1)
        printIsprintLabel(maddata, maxCharsPerLine, fp);
        
    /* if numCycles == 0, print message */
    if (maddata->numCycles == 0)
        fprintf(fp, "No records were selected with the filters above\n");
    
    for (i=0; i<maddata->numCycles; i++)
        for (j=0; j<maddata->madCycleList[i]->numMadrecords; j++)
            classicMadrecordPrint(maddata, 
                                  i, 
                                  j, 
                                  displayHeaders,
                                  maxCharsPerLine,
                                  missingStr,
                                  assumedStr,
                                  knownBadStr,
                                  fp);

}


/***********************************************************************
*
* printIsprintHeader - prints isprint header (time and instrument)
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int cycleNum - cycle number
*         int recNum - record in that cycle
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints isprint header line.
*/
void printIsprintHeader(Maddata * maddata, 
                        int cycleNum,
                        int recNum, 
                        FILE * fp)
{
    Madrecord * thisRecord = NULL;
    
    /* start time */
    int styear = 0, stmd = 0, sthm = 0, stcs = 0;
    int stmonth = 0, stday = 0, sthour = 0, stmin = 0, stsec = 0;
    
    /* end time */
    int endyear = 0, endmd = 0, endhm = 0, endcs = 0;
    int endhour = 0, endmin = 0, endsec = 0;
    
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[cycleNum]->madRecordList[recNum]; 
    
    dinvmadptr(thisRecord->starttime, &styear, &stmd, &sthm, &stcs);
    dinvmadptr(thisRecord->endtime, &endyear, &endmd, &endhm, &endcs);
    
    stmonth = stmd / 100;
    
    stday = stmd - 100*stmonth;
    
    sthour = sthm / 100;
    endhour = endhm / 100;
    
    stmin = sthm - 100*sthour;
    endmin = endhm - 100*endhour;
    
    stsec = stcs / 100;
    endsec = endcs / 100;
    
    fprintf(fp, "%s: %02i/%02i/%04i %04i:%02i-%04i:%02i\n",
                cedarGetStationName(thisRecord->kinst), 
                stmonth, stday, styear,
                sthm, stsec, endhm, endsec);
}


/***********************************************************************
*
* getIsprintHeader - returns malloced string containing isprint header (time and instrument)
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int cycleNum - cycle number
*         int recNum - record in that cycle
*
*   returns - char * to malloced string containing isprint header (time and instrument)
*
*    Similiar to printIsprintHeader except returns string instead of printing it.
*    User must free returned string when done with it.
*/
char * getIsprintHeader(Maddata * maddata, 
                        int cycleNum,
                        int recNum)
{
    Madrecord * thisRecord = NULL;
    char headerBuf[BIG_BUF] = "";
    char * retStr = NULL;
    
    /* start time */
    int styear = 0, stmd = 0, sthm = 0, stcs = 0;
    int stmonth = 0, stday = 0, sthour = 0, stmin = 0, stsec = 0;
    
    /* end time */
    int endyear = 0, endmd = 0, endhm = 0, endcs = 0;
    int endhour = 0, endmin = 0, endsec = 0;
    
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[cycleNum]->madRecordList[recNum]; 
    
    dinvmadptr(thisRecord->starttime, &styear, &stmd, &sthm, &stcs);
    dinvmadptr(thisRecord->endtime, &endyear, &endmd, &endhm, &endcs);
    
    stmonth = stmd / 100;
    
    stday = stmd - 100*stmonth;
    
    sthour = sthm / 100;
    endhour = endhm / 100;
    
    stmin = sthm - 100*sthour;
    endmin = endhm - 100*endhour;
    
    stsec = stcs / 100;
    endsec = endcs / 100;
    
    sprintf(headerBuf, "%s: %02i/%02i/%04i %04i:%02i-%04i:%02i",
                       cedarGetStationName(thisRecord->kinst), 
                       stmonth, stday, styear,
                       sthm, stsec, endhm, endsec);
                       
    /* now malloc retStr */
    if ((retStr = (char *)malloc(sizeof(char)*(strlen(headerBuf) + 1))) == 0)
    {
        perror("malloc");
        exit(-1);
    }
    
    strcpy(retStr, headerBuf);
    
    return(retStr);
}



/***********************************************************************
*
* printIsprintLabel - prints isprint header
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int maxCharsPerLine - limit line to maxCharsPerLine
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints isprint header line.
*/
void printIsprintLabel(Maddata * maddata, 
                        int maxCharsPerLine, 
                        FILE * fp)
{
    int charSoFar = 0;      /* characters printed so far                    */
    int linesSoFar = 0;     /* lines printed so far                         */
    int numSpaceMnem = 0;   /* number of spaces available for this mnemonic */
    char formatStr[100] = "";
    int i = 0;
    
    for (i=0; i < maddata->requestParmList->numParm; i++)
    {
        /* get format width for this mnemonic */
        numSpaceMnem = madGetParWidth (maddata->requestParmList->mnemList[i]);
        
        /* print return if end of line */
        if (charSoFar + numSpaceMnem - (linesSoFar*maxCharsPerLine) > maxCharsPerLine)
        {
            linesSoFar++;
            charSoFar = linesSoFar*maxCharsPerLine;
            fprintf(fp, "\n");
        }
        
        /* create the format string */
        sprintf(formatStr, "%%%i.%is", numSpaceMnem-1, numSpaceMnem-1);
        
        /* print the mnemonic */
        fprintf(fp, formatStr, maddata->requestParmList->mnemList[i]);
        
        /* print one space */
        fprintf(fp, " ");
        
        /* increment charSoFar */
        charSoFar = charSoFar + numSpaceMnem;
    }
    
    /* print return at end */
    fprintf(fp, "\n");
}


/***********************************************************************
*
* getIsprintLabel - creates malloced strings containing isprint label
*                   and comma-separated mnemonics
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int maxCharsPerLine - limit line to maxCharsPerLine
*         char ** mnemStr - string containing comma-separated requested mnemonics
*         char ** labelStr - string containing mnemonics labels as formatted for isprint
*
*   returns - void
*
*    User must free malloced strings mnemStr and labelStr when done with them.
*/
void getIsprintLabel(Maddata * maddata, 
                     int maxCharsPerLine,
                     char ** mnemStr,
                     char ** labelStr)
{
    int charSoFar = 0;      /* characters printed so far                    */
    int linesSoFar = 0;     /* lines printed so far                         */
    int numSpaceMnem = 0;   /* number of spaces available for this mnemonic */
    char formatStr[100] = "";
    char thisMnemStr[100] = "";
    int i = 0;
    char labelBuf[BIG_BUF] = ""; /* temporary buffer for label */
    char mnemBuf[BIG_BUF] = "";  /* temporary buffer for comma-seaparated mnemonics */
    
    for (i=0; i < maddata->requestParmList->numParm; i++)
    {
        /* first handle mnem str */
        if (i > 0)
            strcat(mnemBuf, ",");
            
        strcat(mnemBuf, maddata->requestParmList->mnemList[i]);
        
        /* next handle label */
        
        /* get format width for this mnemonic */
        numSpaceMnem = madGetParWidth (maddata->requestParmList->mnemList[i]);
        
        /* append return if end of line */
        if (charSoFar + numSpaceMnem - (linesSoFar*maxCharsPerLine) > maxCharsPerLine)
        {
            linesSoFar++;
            charSoFar = linesSoFar*maxCharsPerLine;
            strcat(labelBuf, "\n");
        }
        
        /* create the format string */
        sprintf(formatStr, "%%%i.%is", numSpaceMnem-1, numSpaceMnem-1);
        
        /* create this mnemonic string */
        sprintf(thisMnemStr, formatStr, maddata->requestParmList->mnemList[i]);
        strcat(labelBuf, thisMnemStr);
        
        /* append one space to label */
        strcat(labelBuf, " ");
        
        /* increment charSoFar */
        charSoFar = charSoFar + numSpaceMnem;
        
        /* check for buffer overflow */
        if (charSoFar > BIG_BUF)
        {
            perror("Overflow of BIG BUF in getIsprintLabel");
            exit(-1);
        }
    }
    
    /* now malloc strings */
    if ((*mnemStr = (char *)malloc(sizeof(char)*(strlen(mnemBuf) + 1))) == 0)
    {
        perror("malloc");
        exit(-1);
    }
    
    if ((*labelStr = (char *)malloc(sizeof(char)*(strlen(labelBuf) + 1))) == 0)
    {
        perror("malloc");
        exit(-1);
    }
    
    strcpy(*mnemStr, mnemBuf);
    strcpy(*labelStr, labelBuf);
}


/***********************************************************************
*
* classicMadrecordPrint - prints a single Madrecord in isprint format
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int cycleNum - cycle number
*         int recNum - record in that cycle
*         int displayHeaders - if 1, display headers, if 0, don't
*         int maxCharsPerLine - limit line to maxCharsPerLine
*         char * missingStr - string to use when data missing
*         char * assumedStr - string to use when error data assumed
*         char * knownBadStr - string to use when error data knownBad
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*    Prints isprint header line.
*/
void classicMadrecordPrint(Maddata * maddata, 
                          int cycleNum, 
                          int recNum, 
                          int displayHeaders,
                          int maxCharsPerLine,
                          char * missingStr,
                          char * assumedStr,
                          char * knownBadStr,
                          FILE * fp)
{
    int i = 0, j = 0;
    int rowNum = 0;
    int totalRows = 0;
    int numParms = 0;
    int recTypeIndex = 0;
    Madrecord * thisRecord = NULL;
    char dataBuf[50] = "";     /* holds data as string                                   */
    char formatBuf[50] = "";   /* holds temporary format string                          */
    int charSoFar = 0;         /* characters printed so far                              */
    int linesSoFar = 0;        /* lines printed so far                                   */
    char ** formatArr = NULL;  /* an array of char * to format string, len = numParm     */
    int * locationArr = NULL;  /* an array of int, len = 3*numParm, where each parm has  */
                               /* 3 ints, first is dimension (1 or 2), 2nd is index into */
                               /* data array for that parameter, 3rd indicates whether   */
                               /* its an error (1) or not (0) (needed to handle special  */
                               /* error values)                                          */
    int * widthArr = NULL;     /* an array of int to width, len = numParm                */
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[cycleNum]->madRecordList[recNum];    
    
    if (displayHeaders)
    {
        printIsprintHeader(maddata, cycleNum, recNum, fp);
        printIsprintLabel(maddata, maxCharsPerLine, fp);
    }
        
    /* if no parameters selected, return */
    numParms = maddata->requestParmList->numParm;
    if (numParms == 0)
        return;
        
    /* malloc formatArr, locationArr, and widthArr */
    formatArr   = (char **)malloc(sizeof(char *)*numParms);
    locationArr = (int   *)malloc(sizeof(int)*3*numParms);
    widthArr    = (int   *)malloc(sizeof(int)*numParms);
    if (formatArr == NULL || locationArr == NULL || widthArr == NULL)
    {
        perror("malloc");
        exit(-1);
    }
        
    /* get record type of this Madrecord */
    recTypeIndex =  thisRecord->numType;
    
    /* populate formatArr, locationArr, and widthArr */
    for (i=0; i<numParms; i++)
    {
       formatArr[i] = madGetParFormat(maddata->requestParmList->mnemList[i]);
       
       /* default format %9.1f */
       if (formatArr[i] == NULL)
           formatArr[i] = "%9.1f";
           
       widthArr[i]  = madGetParWidth(maddata->requestParmList->mnemList[i]);
       /* default is 14 */
       
       /* set up locationArr */
       if (hasParm(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                   maddata->requestParmList->mnemList[i]))
       {
           /* its a 1D parameter */
           locationArr[3*i] = 1;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       else
       {
           /* its a 2D parameter */
           locationArr[3*i] = 2;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm2DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       locationArr[3*i + 2] = isMadparmError(maddata->requestParmList->mnemList[i]);
    }
        
    /* get total 2D rows - must be at least one */
    totalRows = thisRecord->num2Drows;
    if (totalRows == 0)
        totalRows = 1;
        
    /* loop through each row */
    for (rowNum = 0; rowNum < totalRows; rowNum++)
    {
        /* reset counters */
        linesSoFar = 0;
        charSoFar  = 0;
        
        /* loop through each parameter */
        for (j=0; j < numParms; j++)
        {
            /* set up format buffer */
            sprintf(formatBuf, "%%%i.%is", widthArr[j]-1, widthArr[j]-1);
            
            /* print return if end of line */
            if (charSoFar + widthArr[j] - (linesSoFar*maxCharsPerLine) > maxCharsPerLine)
            {
                linesSoFar++;
                charSoFar = linesSoFar*maxCharsPerLine;
                fprintf(fp, "\n");
            }
        
            /* print the data */
            if (locationArr[3*j] == 1) /* 1D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data1Dparms[locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data1Dparms[locationArr[3*j + 1]]);  
                    
                fprintf(fp, formatBuf, dataBuf);
            }
            else   /* 2D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]]);  
                    
                fprintf(fp, formatBuf, dataBuf);
            }
        
            /* increment charSoFar */
            charSoFar = charSoFar + widthArr[j];
            
            /* print one space */
            fprintf(fp, " ");
        }
        
        /* print line feed */
        fprintf(fp, "\n");
    }
    
    /* free temp memory */
    free(formatArr);
    free(locationArr);
    free(widthArr);
    
    /* separate records with blank line if displayHeaders */
    if (displayHeaders)
        fprintf(fp, "\n");
}


/***********************************************************************
*
* getClassicMadrecordStrings - returns strings describing a single Madrecord 
*                              in isprint format
*
*   similar to classicMadrecordPrint except returns data as strings instead
*   of directly fprintf'ing output
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata
*         int cycleNum - cycle number
*         int recNum - record in that cycle
*         int maxCharsPerLine - limit line to maxCharsPerLine
*         char * missingStr - string to use when data missing
*         char * assumedStr - string to use when error data assumed
*         char * knownBadStr - string to use when error data knownBad
*         char ** headerStr - string containing header as formatted for isprint
*         char ** mnemStr - string containing comma-separated requested mnemonics
*         char ** labelStr - string containing mnemonics labels as formatted for isprint
*         char ** dataStr - string containing data as formatted for isprint, except
*                          rows separated by commas instead of carriage return.
*
*   The strings mnemStr, headerStr, and dataStr are allocated on the heap, and are
*   the resposiblity of the caller to free when no longer needed.
*
*   returns - void
*/
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
                                char ** dataStr)
{
    int i = 0, j = 0;
    int rowNum = 0;
    int totalRows = 0;
    int numParms = 0;
    int recTypeIndex = 0;
    Madrecord * thisRecord = NULL;
    int presentDataStrLen = 0; /* holds present length of dataStr                        */
    char dataBuf[50] = "";     /* holds data as string                                   */
    char formatBuf[50] = "";   /* holds temporary format string                          */
    char dataBuf2[50] = "";    /* holds one data item as formatted string                */
    int charSoFar = 0;         /* characters printed so far                              */
    int linesSoFar = 0;        /* lines printed so far                                   */
    char ** formatArr = NULL;  /* an array of char * to format string, len = numParm     */
    int * locationArr = NULL;  /* an array of int, len = 3*numParm, where each parm has  */
                               /* 3 ints, first is dimension (1 or 2), 2nd is index into */
                               /* data array for that parameter, 3rd indicates whether   */
                               /* its an error (1) or not (0) (needed to handle special  */
                               /* error values)                                          */
    int * widthArr = NULL;     /* an array of int to width, len = numParm                */
    char lineBuf[BIG_BUF] = "";   /* buffer to hold present line                         */
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[cycleNum]->madRecordList[recNum];   
     
    *headerStr = getIsprintHeader(maddata, 
                                  cycleNum,
                                  recNum);
                            
    getIsprintLabel(maddata, 
                    maxCharsPerLine,
                    mnemStr,
                    labelStr);
    
    /* initialize dataStr */
    *dataStr = (char *)malloc(sizeof(char)*1);
    if (*dataStr == NULL)
    {
        perror("malloc");
        exit(-1);
    }
    *dataStr[0] = '\0';
                      
    /* if no parameters selected, return */
    numParms = maddata->requestParmList->numParm;
    if (numParms == 0)
        return;
        
    /* malloc formatArr, locationArr, and widthArr */
    formatArr   = (char **)malloc(sizeof(char *)*numParms);
    locationArr = (int   *)malloc(sizeof(int)*3*numParms);
    widthArr    = (int   *)malloc(sizeof(int)*numParms);
    if (formatArr == NULL || locationArr == NULL || widthArr == NULL)
    {
        perror("malloc");
        exit(-1);
    }
        
    /* get record type of this Madrecord */
    recTypeIndex =  thisRecord->numType;
    
    /* populate formatArr, locationArr, and widthArr */
    for (i=0; i<numParms; i++)
    {
       formatArr[i] = madGetParFormat(maddata->requestParmList->mnemList[i]);
       
       /* default format %9.1f */
       if (formatArr[i] == NULL)
           formatArr[i] = "%9.1f";
           
       widthArr[i]  = madGetParWidth(maddata->requestParmList->mnemList[i]);
       /* default is 14 */
       
       /* set up locationArr */
       if (hasParm(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                   maddata->requestParmList->mnemList[i]))
       {
           /* its a 1D parameter */
           locationArr[3*i] = 1;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       else
       {
           /* its a 2D parameter */
           locationArr[3*i] = 2;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm2DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       locationArr[3*i + 2] = isMadparmError(maddata->requestParmList->mnemList[i]);
    }
        
    /* get total 2D rows - must be at least one */
    totalRows = thisRecord->num2Drows;
    if (totalRows == 0)
        totalRows = 1;
        
    /* loop through each row */
    for (rowNum = 0; rowNum < totalRows; rowNum++)
    {
        /* reset */
        linesSoFar = 0;
        charSoFar  = 0;
        strcpy(lineBuf, "");
        
        /* loop through each parameter */
        for (j=0; j < numParms; j++)
        {
            /* set up format buffer */
            sprintf(formatBuf, "%%%i.%is", widthArr[j]-1, widthArr[j]-1);
            
            /* print return if end of line */
            if (charSoFar + widthArr[j] - (linesSoFar*maxCharsPerLine) > maxCharsPerLine)
            {
                linesSoFar++;
                charSoFar = linesSoFar*maxCharsPerLine;
                strcat(lineBuf, "\n");
            }
        
            /* print the data */
            if (locationArr[3*j] == 1) /* 1D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data1Dparms[locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data1Dparms[locationArr[3*j + 1]]);  
                    
                sprintf(dataBuf2, formatBuf, dataBuf);
                strcat(lineBuf, dataBuf2);
            }
            else   /* 2D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]]);  
                    
                sprintf(dataBuf2, formatBuf, dataBuf);
                strcat(lineBuf, dataBuf2);
            }
        
            /* increment charSoFar */
            charSoFar = charSoFar + widthArr[j];
            
            /* check for temp buffer overflow */
            if (charSoFar > BIG_BUF)
            {
                perror("Overflow of BIG BUF in getClassicMadrecordStrings");
                exit(-1);
            }
        
            /* print one space if not last item in row*/
            if(j < numParms - 1)
                strcat(lineBuf, " ");
        }
        
        /* print comma to separate lines if not last row */
        if (rowNum < totalRows - 1)
            strcat(lineBuf, ",");
            
        /* realloc dataStr to hold new lineBuf and copy it in */
        presentDataStrLen = strlen(*dataStr);
        *dataStr = (char *)realloc(*dataStr, sizeof(char)*(presentDataStrLen + strlen(lineBuf) + 1));
        if (*dataStr == NULL)
        {
            perror("malloc");
            exit(-1);
        }
        
        strcat(*dataStr, lineBuf);
    }
    
    /* free temp memory */
    free(formatArr);
    free(locationArr);
    free(widthArr);
    
}

/***********************************************************************
*
* lookerMadrecordPrint - prints a single Madrecord in looker format
*
*
*   arguments: 
*         Maddata * maddata - pointer to Maddata (assumed to have one record only)
*         char * missingStr - string to use when data missing
*         char * assumedStr - string to use when error data assumed
*         char * knownBadStr - string to use when error data knownBad
*         int printHeaderFlag - if zero, surpress printing header line
*         FILE * fp - file to print to (may be stdout)
*
*   returns - void
*
*/
void lookerMadrecordPrint(Maddata * maddata, 
                          char * missingStr,
                          char * assumedStr,
                          char * knownBadStr,
			  int printHeaderFlag,
                          FILE * fp)
{
    int i = 0, j = 0;
    int rowNum = 0;
    int totalRows = 0;
    int maxCharsPerLine = 9999;
    int numParms = 0;
    int recTypeIndex = 0;
    Madrecord * thisRecord = NULL;
    char dataBuf[50] = "";     /* holds data as string                                   */
    char formatBuf[50] = "";   /* holds temporary format string                          */
    int charSoFar = 0;         /* characters printed so far                              */
    int linesSoFar = 0;        /* lines printed so far                                   */
    char ** formatArr = NULL;  /* an array of char * to format string, len = numParm     */
    int * locationArr = NULL;  /* an array of int, len = 3*numParm, where each parm has  */
                               /* 3 ints, first is dimension (1 or 2), 2nd is index into */
                               /* data array for that parameter, 3rd indicates whether   */
                               /* its an error (1) or not (0) (needed to handle special  */
                               /* error values)                                          */
    int * widthArr = NULL;     /* an array of int to width, len = numParm                */
    
    /* set thisRecord */
    thisRecord = maddata->madCycleList[0]->madRecordList[0];    
    
    if (printHeaderFlag)
    	printIsprintLabel(maddata, maxCharsPerLine, fp);
        
    /* if no parameters selected, return */
    numParms = maddata->requestParmList->numParm;
    if (numParms == 0)
        return;
        
    /* malloc formatArr, locationArr, and widthArr */
    formatArr   = (char **)malloc(sizeof(char *)*numParms);
    locationArr = (int   *)malloc(sizeof(int)*3*numParms);
    widthArr    = (int   *)malloc(sizeof(int)*numParms);
    if (formatArr == NULL || locationArr == NULL || widthArr == NULL)
    {
        perror("malloc");
        exit(-1);
    }
        
    /* get record type of this Madrecord */
    recTypeIndex =  thisRecord->numType;
    
    /* populate formatArr, locationArr, and widthArr */
    for (i=0; i<numParms; i++)
    {
       formatArr[i] = madGetParFormat(maddata->requestParmList->mnemList[i]);
       
       /* default format %9.1f */
       if (formatArr[i] == NULL)
           formatArr[i] = "%9.1f";
           
       widthArr[i]  = madGetParWidth(maddata->requestParmList->mnemList[i]);
       /* default is 14 */
       
       /* set up locationArr */
       if (hasParm(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                   maddata->requestParmList->mnemList[i]))
       {
           /* its a 1D parameter */
           locationArr[3*i] = 1;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm1DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       else
       {
           /* its a 2D parameter */
           locationArr[3*i] = 2;
           locationArr[3*i + 1] = getIndex(maddata->madrecParmTypeList[recTypeIndex].parm2DList, 
                                           maddata->requestParmList->mnemList[i]);
           assert(locationArr[3*i + 1] != -1);
       }
       locationArr[3*i + 2] = isMadparmError(maddata->requestParmList->mnemList[i]);
    }
        
    /* get total 2D rows - must be at least one */
    totalRows = thisRecord->num2Drows;
    if (totalRows == 0)
        totalRows = 1;
        
    /* loop through each row */
    for (rowNum = 0; rowNum < totalRows; rowNum++)
    {
        /* reset counters */
        linesSoFar = 0;
        charSoFar  = 0;
        
        /* loop through each parameter */
        for (j=0; j < numParms; j++)
        {
            /* set up format buffer */
            sprintf(formatBuf, "%%%i.%is", widthArr[j]-1, widthArr[j]-1);
            
            /* print return if end of line */
            if (charSoFar + widthArr[j] - (linesSoFar*maxCharsPerLine) > maxCharsPerLine)
            {
                linesSoFar++;
                charSoFar = linesSoFar*maxCharsPerLine;
                fprintf(fp, "\n");
            }
        
            /* print the data */
            if (locationArr[3*j] == 1) /* 1D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data1Dparms[locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data1Dparms[locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data1Dparms[locationArr[3*j + 1]]);  
                    
                fprintf(fp, formatBuf, dataBuf);
            }
            else   /* 2D data */
            {
                /* check for special values */
                /* missing */
                if (thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  missing)
                    sprintf(dataBuf, "%1.49s", missingStr);
                /* assumed error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  assumed)
                    sprintf(dataBuf, "%1.49s", assumedStr);
                /* knownbad error value */
                else if (locationArr[3*j + 2] && thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]] ==  knownbad)
                    sprintf(dataBuf, "%1.49s", knownBadStr);  
                /* a real value */
                else
                    sprintf(dataBuf, formatArr[j], thisRecord->data2Dparms[rowNum][locationArr[3*j + 1]]);  
                    
                fprintf(fp, formatBuf, dataBuf);
            }
        
            /* increment charSoFar */
            charSoFar = charSoFar + widthArr[j];
            
            /* print one space */
            fprintf(fp, " ");
        }
        
        /* print line feed */
        fprintf(fp, "\n");
    }
    
    /* free temp memory */
    free(formatArr);
    free(locationArr);
    free(widthArr);
    
}



/***********************************************************************
*
* populate1DDataFromStr - uses a command string to populate a MadparmList
*                         and an array of doubles
*
*   For example, if onedString = "gdalt=100.0 glon=45.0 gdlat=-20.0",
*   oneDParms would have gdalt, glon, and gdlat, and oneDdata = {100.0, 45.0, -20.0}
*
*   arguments: 
*         char * onedString - string that describes 1D data
*         MadparmList ** oneDParms - created list of 1D parameters
*         double ** oneDdata - pointer to array of doubles.  Memory malloc'ed here
*         and must be free'd by the user when done
*
*   returns - 0 if success, -1 if problem
*
*/
int populate1DDataFromStr(char * onedString, 
                          MadparmList ** oneDParms, 
                          double ** oneDdata)
{
    char * mainToken = NULL;      /* used to parse main str */
    char * restrict1 = NULL;      /* used with strtof_r, allows parsing without globals */
    char * subToken  = NULL;      /* used to parse individual tokens in main str */
    char   subStr[BIG_BUF] = "";  /* used to parse individual tokens in main str */
    char * restrict2 = NULL;      /* used with strtof_r, allows parsing without globals */
    
    *oneDdata = NULL;
    
    /* create parameter list */
    *oneDParms = createMadparmList();

    /* parse onedString using spaces */
    mainToken = (char *)strtok_r(onedString, " ", &restrict1);
    while (mainToken)
    { 
        strcpy(subStr, mainToken); 
        
        /* parse each argument using = */
        subToken = (char *)strtok_r(subStr, "=", &restrict2);
        /* try to append first parameter as a Madrigal parameter */
        if (appendMadparm(*oneDParms, subToken) != 0)
        {
            destroyMadparmList(*oneDParms);
            if (*oneDdata != NULL)
                free(*oneDdata);
            return (-1);
        }
        /* get 1D value next */
        subToken = (char *)strtok_r('\0', "=", &restrict2);
        
        if (subToken == NULL)
        {
            destroyMadparmList(*oneDParms);
            if (*oneDdata != NULL)
                free(*oneDdata);
            return (-1);
        }
        /* realloc *oneDdata */
        if ((*oneDdata = (double *)realloc(*oneDdata, sizeof(double)*((*oneDParms)->numParm))) == NULL)
        {
            perror("malloc");
            exit (-1);
        }
        /* set value */
        (*oneDdata)[(*oneDParms)->numParm - 1] = atof(subToken);
        
        mainToken = (char *)strtok_r('\0', " ", &restrict1);
    }   
    
    /* no problems found */  
    return (0);
}




/***********************************************************************
*
* populate2DDataFromStr - uses a command string to populate a MadparmList
*                         and an array of doubles
*
*   For example, if twodString is:
*
*      "gdlat=45,45,45,45,50,50,50,50 glon=20,20,30,30,20,20,30,30 gdalt=500,600,500,600,500,600,500,600"
*
*   (Note that each parameters must have same number of values or an error is thrown)
*
*   twoDParms would have gdalt, glon, and gdlat, and 
*   twoDdata = { {45.0,45.0,45.0,45.0,50.0,50.0,50.0,50.0},
*                {20.0,20.0,30.0,30.0,20.0,20.0,30.0,30.0},
*                {500.0,600.0,500.0,600.0,500.0,600.0,500.0,600.0}}
*
*   arguments: 
*         char * twodString - string that describes 2D data
*         MadparmList ** twoDParms - created list of 2D parameters
*         double *** twoDdata - pointer to array of arrays of doubles.  Memory malloc'ed here
*         and must be free'd by the user when done
*         int * num2Drows - number of rows found in each 2D parameter
*
*   returns - number of 2D values per parameter if success, -1 if problem
*
*/
int populate2DDataFromStr(char * twodString, 
                          MadparmList ** twoDParms, 
                          double *** twoDdata, 
                          int * num2Drows)
{
    char * mainToken = NULL;      /* used to parse main str */
    char * restrict1 = NULL;      /* used with strtof_r, allows parsing without globals */
    char * subToken  = NULL;      /* used to parse individual tokens in main str */
    char   subStr[BIG_BUF] = "";  /* used to parse individual tokens in main str */
    char * restrict2 = NULL;      /* used with strtof_r, allows parsing without globals */
    char   subStr1[BIG_BUF] = ""; /* used to parse sub tokens in token */
    char * subToken1  = NULL;     /* used to parse sub tokens in token */
    char * restrict3 = NULL;      /* used with strtof_r, allows parsing without globals */
    int presNum2Drows = 0, i=0;
    
    *twoDdata = NULL;
    *num2Drows = 0;
    
    /* create parameter list */
    *twoDParms = createMadparmList();

    /* parse twodString using spaces */
    mainToken = (char *)strtok_r(twodString, " ", &restrict1);
    while (mainToken)
    { 
        strcpy(subStr, mainToken); 
        /* parse each argument using = */
        subToken = (char *)strtok_r(subStr, "=", &restrict2);
        /* try to append first parameter as a Madrigal parameter */
        if (appendMadparm(*twoDParms, subToken) != 0)
        {
            if (*twoDdata != NULL)
            {
                for (i=0; i<(*twoDParms)->numParm; i++)
                    free(*twoDdata[i]);
                free(*twoDdata);
            }
            destroyMadparmList(*twoDParms);
            return (-1);
        }
        
        /* malloc next pointer */
        if ((*twoDdata = (double **)realloc(*twoDdata, sizeof(double *)*(*twoDParms)->numParm)) == NULL)
        {
            perror("malloc");
            exit (-1);
        }
        
        /* get 2D values next */
        subToken = (char *)strtok_r('\0', "=", &restrict2);
        if (subToken == NULL)
        {
            if (*twoDdata != NULL)
            {
                for (i=0; i<(*twoDParms)->numParm; i++)
                    free(*twoDdata[i]);
                free(*twoDdata);
            }
            destroyMadparmList(*twoDParms);
            return (-1);
        }
        
        /* now parse each value separated by commas */
        strcpy(subStr1, subToken);
        subToken1 = (char *)strtok_r(subStr1, ",", &restrict3);
        presNum2Drows = 0;
        while (subToken1)
        {
            presNum2Drows++;
            
            /* use malloc if first row */
            if (presNum2Drows == 1)
            {
                /* malloc *twoDdata[(*twoDParms)->numParm - 1] */
                if (((*twoDdata)[((*twoDParms)->numParm) - 1] = (double *)malloc(sizeof(double)*presNum2Drows)) == NULL)
                {
                    perror("malloc");
                    exit (-1);
                }
            
            }
            else
            {
            
                /* realloc *twoDdata[(*twoDParms)->numParm - 1] */
                if (((*twoDdata)[(*twoDParms)->numParm - 1] = (double *)realloc((*twoDdata)[(*twoDParms)->numParm - 1], 
                                                                      sizeof(double)*presNum2Drows)) == NULL)
                {
                    perror("malloc");
                    exit (-1);
                }
            }
            /* set value */
            (*twoDdata)[(*twoDParms)->numParm - 1][presNum2Drows - 1] = atof(subToken1);
        
            subToken1 = (char *)strtok_r('\0', ",", &restrict3);
        }
        
        /* check if this was first parameter */
        if (*num2Drows == 0)
            *num2Drows = presNum2Drows;
        else if (*num2Drows != presNum2Drows)
        {
            /* not all parameters have the same number of values */
            if (*twoDdata != NULL)
            {
                for (i=0; i<(*twoDParms)->numParm; i++)
                    free(*twoDdata[i]);
                free(*twoDdata);
            }
            destroyMadparmList(*twoDParms);
            return (-1);
        }
        
        /* get next parameter */
        mainToken = (char *)strtok_r('\0', " ", &restrict1);
    }   
    
    /* no problems found */
    return (0);

}




